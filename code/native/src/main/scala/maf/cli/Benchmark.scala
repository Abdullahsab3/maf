package maf.cli.runnables

import maf.bench.Measurement
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.{Identifier, Monad}
import maf.gc.NativeGC
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.{DependencyTracking, ModAnalysis}
import maf.modular.scheme.modf.{SchemeModFComponent, SchemeModFNoSensitivity, SimpleSchemeModFAnalysis}
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.util.Reader
import maf.util.benchmarks.{Clock, Timeout, Timer}
import maf.lattice.NativeString

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls
import maf.modular.scheme.{NativeDomainWSStrings, NativeSchemeDomain}
import maf.lattice.NativeLattice
import maf.modular.scheme.SchemeConstantPropagationDomain
import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File

object  Benchmark:

    def runAnalysis[A <: ModAnalysis[SchemeExp]](bench: String, text: SchemeExp, analysis: SchemeExp => A, timeout: () => Timeout.T): Double =
        var t = -1.0
        val a = analysis(text)
        try {
            val time = Timer.timeOnly {
                a.analyzeWithTimeout(timeout())
            }.toDouble
            t = time / 1000000

        } catch {
            case t: Throwable =>
                println(s"raised exception.")
                System.err.println(t.getMessage)
                t.printStackTrace()
                System.err.flush()
        }
        t

    def newNativeAnalysisWithGC(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with NativeGC[SchemeExp]
            with SchemeModFNoSensitivity
            with NativeSchemeDomain
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def run(timeout: Timeout.T): Unit =
                super.run(timeout)
                emptyMemory()
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with NativeIntraGC
        }

    def newNativeAnalysisWoGC(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with NativeSchemeDomain
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra

            override def run(timeout: Timeout.T): Unit =
                super.run(timeout)
        }

    def newNativeAnalysisWScalaStrings(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with NativeDomainWSStrings
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra
        }


    def newCPAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with SchemeConstantPropagationDomain
            with DependencyTracking[SchemeExp]
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    val analyses: Map[String, SchemeExp => ModAnalysis[SchemeExp]] =
        Map("NativeWGC" -> newNativeAnalysisWithGC,
            "NativeWoGC" -> newNativeAnalysisWoGC,
            "NativewScalaString" -> newNativeAnalysisWScalaStrings,
            "CP" -> newCPAnalysis)


    def main(args: Array[String]): Unit =

        if (args.length < 3) then
            println("Pleas specify the strategy, how many iterations, how many of these iterations are warmup rounds, and optionally which folders to benchmark")
            println(s"strategies: ${analyses.keySet}")
        else
            val progStartingTime = System.nanoTime()
            val strategy = args(0)
            val rounds = args(1).toInt
            val warmup = args(2).toInt
            val testFiles: ListBuffer[String] = ListBuffer()
            if (args.length > 3) then
                val folders = args.slice(3, args.length)
                testFiles ++= folders
            else
            testFiles += "test/R5RS/icp"
            println(s"Analysing $strategy with $rounds rounds and $warmup warmup rounds using $testFiles")
            val bench: List[String] = SchemeBenchmarkPrograms.fromFolders(testFiles.toList)
            val parsedPrograms: List[SchemeExp] = bench.map((s: String) => SchemeParser.parseProgram(Reader.loadFile(s)))
            var measurement : Option[Measurement] = None
            
            println("file,mean,CI,min,median,max")
            var analysis = analyses(strategy)
            var i = 0
            while (i < bench.length) do
                val filename = bench(i)
                measurement = Some(Measurement(20, warmup, strategy, filename))
                var j = 0
                while (j < rounds) do
                    val t = runAnalysis(filename, parsedPrograms(i), analysis, () => Timeout.start(Duration(1, MINUTES)))
                    measurement.get.addMeasurement(t)
                    j = j + 1
                measurement.get.calculate()
                println(measurement.get.toString())
                i = i + 1
                
            println()
            println(s"There are still ${NativeString.allocatedStrings.size} allocated strings left")
            val startingTime = System.nanoTime
            NativeString.deallocateAllStrings()
            val totalTime = System.nanoTime() - startingTime
            NativeString.freeBounds()
            println(s"Deallocating the strings took $totalTime ms")
            val finishTime = System.nanoTime - progStartingTime
            println(s"Executing the benchmarks took $finishTime ns")
