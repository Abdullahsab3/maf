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
        print(s"Analysis of $bench ")
        try {
            val time = Timer.timeOnly {
                a.analyzeWithTimeout(timeout())
            }.toDouble
            t = time / 1000000
            println(s"terminated in ${time / 1000000} ms.")

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
    /**
     * USAGE:
     * First arg: number of iterations
     * Second arg: number of warmup iterations
     * rest of args (optionally) paths to folders to be benchmarked
     */
        println("Benchmarking using Scala Native. version built on 25-04 16:00")
        if (args.length < 2) then
            println("Pleas specify how many iterations, how many of these iterations are warmup rounds, and optionally which folders to benchmark")
        else
            val rounds = args(0).toInt
            val warmup = args(1).toInt
            val testFiles: ListBuffer[String] = ListBuffer()
            if (args.length > 2) then
                val folders = args.slice(2, args.length)
                testFiles ++= folders
            else
                testFiles += "test/R5RS/icp"
            val bench: List[String] = SchemeBenchmarkPrograms.fromFolders(testFiles.toList)
            val parsedPrograms: List[SchemeExp] = bench.map((s: String) => SchemeParser.parseProgram(Reader.loadFile(s)))
            val measurements: Map[String, Map[String, Measurement]] =
                analyses.map((analysisName, f) =>
                    (analysisName -> bench.map((filename) => (filename -> Measurement(warmup, analysisName, filename))).toMap)
                )

            analyses.foreach((name, analysis) =>
                println(s"benchmarking of: $name")
                var i = 0
                while (i < rounds) do
                    var j = 0
                    while (j < bench.length) do
                        val t = runAnalysis(bench(j), parsedPrograms(j), analysis, () => Timeout.start(Duration(1, MINUTES)))
                        measurements(name)(bench(j)).addMeasurement(t)
                        j = j + 1
                    i = i + 1
                println())

            measurements.foreach((s, m) => m.foreach((f, measurement) =>
                measurement.calculate()
                println(measurement.toString())))

