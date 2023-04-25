package maf.cli.runnables

import maf.core.{Identifier, Monad}
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.{DependencyTracking, ModAnalysis}
import maf.modular.scheme.modf.{SimpleSchemeModFAnalysis, SchemeModFNoSensitivity, SchemeModFComponent}

import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls
import maf.modular.scheme.SchemeConstantPropagationDomain
import scala.collection.mutable.ListBuffer



object AnalyzeProgram extends App:    
    val bench: List[String] = SchemeBenchmarkPrograms.fromFolder("test/R5RS/ad")().toList
    val parsedPrograms: List[SchemeExp] = bench.map((s: String) => SchemeParser.parseProgram(Reader.loadFile(s)))

    val results: ListBuffer[Long] = ListBuffer.empty ++ bench.map(_ => 0)

    var warmupskipped = true

    def runAnalysis[A <: ModAnalysis[SchemeExp]](index: Int, analysis: SchemeExp => A, timeout: () => Timeout.T): A =
        val text = parsedPrograms(index)
        val pr = bench(index)
        val a = analysis(text)
        print(s"Analysis of $pr ")
        try {
            val time = Timer.timeOnly {
                a.analyzeWithTimeout(timeout())
            }
            results(index) = results(index) + time
            println(s"terminated in ${time / 1000000} ms.")
        } catch {
            case t: Throwable =>
                println(s"raised exception.")
                System.err.println(t.getMessage)
                t.printStackTrace()
                System.err.flush()
        }
        a

    def newStandardAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with SchemeConstantPropagationDomain
            with DependencyTracking[SchemeExp]
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    var i = 0
    while(i < 50) do
        //if i == 10 then warmupskipped = true
        for(j <- 0 until bench.length) do
            runAnalysis(j, program => newStandardAnalysis(program), () => Timeout.start(Duration(1, MINUTES)))
        i = i + 1
    
    println("------------- RESULTS ---------------")
    
    for((name, result) <- bench.zip(results)) do 
        println(s"$name     total: ${result / 1000000000} s")