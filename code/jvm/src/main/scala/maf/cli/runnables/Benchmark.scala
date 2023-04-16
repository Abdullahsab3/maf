package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.{Identifier, Monad}
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.{DependencyTracking, ModAnalysis}
import maf.modular.scheme.modf.{SimpleSchemeModFAnalysis, SchemeModFNoSensitivity, SchemeModFComponent}

import maf.modular.worklist.{FIFOWorklistAlgorithm}
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer, Clock}

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls
import maf.modular.scheme.SchemeConstantPropagationDomain
import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File

object  Benchmark extends App:

    var testFiles = "test/R5RS/icp"
    
    var bench: List[String] = SchemeBenchmarkPrograms.fromFolder(testFiles)().toList
    var parsedPrograms: List[SchemeExp] = bench.map((s: String) => SchemeParser.parseProgram(Reader.loadFile(s)))

    var results: ListBuffer[Long] = ListBuffer.empty ++ bench.map(_ => 0)

    def updateBench =
        bench = SchemeBenchmarkPrograms.fromFolder(testFiles)().toList
        parsedPrograms = bench.map((s: String) => SchemeParser.parseProgram(Reader.loadFile(s)))
        results = ListBuffer.empty ++ bench.map(_ => 0)

    var warmupskipped = false

    /**
      * USAGE: 
        First argument:
            -wut: include the warmup time in the results
            -nwut(default): warmup time excluded 
        Second argument:
            path to folder including test files
            no path provided: icp 

      */

    var rounds = 50
    def warmupRounds: Int = (0.10 * rounds).toInt
    if(args.length > 1) then
        val warmupFlag =  args(0)
        if(warmupFlag == "-wut") then warmupskipped = true
        else warmupskipped = false
        if(args.length > 2) then
            testFiles = args(1)
            updateBench
        if(args.length > 3) then
            rounds = args(2).toInt



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
    while(i < rounds) do
        if (i == warmupRounds && warmupskipped) then warmupskipped = true
        var j = 0
        while(j < bench.length) do
            runAnalysis(j ,program => newStandardAnalysis(program), () => Timeout.start(Duration(1, MINUTES)))
            j = j + 1
        i = i + 1
    
    println("------------- RESULTS ---------------")
    
    val writer = PrintWriter(File(s"results${Clock.nowStr()}.txt"))
    writer.write(s"JVMCPDomain\n")
    for((name, result) <- bench.zip(results)) do 
        writer.write(s"$name    |   total: ${result / 1000000000} s\n")
        

