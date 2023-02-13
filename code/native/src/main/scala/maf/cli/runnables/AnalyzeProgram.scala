package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.{Identifier, Monad}
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.{DependencyTracking, ModAnalysis}
import maf.modular.scheme.modf.{SimpleSchemeModFAnalysis, SchemeModFNoSensitivity, SchemeModFComponent}

import maf.modular.worklist.{FIFOWorklistAlgorithm}
import maf.util.Reader

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls
import maf.modular.scheme.SchemeConstantPropagationDomain



object AnalyzeProgram extends App:
    def runAnalysis[A <: ModAnalysis[SchemeExp]](text: SchemeExp, analysis: SchemeExp => A) :A = //, timeout: () => Timeout.T): A =

        val a = analysis(text)
        a.analyze()
        a
/*         print(s"Analysis started ")
        try {
            val time = Timer.timeOnly {
                a.analyzeWithTimeout(timeout())
                //println(a.program.prettyString())
            }
            println(s"terminated in ${time / 1000000} ms.")
            //a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) }).foreach(println)
        } catch {
            case t: Throwable =>
                println(s"raised exception.")
                System.err.println(t.getMessage)
                t.printStackTrace()
                System.err.flush()
        }
        a */

    val bench: List[String] = List(
        //  "test/taint/tainted-function-select.scm",
        "test/R5RS/icp/icp_1c_ambeval.scm",
        "test/R5RS/icp/icp_5_regsim.scm",
        "test/R5RS/icp/icp_7_eceval.scm",
        "test/R5RS/icp/icp_1c_multiple-dwelling.scm",
        "test/R5RS/icp/icp_1c_ontleed.scm",
        "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
        "test/R5RS/icp/icp_2_aeval.scm",
        "test/R5RS/icp/icp_3_leval.scm",
        "test/R5RS/icp/icp_6_stopandcopy_scheme.scm",
        "test/R5RS/icp/icp_8_compiler.scm")

    val parsedPrograms: List[SchemeExp] = bench.map((s: String) => SchemeParser.parseProgram(Reader.loadFile(s)))

    // Used by webviz.
    def newStandardAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with SchemeConstantPropagationDomain
            with DependencyTracking[SchemeExp]
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    def triggerJprofiler: Unit =
        println("Triggering the Jprofiler to start after the JVM warmupi")
    @main
    def run() =
        parsedPrograms.foreach({ b =>
            //runAnalysis(b, program => SchemeAnalyses.kCFAAnalysis(program, 0), () => Timeout.start(Duration(2, MINUTES)))
            for (i <- 0 to 15) {
                if(i > 10) then triggerJprofiler
                val a = runAnalysis(b, program => newStandardAnalysis(program)) //, () => Timeout.start(Duration(1, MINUTES)))
                println(b)
                println()
            }

        })