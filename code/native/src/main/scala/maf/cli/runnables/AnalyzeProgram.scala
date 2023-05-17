package maf.cli.runnables

import maf.core.{Identifier, Monad}
import maf.gc.NativeGC
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.scheme.NativeSchemeDomain.modularLattice
import maf.modular.{DependencyTracking, ModAnalysis}
import maf.modular.scheme.modf.{SchemeModFCallSiteSensitivity, SchemeModFComponent, SchemeModFNoSensitivity, SimpleSchemeModFAnalysis}
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.util.{Reader, StoreUtil}
import maf.util.benchmarks.{Timeout, Timer}
import maf.modular.scheme.SchemeConstantPropagationDomain
import scalanative.unsafe._
import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls
import maf.modular.scheme.{NativeDomainWSStrings, NativeSchemeDomain}
import maf.core.Address
import maf.lattice._
import maf.modular.AddrDependency
import maf.lattice.NativeString.allocatedStrings



object AnalyzeProgram:
    def runAnalysis[A <: ModAnalysis[SchemeExp]](bench: String, analysis: SchemeExp => A, timeout: () => Timeout.T): A =
        val text = SchemeParser.parseProgram(Reader.loadFile(bench))
        val a = analysis(text)
        print(s"Analysis of $bench ")
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
        a


    /**
      * Runs a program given the string containing the program
      */
    def runProgram[A <: ModAnalysis[SchemeExp]](program: String, analysis: SchemeExp => A, timeout: () => Timeout.T): A =
        val text = SchemeParser.parseProgram(program)
        val a = analysis(text)
      //  print(s"Analysis of $bench ")
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
        a
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



    def newNativeAnalysisWoGC(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with NativeSchemeDomain
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra

            override def run(timeout: Timeout.T): Unit =
                super.run(timeout)
                NativeString.deallocateAllStrings()
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
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def updateAddr(store: Map[Addr, modularLattice.L], addr: Addr, value: modularLatticeWrapper.modularLattice.L): Option[Map[Addr, modularLattice.L]] =
              //  println(value.contents)
                super.updateAddr(store, addr, value)


            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra

                
        }


    def newNativeAnalysisWithGC(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with NativeGC[SchemeExp]
            with SchemeModFNoSensitivity
            with NativeSchemeDomain
            with FIFOWorklistAlgorithm[SchemeExp] {

            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with NativeIntraGC:
                    override def commit(): Unit =
                        println(component)
                        println(W)
                        println(R)
                        println(C)

                        super.commit()
                        println()


        }

    def main(args: Array[String]): Unit =
        val a = runAnalysis(
            "test/test.rkt", program => newNativeAnalysisWithGC(program), () => Timeout.start(Duration(5, MINUTES))
        )
        val b = runAnalysis(
            "test/test.rkt", program => newCPAnalysis(program), () => Timeout.start(Duration(5, MINUTES))
        )
        println(a.storeString(false))
        println(b.storeString(false)) 
        a.emptyMemory()
        NativeString.freeBounds()