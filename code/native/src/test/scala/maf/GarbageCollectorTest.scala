package maf.test

import maf.core.{Identifier, Monad}
import maf.gc.NativeGC
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.{DependencyTracking, ModAnalysis, ReturnAddr}
import maf.modular.scheme.modf.{SchemeModFCallSiteSensitivity, SchemeModFComponent, SchemeModFNoSensitivity, SimpleSchemeModFAnalysis}
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.util.{Reader, StoreUtil}
import maf.util.benchmarks.{Timeout, Timer}
import maf.modular.scheme.SchemeConstantPropagationDomain
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*
import scala.scalanative.unsafe.{CChar, CDouble, CInt, CString, fromCString}

// null values are used here due to Java interop
import scala.language.unsafeNulls
import maf.modular.scheme.{NativeDomainWSStrings, NativeSchemeDomain}
import maf.core.Address
import maf.lattice._


class GarbageCollectorTest extends AnyFlatSpec with should.Matchers:

    val testFiles: List[String] = List(
       // "test/R5RS/gambit/earley.scm"
        //"test/R5RS/gambit/nboyer.scm"
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
        "test/R5RS/icp/icp_8_compiler.scm"
        )

    var cpStrings: Map[Address, ConstantPropagation.S] = Map.empty
    var cpSyms: Map[Address, ConstantPropagation.Sym] = Map.empty
    var cpInts: Map[Address, ConstantPropagation.I] = Map.empty
    var cpReals: Map[Address, ConstantPropagation.R] = Map.empty
    var cpChars: Map[Address, ConstantPropagation.C] = Map.empty

    var nativeStrings: Map[Address, String] = Map.empty
    var nativeSyms: Map[Address, String] = Map.empty
    var nativeInts: Map[Address, CInt] = Map.empty
    var nativeReals: Map[Address, CDouble] = Map.empty
    var nativeChars: Map[Address, CChar] = Map.empty


    def newCPAnalysis(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with SchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp] {
            override def run(timeout: Timeout.T): Unit =
                super.run(timeout)
                store.foreach(
                    (addr, value) =>
                        value.contents.foreach(
                            (key, el) =>
                                el match
                                    case str: modularLattice.Str =>
                                        cpStrings +=   (addr.asInstanceOf[Address], str.s.asInstanceOf[ConstantPropagation.S])
                                    case sym: modularLattice.Symbol =>
                                       cpSyms += (addr.asInstanceOf[Address], sym.s.asInstanceOf[ConstantPropagation.Sym])
                                    case int: modularLattice.Int =>
                                        cpInts += (addr.asInstanceOf[Address], int.i.asInstanceOf[ConstantPropagation.I])
                                    case real: modularLattice.Real =>
                                        cpReals += (addr.asInstanceOf[Address], real.r.asInstanceOf[ConstantPropagation.R])
                                    case char: modularLattice.Char =>
                                        cpChars += (addr.asInstanceOf[Address], char.c.asInstanceOf[ConstantPropagation.C])
                                    case _ => /* None */))

            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra
        }


    def newNativeAnalysisWithGC(program: SchemeExp) =
        new SimpleSchemeModFAnalysis(program)
            with NativeGC[SchemeExp]
            with SchemeModFNoSensitivity
            with NativeSchemeDomain
            with FIFOWorklistAlgorithm[SchemeExp] {

            override def emptyMemory(): Unit =
                store.foreach((addr, v) =>
                    v.contents.foreach(
                    (key, el) =>
                        
                        el match
                            case str: modularLattice.Str =>
                                nativeStrings += (addr.asInstanceOf[Address], fromCString(str.s.asInstanceOf[NativeString].underlying._2))
                            case str: modularLattice.Symbol =>
                                nativeSyms += (addr.asInstanceOf[Address], fromCString(str.s.asInstanceOf[NativeString].underlying._2))
                            case int: modularLattice.Int =>
                                nativeInts += (addr.asInstanceOf[Address], int.i.asInstanceOf[NativeLattice.I])
                            case real: modularLattice.Real =>
                                nativeReals += (addr.asInstanceOf[Address], real.r.asInstanceOf[NativeLattice.R])
                            case char: modularLattice.Char =>
                                nativeChars += (addr.asInstanceOf[Address], char.c.asInstanceOf[NativeLattice.C])
                            case _ => /* None */))
                super.emptyMemory()

            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with NativeIntraGC
        }

    "Native Analysis with Garbage Collection" should "return correct and sound results" in {
        testFiles.foreach(file =>

            val text = SchemeParser.parseProgram(Reader.loadFile(file))
            val cp = newCPAnalysis(text)
            val sn = newNativeAnalysisWithGC(text)

            println(s"allocList: ${NativeString.allocList}")


            cp.analyzeWithTimeout(Timeout.start(Duration(1, MINUTES)))
            sn.analyzeWithTimeout(Timeout.start(Duration(1, MINUTES)))

            sn.emptyMemory()

            //println(s"freectr = ${NativeString.free_ctr}, mallocctr = ${NativeString.malloc_ctr}")

            //assert(NativeString.allocatedStrings.isEmpty)


            cpStrings.foreach {
                case (addr, ConstantPropagation.Constant(x: String)) => assert(nativeStrings.exists((naddr, nx) => naddr == addr && nx == x))
                case _ => /* None */
            }
            cpChars.foreach {
                case (addr, ConstantPropagation.Constant(x: Char)) => assert(nativeChars.exists((naddr, nx) => naddr == addr && nx.toByte == x))
                case _ => /* None */
            }

            cpInts.foreach {
                case (addr, ConstantPropagation.Constant(x: BigInt)) => assert(nativeInts.exists((naddr, nx) => naddr == addr && nx == x.toInt))
                case _ => /* None */
            }


            cpSyms.foreach {
                case (addr, ConstantPropagation.Constant(x: String)) => assert(nativeSyms.exists((naddr, nx) => naddr == addr && nx == x), s"($addr, $x)")
                case _ => /* None */
             }



            cpReals.foreach {
                case (addr, ConstantPropagation.Constant(x: Double)) => assert(nativeReals.exists((naddr, nx) => naddr == addr && x == nx))
                case _ => /* None */
            }

            cpStrings = Map.empty
            cpChars = Map.empty
            cpInts = Map.empty
            cpSyms = Map.empty
            cpReals = Map.empty

            nativeStrings = Map.empty
            nativeChars = Map.empty
            nativeInts = Map.empty
            nativeSyms = Map.empty
            nativeReals = Map.empty

        )

        NativeString.freeBounds()



    }



