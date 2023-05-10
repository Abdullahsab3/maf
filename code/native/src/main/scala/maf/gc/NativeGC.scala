package maf.gc

import maf.core.Expression
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.NativeString
import maf.modular.scheme.NativeSchemeDomain
import maf.modular.scheme.NativeSchemeDomain.modularLattice
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.modular.{AddrDependency, Dependency, GlobalStore, ModAnalysis}
import maf.util.StoreUtil
import maf.util.benchmarks.Timeout

import scala.collection.mutable.ListBuffer
import scala.scalanative.unsafe.fromCString

trait NativeGC[Expr <: Expression] extends ModAnalysis[Expr] with GC[Expr] with NativeSchemeDomain with GlobalStore[Expr] { inter =>

    def initializeMemory(): Unit =
        NativeString.initializeMemory()


    def emptyMemory(): Unit =
        NativeString.deallocateAllStrings()

    override def emptyAnalysisMemory(): Unit =
        store.foreach(
            (a, v) =>
                v.contents.foreach(
                    (mk, mv) =>
                        mv match
                            case str: modularLattice.Str =>
                                str.s.asInstanceOf[NativeString].unmark()
                            case str: modularLattice.Symbol =>
                                str.s.asInstanceOf[NativeString].unmark()
                            case _ => /* None */
                )
        )


    def markValues(value: Value): Unit =
        value.contents.foreach(
            (k, v) =>
                v match
                    case str: modularLattice.Str =>
                        str.s.asInstanceOf[NativeString].mark()
                    case str: modularLattice.Symbol =>
                        str.s.asInstanceOf[NativeString].mark()
                    case _ => /* None */
        )

    def unmarkValues(value: Value): Unit =
        value.contents.foreach(
            (k, v) =>
                v match
                    case str: modularLattice.Str =>
                        str.s.asInstanceOf[NativeString].unmark()
                    case str: modularLattice.Symbol =>
                        str.s.asInstanceOf[NativeString].unmark()
                    case _ => /* None */
        )

    def gc(): Unit =
        NativeString.gc()



    // update some rudimentary analysis results
    override def intraAnalysis(component: Component): NativeIntraGC
    trait NativeIntraGC extends IntraGC with GlobalStoreIntra


    override def configString(): String = super.configString() + "\n  with domain-specific garbage collection"
}