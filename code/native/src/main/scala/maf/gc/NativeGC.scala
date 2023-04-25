package maf.gc

import maf.core.{Expression}
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.NativeString
import maf.modular.scheme.NativeSchemeDomain
import maf.modular.scheme.NativeSchemeDomain.modularLattice
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.modular.{AddrDependency, Dependency, GlobalStore, ModAnalysis}
import maf.util.StoreUtil
import maf.util.benchmarks.Timeout

import scala.collection.mutable.ListBuffer

trait NativeGC[Expr <: Expression] extends ModAnalysis[Expr] with GC[Expr] with NativeSchemeDomain with GlobalStore[Expr] { inter =>

    def emptyMemory(): Unit =
        NativeString.deallocateAllStrings()

    def markValues(value: Value): Unit =
        value.contents.foreach(
            (k, v) =>
                v match
                    case str: modularLattice.Str =>
                        println(str.s)
                        str.s.asInstanceOf[NativeString].mark()
                    case _ => /* None */
        )
    def unmarkValues(value: Value): Unit =
        value.contents.foreach(
            (k, v) =>
                v match
                    case str: modularLattice.Str =>
                        println(str.s)
                        str.s.asInstanceOf[NativeString].unmark()
                    case _ => /* None */
        )
    

    // update some rudimentary analysis results
    override def intraAnalysis(component: Component): NativeIntraGC
    trait NativeIntraGC extends IntraGC with GlobalStoreIntra {
        intra =>

        def gc(): Unit =
            NativeString.gc()


    }


    override def configString(): String = super.configString() + "\n  with domain-specific garbage collection"
}