package maf.gc

import maf.core.*
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.NativeString
import maf.modular.scheme.NativeSchemeDomain
import maf.modular.{AddrDependency, Dependency, GlobalStore, ModAnalysis}
import maf.util.StoreUtil

import scala.collection.mutable.ListBuffer

trait DsGC[Expr <: Expression] extends ModAnalysis[Expr] with NativeSchemeDomain with GlobalStore[Expr] { inter =>

    // update some rudimentary analysis results
    override def intraAnalysis(component: Component): IntraGC
    trait IntraGC extends GlobalStoreIntra {
        intra =>

        override def doWrite(dep: Dependency): Boolean =
            dep match
            case AddrDependency(addr) =>
                val value = intra.store(addr)
                val isGlobalStateUpdated = inter.writeAddr(addr, value)
                if isGlobalStateUpdated then
                    value.contents.foreach(
                        (k, v) =>
                            v match
                                case str: modularLattice.Str => 
                                    str.s.asInstanceOf[NativeString].mark()
                                case _ => /* None */)
                isGlobalStateUpdated
            case _ => super.doWrite(dep)

        def gc(): Unit =

            NativeString.gc()
            println()
            println(StoreUtil.storeString(intra.store, false, Some(50)))
            println(NativeString.allocatedStrings)


        override def commit(): Unit =
            R.foreach(inter.register(component, _))
            W.foreach(dep =>
                if doWrite(dep) then
                    inter.trigger(dep)
            )

            C.foreach(inter.spawn(_, component))
            gc()
    }


    override def configString(): String = super.configString() + "\n  with domain-specific garbage collection"
}