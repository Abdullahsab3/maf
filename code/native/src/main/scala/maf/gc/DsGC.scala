package maf.gc

import maf.core.*
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.NativeString
import maf.modular.scheme.NativeSchemeDomain
import maf.modular.{AddrDependency, Dependency, GlobalStore, ModAnalysis}

import scala.collection.mutable.ListBuffer

trait DsGC[Expr <: Expression] extends ModAnalysis[Expr] with NativeSchemeDomain with GlobalStore[Expr] { inter =>

    // update some rudimentary analysis results
    override def intraAnalysis(component: Component): IntraGC
    trait IntraGC extends GlobalStoreIntra {
        intra =>
            var stay: ListBuffer[Dependency] = ListBuffer()

            private def markToStay: Unit =
                stay.foreach {
                    case AddrDependency(addr) =>
                        intra.store(addr).contents.foreach((k, v) => v match
                            case str: modularLattice.Str => str.s.asInstanceOf[NativeString].markToStay()
                            case _ => /* None */)
                }

            def gc(): Unit =
                markToStay
                NativeString.gc()


            override def commit(): Unit =

                R.foreach(inter.register(component, _))
                W.foreach(dep =>
                    if doWrite(dep) then
                        stay += dep
                        inter.trigger(dep)
                )

                C.foreach(inter.spawn(_, component))
                gc()
    }


    override def configString(): String = super.configString() + "\n  with domain-specific garbage collection"
}