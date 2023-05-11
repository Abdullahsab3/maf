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

trait DebuggableGC[Expr <: Expression](extensive: Boolean) extends ModAnalysis[Expr] with NativeGC[Expr] with GC[Expr] with NativeSchemeDomain with GlobalStore[Expr] { inter =>
    
    def printNativeStringCount(): Unit = 
        println(s"${NativeString.allocatedStrings.size} remaining")
    override def emptyAnalysisMemory(): Unit = 
        printNativeStringCount()
        if extensive then
            NativeString.allocatedStrings.foreach(
                s => println(s.prettyString()))
            println()
        super.emptyAnalysisMemory()

    override def gc(): Unit = 
        val ctr = NativeString.allocatedStrings.size
        printNativeStringCount()
        super.gc()
        val deallocatedCtr = NativeString.allocatedStrings.size - ctr
        println(s"Dealocated $deallocatedCtr strings")
        printNativeStringCount()

    override def intraAnalysis(component: Component): DebuggableGCIntra
    trait DebuggableGCIntra extends GlobalStoreIntra with  NativeIntraGC { intra =>
        var garbage: Map[Dependency, Value] = Map.empty
        var needed: Map[Dependency, Value] = Map.empty

        override def doWrite(dep: Dependency): Boolean =
            dep match
                case AddrDependency(addr) =>
                    val value = intra.store(addr)
                    val isGlobalStateUpdated = inter.writeAddr(addr, value)
                    if isGlobalStateUpdated then
                        markValues(value)
                        needed += (dep, value)
                    else
                        garbage += (dep, value)
                    isGlobalStateUpdated
                case _ => super.doWrite(dep)

        override def commit(): Unit =
            super.commit()
            println(s"${garbage.size} garbage values")
            println(s"${needed.size} needed values")
            gc()
            

    }



}
