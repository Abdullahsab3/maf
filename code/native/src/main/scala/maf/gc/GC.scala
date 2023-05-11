package maf.gc

import maf.core.Expression
import maf.lattice.NativeString
import maf.modular.{AddrDependency, Dependency, GlobalStore, ModAnalysis, ReturnAddr, ReturnValue}
import maf.modular.scheme.NativeSchemeDomain
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.util.benchmarks.Timeout
import scala.collection.mutable.ListBuffer

trait GC[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr]  with GlobalStore[Expr] with ReturnValue[Expr] { inter =>

    /**
     * When the inter-analysis is finished, deallocate all the memory that is still allocated for this program.
     */
    override def run(timeout: Timeout.T): Unit =
        initializeMemory()
        super.run(timeout)

    /**
     * Initializes the memory before each program analysis.
     * 
     */
    def initializeMemory(): Unit
    /**
     * Deallocates all the memory that is still allocated for the program
     */
    def emptyMemory(): Unit


    def emptyAnalysisMemory(): Unit

    /**
     * Marks the values that should not be deallocated in this GC cycle.
     * These values are the ones that are saved in the share global analysis state after each intra-analysis
     * @param value the value that should not be deallocated
     */
    def markValues(value: Value): Unit


    /**
     * Unmark a value that was marked not to be deallocated
     * In other words: this method will make sure value will be deallocated during this GC cycle
     * This will be used when a value in the global shared state is updated, and the old value needs to be deallocated
     * @param value the value to be deallocated
     */
    def unmarkValues(value: Value): Unit
    /**
     * Garbage collect after every commit from the intra analysis to the shared analysis state.
     */
    def gc(): Unit

    override def updateAddr(store: Map[Addr, Value], addr: Addr, value: Value): Option[Map[Addr, Value]] =
        store.get(addr) match
            case None if lattice.isBottom(value) => None
            case None => Some(store + (addr -> value))
            case Some(oldValue) =>
                val newValue = lattice.join(oldValue, value)
                if newValue == oldValue then 
                    None
                else
                    unmarkValues(oldValue)
                    Some(store + (addr -> newValue))



    def updateIntraAddr(store: Map[Addr, Value], addr: Addr, value: Value): Option[Map[Addr, Value]] =
        store.get(addr) match
            case None if lattice.isBottom(value) => None
            case None => Some(store + (addr -> value))
            case Some(oldValue) =>
                val newValue = lattice.join(oldValue, value)
                if newValue == oldValue then None
                else
                    Some(store + (addr -> newValue))

    override def intraAnalysis(component: Component): IntraGC

    trait IntraGC extends GlobalStoreIntra with ReturnResultIntra {
        intra =>

        override def writeAddr(addr: Addr, value: Value): Boolean =
            updateIntraAddr(intra.store, addr, value).map { updated =>
                intra.store = updated
                trigger(AddrDependency(addr))
            }.isDefined


        override def doWrite(dep: Dependency): Boolean =
            dep match
                case AddrDependency(addr) =>
                    val value = intra.store(addr)
                    val isGlobalStateUpdated = inter.writeAddr(addr, value)
                    if isGlobalStateUpdated then
                        markValues(value)
                    isGlobalStateUpdated
                case _ => super.doWrite(dep)


        override def commit(): Unit =
            super.commit()
            gc()
    }


    override def configString(): String = super.configString() + "\n  with domain-specific garbage collection"



}
