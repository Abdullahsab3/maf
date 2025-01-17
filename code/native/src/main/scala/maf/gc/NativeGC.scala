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
import scala.collection.mutable.Stack

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
                                str.s.asInstanceOf[NativeString].decreaseDependencyCounter()
                            case str: modularLattice.Symbol =>
                                str.s.asInstanceOf[NativeString].decreaseDependencyCounter()
                            case _ => /* None */
                )
        )
        gc()




    def increaseDependencyCounter(value: Value): Unit =
        val stack = Stack(value)

        while(stack.nonEmpty) do 
            val current = stack.pop()

            for v <- current.contents.values do 
                v match
                        case str: modularLattice.Str =>
                            str.s.asInstanceOf[NativeString].increaseDependencyCounter()
                        case str: modularLattice.Symbol =>
                            str.s.asInstanceOf[NativeString].increaseDependencyCounter()
                        case modularLattice.Cons(car, cdr) =>
                            stack.push(car)
                            stack.push(cdr)
                        case modularLattice.Vec(size, elements) =>
                            elements.foreach((i, el) => stack.push(el))
                        case _ => /* None */

    def decreaseDependencyCounter(value: Value): Unit =
        val stack = Stack(value)
        
        while(stack.nonEmpty) do
            val current = stack.pop()

            for v <- current.contents.values do
                v match
                        case str: modularLattice.Str =>
                            str.s.asInstanceOf[NativeString].decreaseDependencyCounter()
                        case str: modularLattice.Symbol =>
                            str.s.asInstanceOf[NativeString].decreaseDependencyCounter()
                        case modularLattice.Cons(car, cdr) =>
                            stack.push(car)
                            stack.push(cdr)
                        case modularLattice.Vec(size, elements) =>
                            elements.foreach((i, el) => stack.push(el))
                        case _ => /* None */
    def gc(): Unit =
        NativeString.gc()


    override def configString(): String = super.configString() + "\n  with domain-specific garbage collection"
}