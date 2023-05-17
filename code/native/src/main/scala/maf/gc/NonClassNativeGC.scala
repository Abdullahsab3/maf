package maf.gc

import maf.core.Expression
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.modular.scheme.NativeSchemeDomain
import maf.modular.scheme.NativeSchemeDomain.modularLattice
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.modular.{AddrDependency, Dependency, GlobalStore, ModAnalysis}
import maf.util.StoreUtil
import maf.util.benchmarks.Timeout

import scala.collection.mutable.ListBuffer
import scala.scalanative.unsafe.fromCString
import scala.collection.mutable.Stack
import maf.lattice.NonClassNativeString.*
import maf.lattice.NonClassNativeString
import maf.modular.scheme.NonClassNativeSchemeDomain

trait NonClassNativeGC[Expr <: Expression] extends ModAnalysis[Expr] with GC[Expr] with NonClassNativeSchemeDomain with GlobalStore[Expr] { inter =>

    def initializeMemory(): Unit =
        NonClassNativeString.initializeMemory()


    def emptyMemory(): Unit =
        NonClassNativeString.deallocateAllStrings()

    override def emptyAnalysisMemory(): Unit =
        store.foreach(
            (a, v) =>
                v.contents.foreach(
                    (mk, mv) =>
                        mv match
                            case str: modularLattice.Str =>
                                NonClassNativeString.makeGarbage(str.s.asInstanceOf[NonClassNativeString.S])
                            case str: modularLattice.Symbol =>
                                 NonClassNativeString.makeGarbage(str.s.asInstanceOf[NonClassNativeString.S])
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
                            NonClassNativeString.increaseDependencyCounter(str.s.asInstanceOf[NonClassNativeString.S])
                        case str: modularLattice.Symbol =>
                             NonClassNativeString.increaseDependencyCounter(str.s.asInstanceOf[NonClassNativeString.S])
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
                            NonClassNativeString.decreaseDependencyCounter(str.s.asInstanceOf[NonClassNativeString.S])
                        case str: modularLattice.Symbol =>
                            NonClassNativeString.decreaseDependencyCounter(str.s.asInstanceOf[NonClassNativeString.S])
                        case modularLattice.Cons(car, cdr) =>
                            stack.push(car)
                            stack.push(cdr)
                        case modularLattice.Vec(size, elements) =>
                            elements.foreach((i, el) => stack.push(el))
                        case _ => /* None */
    def gc(): Unit =
        NonClassNativeString.gc()


    override def configString(): String = super.configString() + "\n  with domain-specific garbage collection"
}