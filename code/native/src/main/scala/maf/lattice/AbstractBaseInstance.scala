package maf.lattice

import maf.util._
import maf.core._
import maf.lattice.interfaces._
import NumOps._
import scala.scalanative.libc.stdlib.{malloc, free, atol}
import scala.scalanative.libc.string.strcmp
import scala.scalanative.libc.ctype.{tolower, toupper, islower, isupper}
import scalanative.unsigned.UnsignedRichInt

import scalanative.unsafe._
import scala.scalanative.unsigned.UByte
import scala.collection.mutable.ListBuffer


  /**
      * 
      *
      * @tparam E  the type of elements that you're willing to inject
      * @tparam T  the type of the results of injection (i.e. the elements in the lattice)
      */
    abstract class AbstractBaseInstance[E, T](val typeName: String) extends Lattice[T]:

        def inject(x: E): T
        def show(x: T): String =
            if(x == top) then typeName
            else if(x == bottom) then s"$typeName.⊥"
            else x.toString

        val bottom: T
        val top: T
        def join(x: T, y: => T): T =

            if(x == top) then top
            else if(x == bottom) then y
            else if(y == top) then top
            else if(y == bottom) then x
            else if(x == y) then x
            else top
            

        def meet(x: T, y: => T): T = 
            if(x == bottom || y == bottom) then bottom
            else if(x == top) then y
            else if(y == top) then x
            else if(x == y) then x
            else bottom

        def subsumes(x: T, y: => T): Boolean =
            // werkt niet! als y constante is, dan is y != top true
            // idem x != bottom
            // x == top || y != top || y == bottom || x != bottom || x == y

            if(x == top) then true
            else if(y == top) then false
            else if(y == bottom) then true
            else if(x == bottom) then false
            else x == y // if both x is bottom, it is certainly not equal to y at this point, since the possibility of y being a bottom is already exhausted.
             
             

        def eql[B2: BoolLattice](n1: T, n2: T): B2 =
            if (n1 == bottom || n2 == bottom) then BoolLattice[B2].bottom
            else if (n1 == top || n2 == top) then BoolLattice[B2].top
            else BoolLattice[B2].inject(n1 == n2)   