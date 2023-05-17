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

object AltNativeLattice:
    type B = CChar
    type S2 = String
    type Sym2 = String
    type S =  NonClassNativeString.S
    type Sym = NonClassNativeString.S


    implicit val boolLL:  BoolLattice[B] = new AbstractBaseInstance[Boolean, B]("Bool") with BoolLattice[B] {
            
            private def CChar2Boolean(i: B): Boolean = 
                i == 1.toByte
            
            val top = 4.toByte
            val bottom = 2.toByte
            def inject(b: Boolean): B = (if b then 1 else 0).toByte

            def isTrue(b: B): Boolean = 
                if(b == top) then true
                else if(b == bottom) then false
                else b == 1.toByte

            def isFalse(b: B): Boolean = 
                if(b == top) then true
                else if(b == bottom) then false
                else b != 1.toByte
            def not(b: B): B = 
                if(b < bottom) then inject(b != 1)
                else b

            override def show(x: B): String =
                if(x == top) then typeName
                else if(x == bottom) then s"$typeName.⊥"
                else CChar2Boolean(x).toString
        }


    implicit val StringLL: AbstractBaseInstance[String, S] with StringLattice[S] = new AbstractBaseInstance[String, S]("Str") with StringLattice[S] {

        override def join(x: S, y: => S): S =
            if (NonClassNativeString.eq(x, top) || NonClassNativeString.eq(y, top)) then top
            else if (NonClassNativeString.eq(y, bottom)) then x
            else if (NonClassNativeString.eq(x, bottom)) then y
            else if (NonClassNativeString.equals(x, y)) then x
            else top

        override def meet(x: S, y: => S): S =
            if (NonClassNativeString.eq(x, bottom)) then bottom
            else if (NonClassNativeString.eq(y, bottom)) then bottom
            else if (NonClassNativeString.eq(x, top)) then y
            else if (NonClassNativeString.eq(y, top)) then x
            else if (NonClassNativeString.equals(x, y)) then x
            else bottom

        override def eql[B2: BoolLattice](n1: S, n2: S): B2 =
            if (NonClassNativeString.eq(n1, bottom) || NonClassNativeString.eq(n2, bottom)) then BoolLattice[B2].bottom
            else if (NonClassNativeString.eq(n1, top) || NonClassNativeString.eq(n2, top)) then BoolLattice[B2].top
            else BoolLattice[B2].inject(NonClassNativeString.equals(n1, n2))



        override def subsumes(x: S, y: => S): Boolean =
        // werkt niet! als y constante is, dan is y != top true
        // idem x != bottom
        //x.eq(top) || !(y.eq(top)) || y.eq(bottom) || !(x.eq(bottom)) ||  x == y

            if(NonClassNativeString.eq(x, top)) then true
            else if(NonClassNativeString.eq(y, top)) then false
            else if(NonClassNativeString.eq(y, bottom)) then true
            else if(NonClassNativeString.eq(x, bottom)) then false
            else NonClassNativeString.equals(x, y) // if both x is bottom, it is certainly not equal to y at this point, since the possibility of y being a bottom is already exhausted.
            

        override def show(x: S): String =
            if(NonClassNativeString.eq(x, top)) then typeName
            else if(NonClassNativeString.eq(x, bottom)) then s"$typeName.⊥"
            else NonClassNativeString.toString(x)

        val top: S = NonClassNativeString.top
        val bottom: S = NonClassNativeString.bottom
        
        def inject(x: String): S = NonClassNativeString.makeNativeString(x)
        
        def length[I2: IntLattice](s: S): I2 = 
            if(NonClassNativeString.eq(s, top)) then
                IntLattice[I2].top
            else if (NonClassNativeString.eq(s, bottom)) then
                IntLattice[I2].bottom
            else IntLattice[I2].inject(s._1)


        def append(s1: S, s2: S): S = 
            if(NonClassNativeString.eq(s1, bottom) || NonClassNativeString.eq(s2, bottom)) then bottom
            else if(NonClassNativeString.eq(s1, top) || NonClassNativeString.eq(s2, top)) then top
            else NonClassNativeString.++(s1, s2)
        
            
        def substring[I2: IntLattice](s: S, from: I2, to: I2): S =
            if(NonClassNativeString.eq(s, top)) then top
            else if(NonClassNativeString.eq(s, bottom)) then bottom
            else if(IntLattice[I2].isBottom(from) || IntLattice[I2].isBottom(to)) then bottom
            else if(from == IntLattice[I2].top || to == IntLattice[I2].top) then top
            else
                NonClassNativeString.substring(s, from.asInstanceOf[CInt], to.asInstanceOf[CInt])

        def ref[I2: IntLattice, C2: CharLattice](s: S, i: I2): C2 =
            if(NonClassNativeString.eq(s, top)) then CharLattice[C2].top
            else if(NonClassNativeString.eq(s, bottom)) then CharLattice[C2].bottom
            else if(i == IntLattice[I2].top) then CharLattice[C2].top
            else if(i == IntLattice[I2].bottom) then CharLattice[C2].bottom
            else 
                val c = NonClassNativeString.ref(s, i.asInstanceOf[CInt])
                if(c == 0.toByte) then CharLattice[C2].bottom
                else CharLattice[C2].inject(c.toChar)

        def set[I2: IntLattice, C2: CharLattice](
            s: S,
            i: I2,
            c: C2
            ): S =
            if(NonClassNativeString.eq(s, bottom)) then bottom
            else if (IntLattice[I2].isBottom(i) || CharLattice[C2].isBottom(c)) then bottom
            else if(NonClassNativeString.eq(s, top)) then top
            else if (i == IntLattice[I2].top || c == CharLattice[C2].top) then top
            else
                NonClassNativeString.replace(s, i.asInstanceOf[CInt], c.asInstanceOf[CChar])

        def lt[B2 : BoolLattice](s1: S, s2: S): B2 = 
            if(NonClassNativeString.eq(s1, bottom) || NonClassNativeString.eq(s2, bottom)) then BoolLattice[B2].bottom
            else if(NonClassNativeString.eq(s1, top) || NonClassNativeString.eq(s2, top)) then BoolLattice[B2].top
            else
                BoolLattice[B2].inject(NonClassNativeString.lt(s1, s2))

        def toSymbol[Sym2: SymbolLattice](s: S): Sym2 = 
            if(NonClassNativeString.eq(s, top)) then SymbolLattice[Sym2].top
            else if(NonClassNativeString.eq(s, bottom)) then SymbolLattice[Sym2].bottom
            else 
                SymbolLattice[Sym2].inject(NonClassNativeString.toString(s))

        def toNumber[I2: IntLattice](s: S): MayFail[I2, Error] =
            if(NonClassNativeString.eq(s, bottom)) then MayFail.success(IntLattice[I2].bottom)
            else if(NonClassNativeString.eq(s, top)) then MayFail.success(IntLattice[I2].top).addError(NotANumberString)
            else
                val l =  NonClassNativeString.toNumber(s)
                if(l == 0 && !NonClassNativeString.isEmpty(s)) then MayFail.failure(NotANumberString)
                else MayFail.success(IntLattice[I2].inject(l.toInt))
    } 

    implicit val symLL: AbstractBaseInstance[String, Sym]with SymbolLattice[Sym] = new AbstractBaseInstance[String, Sym]("Symbol") with SymbolLattice[Sym] {
        
        val top: Sym = NonClassNativeString.top
        val bottom: Sym = NonClassNativeString.bottom

        override def join(x: S, y: => S): S =
            if (NonClassNativeString.eq(x, top) || NonClassNativeString.eq(y, top)) then top
            else if (NonClassNativeString.eq(y, bottom)) then x
            else if (NonClassNativeString.eq(x, bottom)) then y
            else if (NonClassNativeString.equals(x, y)) then x
            else top

        override def meet(x: S, y: => S): S =
            if (NonClassNativeString.eq(x, bottom)) then bottom
            else if (NonClassNativeString.eq(y, bottom)) then bottom
            else if (NonClassNativeString.eq(x, top)) then y
            else if (NonClassNativeString.eq(y, top)) then x
            else if (NonClassNativeString.equals(x, y)) then x
            else bottom

        override def eql[B2: BoolLattice](n1: S, n2: S): B2 =
            if (NonClassNativeString.eq(n1, bottom) || NonClassNativeString.eq(n2, bottom)) then BoolLattice[B2].bottom
            else if (NonClassNativeString.eq(n1, top) || NonClassNativeString.eq(n2, top)) then BoolLattice[B2].top
            else BoolLattice[B2].inject(NonClassNativeString.equals(n1, n2))

        def inject(x: String): Sym = NonClassNativeString.makeNativeString(x)

        def toString[S2: StringLattice](s: Sym): S2 = 
            if(s == top) then StringLattice[S2].top
            else if(s == bottom) then StringLattice[S2].bottom
            else StringLattice[S2].inject(NonClassNativeString.toString(s))

        override def show(x: Sym): String =
            if(x == top) then typeName
            else if(x == bottom) then s"$typeName.⊥"
            else s"'${NonClassNativeString.toString(x)}"
    }


    implicit val StringLL2: AbstractBaseInstance[String, S2] with StringLattice[S2] = new AbstractBaseInstance[String, S2]("Str") with StringLattice[S2] {

            override def show(x: S2): String =
                if(x == top) then typeName
                else if(x == bottom) then s"$typeName.⊥"
                else x

            val top: S2 =    "fs6f5ertt85e§(è)"
            val bottom: S2 = "er6g85esfetr98'§"
            
            def inject(x: String): S2 = x
            
            def length[I2: IntLattice](s: S2): I2 = 
                if(s == top) then IntLattice[I2].top
                else if (s == bottom) then IntLattice[I2].bottom
                else IntLattice[I2].inject(s.length)

            override def eql[B2: BoolLattice](n1: S2, n2: S2): B2 =
                if (n1 == bottom || n2 == bottom) then BoolLattice[B2].bottom
                else if (n1 == top || n2 == top) then BoolLattice[B2].top
                else BoolLattice[B2].inject(n1 == n2)

            def append(s1: S2, s2: S2): S2 = 
                if(s1 == bottom || s2 == bottom) then bottom
                else if(s1 == top || s2 == top) then top
                else 
                    inject(s1 ++ s2)
            

            def substring[I2: IntLattice](s: S2, from: I2, to: I2): S2 =
                if(s == top) then top
                else if(s == bottom) then bottom
                else if(IntLattice[I2].isBottom(from) || IntLattice[I2].isBottom(to)) then bottom
                else if(from == IntLattice[I2].top || to == IntLattice[I2].top) then top
                else
                    inject(s.substring(from.asInstanceOf[Int], to.asInstanceOf[Int]).nn)

            def ref[I2: IntLattice, C2: CharLattice](s: S2, i: I2): C2 =
                if(s == top) then CharLattice[C2].top
                else if(s == bottom) then CharLattice[C2].bottom
                else if(i == IntLattice[I2].top) then CharLattice[C2].top
                else if(i == IntLattice[I2].bottom) then CharLattice[C2].bottom
                else 
                    CharLattice[C2].inject(s(i.asInstanceOf[Int]))

            def set[I2: IntLattice, C2: CharLattice](
                s: S2,
                i: I2,
                c: C2
            ): S2 =
                if(s == bottom) then bottom
                else if (IntLattice[I2].isBottom(i) || CharLattice[C2].isBottom(c)) then bottom
                else if(s == top) then top
                else if (i == IntLattice[I2].top || c == CharLattice[C2].top) then top
                else 
                    inject(s.replace(s(i.asInstanceOf[Int]), c.asInstanceOf[Char]).nn)

            def lt[B2 : BoolLattice](s1: S2, s2: S2): B2 = 
                if(s1 == bottom || s2 == bottom) then BoolLattice[B2].bottom
                else if(s1 == top || s2 == top) then BoolLattice[B2].top
                else
                    BoolLattice[B2].inject(s1 < s2)

            def toSymbol[Sym2: SymbolLattice](s: S2): Sym2 = 
                if(s == top) then SymbolLattice[Sym2].top
                else if(s == bottom) then SymbolLattice[Sym2].bottom
                else 
                    SymbolLattice[Sym2].inject(s)

            def toNumber[I2: IntLattice](s: S2) = 
                if(s == bottom) then MayFail.success(IntLattice[I2].bottom)
                else if(s == top) then MayFail.success(IntLattice[I2].top).addError(NotANumberString)
                else
                    MayFail.success(IntLattice[I2].inject(s.toInt))

        }

    
    implicit val symLL2: AbstractBaseInstance[String, Sym2] with SymbolLattice[Sym2] = new AbstractBaseInstance[String, Sym2]("Symbol") with SymbolLattice[Sym2] {
        val top: S2 =    "fs6f5ertt85e§(è)"
        val bottom: S2 = "er6g85esfetr98'§"

        def inject(x: String): Sym2 = x
        
        def toString[S2: StringLattice](s: Sym2): S2 =
            if(s == top) then StringLattice[S2].top
            else if (s == bottom) then StringLattice[S2].bottom
            else StringLattice[S2].inject(s)

        override def show(x: Sym2): String =
            if(x == top) then typeName
            else if(x == bottom) then s"$typeName.⊥"
            else s"'${x.toString}"
    }