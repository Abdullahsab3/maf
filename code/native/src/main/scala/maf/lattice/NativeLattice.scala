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

import maf.lattice.NativeString

// Low-level implementation of the constant propagation implemenation.
// prefixed with ll == low-level implementation

// Boollattice 
@extern object boolLatticeOps:
    def boolIsTrue(b: CChar): CChar = extern
    def boolIsFalse(b: CChar): CChar = extern
    def boolNot(b: CChar): CChar = extern
    
    def boolJoin(a: CChar, b: CChar): CChar = extern
    def boolMeet(a: CChar, b: CChar): CChar = extern
    def boolSubsumes(a: CChar, b: CChar): CChar = extern


object NativeLattice:

    type S = NativeString
    type S3 = Sn_struct
    type B = CChar
    type I = CInt
    type R = CDouble
    type C = CChar
    type Sym = NativeString
    type Sym3 = Sn_struct

    implicit val boolLL: BoolLattice[B] = new AbstractBaseInstance[Boolean, B]("Bool") with BoolLattice[B] {
        private def CChar2Boolean(i: B): Boolean = 
            i == 1.toByte

        val top = 4.toByte
        val bottom = 2.toByte
        def inject(b: Boolean): B = (if b then 1 else 0).toByte
        def isTrue(b: B): Boolean = 
            import boolLatticeOps.boolIsTrue
            val result = boolIsTrue(b)
            CChar2Boolean(result)
        def isFalse(b: B): Boolean =
            import boolLatticeOps.boolIsFalse
            val result = boolIsFalse(b)
            CChar2Boolean(result)
        def not(b: B): B =
            import boolLatticeOps.boolNot
            boolNot(b) 
        override def join(x: B, y: => B): B =
            import boolLatticeOps.boolJoin
            boolJoin(x, y)
        override def meet(x: B, y: => B): B =
            import boolLatticeOps.boolMeet
            boolMeet(x, y)

        override def subsumes(x: B, y: => B): Boolean =
            import boolLatticeOps.boolSubsumes
            val result = boolSubsumes(x, y)
            CChar2Boolean(result) 

    } 
        

    implicit val StringLL: AbstractBaseInstance[String, S] with StringLattice[S] = new AbstractBaseInstance[String, S]("Str") with StringLattice[S] {

        override def join(x: S, y: => S): S =
            if (x.eq(top) || y.eq(top)) then top
            else if (y.eq(bottom)) then x
            else if (x.eq(bottom)) then y
            else if (x == y) then x
            else top

        override def meet(x: S, y: => S): S =
            if (x.eq(bottom)) then bottom
            else if (y.eq(bottom)) then bottom
            else if (x.eq(top)) then y
            else if (y.eq(top)) then x
            else if (x == y) then x
            else bottom

        override def eql[B2: BoolLattice](n1: S, n2: S): B2 =
            if (n1.eq(bottom) || n2.eq(bottom)) then BoolLattice[B2].bottom
            else if (n1.eq(top) || n2.eq(top)) then BoolLattice[B2].top
            else BoolLattice[B2].inject(n1 == n2)



        override def subsumes(x: S, y: => S): Boolean =
        // werkt niet! als y constante is, dan is y != top true
        // idem x != bottom
        //x.eq(top) || !(y.eq(top)) || y.eq(bottom) || !(x.eq(bottom)) ||  x == y

            if(x.eq(top)) then true
            else if(y.eq(top)) then false
            else if(y.eq(bottom)) then true
            else if(x.eq(bottom)) then false
            else x == y // if both x is bottom, it is certainly not equal to y at this point, since the possibility of y being a bottom is already exhausted.
            

        override def show(x: S): String =
            if(x.eq(top)) then typeName
            else if(x.eq(bottom)) then s"$typeName.⊥"
            else x.toString()

        val top: S = NativeString.top
        val bottom: S = NativeString.bottom
        
        def inject(x: String): S = NativeString(x)
        
        def length[I2: IntLattice](s: S): I2 = 
            if(s.eq(top)) then
                IntLattice[I2].top
            else if (s.eq(bottom)) then
                IntLattice[I2].bottom
            else IntLattice[I2].inject(s.length)


        def append(s1: S, s2: S): S = 
            if(s1.eq(bottom) || s2.eq(bottom)) then bottom
            else if(s1.eq(top) || s2.eq(top)) then top
            else s1 ++ s2
        
            
        def substring[I2: IntLattice](s: S, from: I2, to: I2): S =
            if(s.eq(top)) then top
            else if(s.eq(bottom)) then bottom
            else if(IntLattice[I2].isBottom(from) || IntLattice[I2].isBottom(to)) then bottom
            else if(from == IntLattice[I2].top || to == IntLattice[I2].top) then top
            else
                s.substring(from.asInstanceOf[CInt], to.asInstanceOf[CInt])

        def ref[I2: IntLattice, C2: CharLattice](s: S, i: I2): C2 =
            if(s.eq(top)) then CharLattice[C2].top
            else if(s.eq(bottom)) then CharLattice[C2].bottom
            else if(i == IntLattice[I2].top) then CharLattice[C2].top
            else if(i == IntLattice[I2].bottom) then CharLattice[C2].bottom
            else 
                val c = s.ref(i.asInstanceOf[CInt])
                if(c == 0.toByte) then CharLattice[C2].bottom
                else CharLattice[C2].inject(c.toChar)

        def set[I2: IntLattice, C2: CharLattice](
            s: S,
            i: I2,
            c: C2
            ): S =
            if(s.eq(bottom)) then bottom
            else if (IntLattice[I2].isBottom(i) || CharLattice[C2].isBottom(c)) then bottom
            else if(s.eq(top)) then top
            else if (i == IntLattice[I2].top || c == CharLattice[C2].top) then top
            else
                s.replace(i.asInstanceOf[CInt], c.asInstanceOf[CChar])

        def lt[B2 : BoolLattice](s1: S, s2: S): B2 = 
            if(s1.eq(bottom) || s2.eq(bottom)) then BoolLattice[B2].bottom
            else if(s1.eq(top) || s2.eq(top)) then BoolLattice[B2].top
            else
                BoolLattice[B2].inject(s1.lt(s2))

        def toSymbol[Sym2: SymbolLattice](s: S): Sym2 = 
            if(s.eq(top)) then SymbolLattice[Sym2].top
            else if(s.eq(bottom)) then SymbolLattice[Sym2].bottom
            else 
                SymbolLattice[Sym2].inject(s.toString())

        def toNumber[I2: IntLattice](s: S): MayFail[I2, Error] =
            if(s.eq(bottom)) then MayFail.success(IntLattice[I2].bottom)
            else if(s.eq(top)) then MayFail.success(IntLattice[I2].top).addError(NotANumberString)
            else
                val l =  s.toNumber
                if(l == 0 && !s.isEmpty) then MayFail.failure(NotANumberString)
                else MayFail.success(IntLattice[I2].inject(l.toInt))
    } 

    implicit val intLL: AbstractBaseInstance[BigInt, I] with IntLattice[I] = new AbstractBaseInstance[BigInt, I]("Int") with IntLattice[I] {

        val top: I = Int.MaxValue
        val bottom: I = Int.MinValue

        def inject(x: BigInt): I = x.toInt

        def toReal[R2: RealLattice](n: I): R2 =
            if(n == top) then RealLattice[R2].top
            else if(n == bottom) then RealLattice[R2].bottom
            else RealLattice[R2].inject(n.toDouble)

        def random(n: I): I = 
            if(n == bottom) then bottom
            else top 

        private def binop(op: (Int, Int) => Int, n1: I, n2: I) = 

            if(n1 == bottom || n2 == bottom) then bottom
            else if(n1 == top || n2 == top) then top
            else op(n1, n2)

        def plus(n1: I, n2: I): I = binop(_ + _, n1, n2)
        def minus(n1: I, n2: I): I = binop(_ - _, n1, n2)
        def times(n1: I, n2: I): I = binop(_ * _, n1, n2)
        def div[F: RealLattice](n1: I, n2: I): F = 
            if(n1 == top || n2 == top) then RealLattice[F].top
            else if (n1 == bottom || n2 == bottom) then RealLattice[F].bottom
            else RealLattice[F].inject(n1.toDouble / n2.toDouble)

        def expt(n1: I, n2: I): I = binop((x, y) => Math.pow(x.toDouble, y.toDouble).toInt, n1, n2)

        def quotient(n1: I, n2: I): I = binop(_ / _, n1, n2)

        def modulo(n1: I, n2: I): I = 
            binop((x, y) => if (y == 0) then bottom else MathOps.modulo(x, y).toInt, n1 ,n2)
            
        def remainder(n1: I, n2: I): I = 
            binop((x, y) => if(y == 0) then bottom else MathOps.remainder(x, y).toInt, n1 ,n2)

        def lt[B2: BoolLattice](n1: I, n2: I): B2 = 

            if(n1 == bottom || n2 == bottom) then BoolLattice[B2].bottom
            else if(n1 == top || n2 == top) then BoolLattice[B2].top
            else BoolLattice[B2].inject(n1 < n2)

        def valuesBetween(n1: I, n2: I): Set[I] = 
            if(n1 == bottom || n2 == bottom) then Set()
            else if(n1 == top || n2 == top) then Set(top)
            else n1.to(n2).toSet

        def makeString[C2: CharLattice, S2: StringLattice](length: I, char: C2): S2 =
            if(length == bottom) then  StringLattice[S2].bottom
            else if(CharLattice[C2].isBottom(char)) then StringLattice[S2].bottom
            else if(char == CharLattice[C2].top) then StringLattice[S2].top
            else if(length == top) then StringLattice[S2].top
            else
                StringLattice[S2].inject(List.fill(length)(char).mkString)

        def toString[S2: StringLattice](n: I): S2 = 

            if(n == bottom) then StringLattice[S2].bottom
            else if(n == top) then StringLattice[S2].top
            else StringLattice[S2].inject(n.toString)


        def toChar[C2: CharLattice](n: I): C2 = 
            if(n == top) then CharLattice[C2].top
            else if(n == bottom) then CharLattice[C2].bottom
            else CharLattice[C2].inject(n.toChar)

    }

    implicit val realLL: AbstractBaseInstance[Double, R] with RealLattice[R] = new AbstractBaseInstance[Double, R]("Real") with RealLattice[R] {
        
        val top = Double.MaxValue
        val bottom = Double.MinValue

        def inject(x: Double) = x

        def toInt[I2: IntLattice](n: R): I2 = 
            if(n == top) then IntLattice[I2].top
            else if(n == bottom) then IntLattice[I2].bottom
            else IntLattice[I2].inject(n.toInt)

        def ceiling(n: R): R =
            if(n == top || n == bottom) then n
            else scalanative.libc.math.ceil(n)

        def floor(n: R): R = 
            if(n == top || n == bottom) then n
            else scalanative.libc.math.floor(n)

        def round(n: R): R = 
            if(n == top || n == bottom) then n
            else MathOps.round(n)

        def random(n: R): R = 
            if(n == bottom) then bottom
            else top

        def log(n: R): R = 
            if(n == top || n == bottom) then n
            else if(n < 0) then 
                bottom
            else
                scalanative.libc.math.log(n)

        def sin(n: R): R =
            if(n == top || n == bottom) then n
            else scalanative.libc.math.sin(n)

        def asin(n: R): R =
            if(n == top || n == bottom) then n
            else if(n > 1 || n < -1) then bottom
            else asin(n)

        def cos(n: R): R =
            if(n == top || n == bottom) then n
            else scalanative.libc.math.cos(n)

        def acos(n: R): R = 
            if(n == top || n == bottom) then n
            else if(n > 1 || n < -1) then bottom
            else scalanative.libc.math.acos(n)

        def tan(n: R): R =
            if(n == top || n == bottom) then n
            else scalanative.libc.math.tan(n) // TODO this does not check safety. You should check on pi/2 as the tan would be undefined
        def atan(n: R): R = 
            if(n == top || n == bottom) then n
            else scalanative.libc.math.atan(n)

        def sqrt(n: R): R = 
            if(n == top || n == bottom) then n
            else if(n < 0) then bottom
            else scalanative.libc.math.sqrt(n)

        private def binop(
            op: (Double, Double) => Double,
            n1: R,
            n2: R
            ) = 

            if (n1 == bottom || n2 == bottom) then bottom
            else if(n1 == top || n2 == top) then top
            else op(n1, n2)

        def plus(n1: R, n2: R): R = binop(_ + _, n1, n2)
        def minus(n1: R, n2: R): R = binop(_ - _, n1, n2)
        def times(n1: R, n2: R): R = binop(_ * _, n1, n2)
        def div(n1: R, n2: R): R = binop(_ / _, n1, n2)
        def expt(n1: R, n2: R): R = binop((x, y) => Math.pow(x, y), n1, n2)
        def lt[B2: BoolLattice](n1: R, n2: R): B2 = 

            if(n1 == bottom || n2 == bottom) then BoolLattice[B2].bottom
            else if(n1 == top || n2 == top) then BoolLattice[B2].top
            else BoolLattice[B2].inject(n1 < n2)
            
        def toString[S2: StringLattice](n: R): S2 =

            if (n == bottom) then StringLattice[S2].bottom
            else if(n == top) then StringLattice[S2].top
            else StringLattice[S2].inject(n.toString)
            
    }


    implicit val charLL: AbstractBaseInstance[Char, C] with CharLattice[C] = new AbstractBaseInstance[Char, C]("Char") with CharLattice[C] {
        
        val top: C = Byte.MaxValue
        val bottom: C = Byte.MinValue
        
        def inject(x: Char) = x.toByte

        def downCase(c: C): C =
            if(c == top || c == bottom) then c
            else tolower(c).toByte

        def upCase(c: C): C = 
            if(c == top || c == bottom) then c
            else toupper(c).toByte

        def toString[S2: StringLattice](c: C): S2 = 
            if(c == top) then StringLattice[S2].top
            else if (c == bottom) then StringLattice[S2].bottom
            else StringLattice[S2].inject(c.toString)

        def toInt[I2: IntLattice](c: C): I2 = c.asInstanceOf[I2] // TODO: change later. make I2 maybe IntLattice[I]

        def isLower[B2: BoolLattice](c: C): B2 = 
            if(c == bottom) then BoolLattice[B2].bottom
            else if(c == top) then BoolLattice[B2].top
            else BoolLattice[B2].inject(islower(c) == 1)

        def isUpper[B2: BoolLattice](c: C): B2 =

            if(c == bottom) then BoolLattice[B2].bottom
            else if(c == top) then BoolLattice[B2].top
            else BoolLattice[B2].inject(isupper(c) == 1)


        override def charEq[B2: BoolLattice](c1: C, c2: C): B2 = 
            if(c1 == top || c2 == top) then BoolLattice[B2].top
            else if(c1 == bottom || c2 == bottom) then BoolLattice[B2].bottom
            else BoolLattice[B2].inject(c1 == c2)


        override def charLt[B2: BoolLattice](c1: C, c2: C): B2 = 

            if(c1 == top || c2 == top) then BoolLattice[B2].top
            else if(c1 == bottom || c2 == bottom) then BoolLattice[B2].bottom
            else BoolLattice[B2].inject(c1 < c2)


        override def charEqCI[B2: BoolLattice](c1: C, c2: C): B2 = 

            if(c1 == top || c2 == top) then BoolLattice[B2].top
            else if(c1 == bottom || c2 == bottom) then BoolLattice[B2].bottom
            else BoolLattice[B2].inject(toupper(c1) == toupper(c2))

            
        override def charLtCI[B2: BoolLattice](c1: C, c2: C): B2 = 

            if(c1 == top || c2 == top) then BoolLattice[B2].top
            else if(c1 == bottom || c2 == bottom) then BoolLattice[B2].bottom
            else BoolLattice[B2].inject(toupper(c1) < toupper(c2))
    }


    // TODO: symshow
    implicit val symLL: AbstractBaseInstance[String, Sym]with SymbolLattice[Sym] = new AbstractBaseInstance[String, Sym]("Symbol") with SymbolLattice[Sym] {
        
        val top: Sym = NativeString.top
        val bottom: Sym = NativeString.bottom

        override def join(x: Sym, y: => Sym): Sym =
            if (x.eq(top) || y.eq(top)) then top
            else if (y.eq(bottom)) then x
            else if (x.eq(bottom)) then y
            else if (x == y) then x
            else top


        override def eql[B2: BoolLattice](n1: Sym, n2: Sym): B2 =
            if (n1.eq(bottom) || n2.eq(bottom)) then BoolLattice[B2].bottom
            else if (n1.eq(top) || n2.eq(top)) then BoolLattice[B2].top
            else BoolLattice[B2].inject(n1 == n2)

        override def subsumes(x: Sym, y: => Sym): Boolean =
        // werkt niet! als y constante is, dan is y != top true
        // idem x != bottom
        //x.eq(top) || !(y.eq(top)) || y.eq(bottom) || !(x.eq(bottom)) ||  x == y

            if(x.eq(top)) then true
            else if(y.eq(top)) then false
            else if(y.eq(bottom)) then true
            else if(x.eq(bottom)) then false
            else x == y // if both x is bottom, it is certainly not equal to y at this point, since the possibility of y being a bottom is already exhausted.
            
            
        def inject(x: String): Sym = NativeString(x)

        def toString[S2: StringLattice](s: Sym): S2 = 
            if(s == top) then StringLattice[S2].top
            else if(s == bottom) then StringLattice[S2].bottom
            else StringLattice[S2].inject(s.toString)

        override def show(x: Sym): String =
            if(x == top) then typeName
            else if(x == bottom) then s"$typeName.⊥"
            else s"'${x.toString}"
    }
