package maf.lattice

import maf.util._
import maf.core._
import maf.lattice.interfaces._
import NumOps._
import scala.scalanative.libc.stdlib.{malloc, free, atol}
import scala.scalanative.libc.string.{strlen, strcpy, strcat, strcmp}
import scala.scalanative.libc.ctype.{tolower, toupper, islower, isupper}
import scalanative.unsigned.UnsignedRichInt

import scalanative.unsafe._
import scala.scalanative.unsigned.UByte
import scala.collection.mutable.ListBuffer

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


    // First field: length of the string when we have a constant. A useless number otherwise
    // second field: pointer to the memory containing the string
    type Sn_struct = CStruct2[CInt, CString]
    type S = Ptr[Sn_struct]
    type B = CChar
    type I = CInt
    type R = CDouble
    type C = CChar
    type Sym = Ptr[Sn_struct]

    val SnSize = sizeof[Sn_struct]

    def cstrEquals(s1 : CString, s2: CString, l1: Int, l2: Int): Boolean =
        var results = true
        if(l1 == l2) then
            var i = 0
            while(i < l2) do
                if(!(s1 + i) != !(s2 + i)) then 
                    results = false
                    i = l2 // break
                i = i + 1
        else 
            results = false
        results

    implicit class SOps(val s: S):
        @inline
        final def ==(that: S): Boolean =
            cstrEquals(s._2, that._2, s._1, that._1)
            


    /**
      * 
      *
      * @tparam E  the type of elements that you're willing to inject
      * @tparam T  the type of the results of injection (i.e. the elements in the lattice)
      */
    abstract class AbstractBaseInstance[E, T](val typeName: String) extends Lattice[T]:
        import maf.lattice.NativeLattice.SOps

        def inject(x: E): T
        def show(x: T): String =
            if(x == top) then typeName
            else if(x == bottom) then s"$typeName.⊥"
            else x.toString

        val bottom: T
        val top: T
        def join(x: T, y: => T): T =
            // exhaust all the possibilities in order to know x and y are "constants" at the end

            if(x == top || y == top) then top
            else if (y == bottom) then x
            else if (x == bottom) then y
            else if(x == y) then x
            else top

        def meet(x: T, y: => T): T = 
            if(x == bottom) then bottom
            else if(y == bottom) then bottom
            else if(x == top) then y
            else if(y == top) then x
            else if(x == y) then x
            else bottom

        def subsumes(x: T, y: => T): Boolean =

            if(x == top) then true
            else if(y == top) then false
            else if(y == bottom) then true 
            else if(x == bottom) then false
            else x == y // if both x is bottom, it is certainly not equal to y at this point, since the possibility of y being a bottom is already exhausted.

        def eql[B2: BoolLattice](n1: T, n2: T): B2 =
            if (n1 == bottom || n2 == bottom) then BoolLattice[B2].bottom
            else if (n1 == top || n2 == top) then BoolLattice[B2].top
            else BoolLattice[B2].inject(n1 == n2)

        

    object L:
        implicit val boolLL:  BoolLattice[B] = new AbstractBaseInstance[Boolean, B]("Bool") with BoolLattice[B] {
            
            private def CChar2Boolean(i: B): Boolean = 
                i == 1.toByte
            
            val top = 3
            val bottom = 2
            def inject(b: Boolean): B = if b then 1 else 0
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
            override def show(x: B): String =
                if(x == top) then typeName
                else if(x == bottom) then s"$typeName.⊥"
                else CChar2Boolean(x).toString

            override def eql[B2: BoolLattice](n1: B, n2: B): B2 =
                if (n1 == bottom || n2 == bottom) then BoolLattice[B2].bottom
                else if (n1 == top || n2 == top) then BoolLattice[B2].top
                else BoolLattice[B2].inject(n1 == n2)
        }
        
        // Ptr[CStruct2[CInt, CString]]

        implicit val StringLL: AbstractBaseInstance[String, S] with StringLattice[S] = new AbstractBaseInstance[String, S]("Str") with StringLattice[S] {

            val allocatedStrings: ListBuffer[S] = ListBuffer()


            // TODO: maybe you can change the representation of top and bottom
            // by having negative numbers, and when comparing against these numbers itd fail

            // TODO refactor. you can do a lot better than this.
            override def subsumes(x: S, y: => S): Boolean =
                if(x == top) then true
                else if(y == top) then false
                else if(y == bottom) then true 
                else if(x == bottom) then false
                else cstrEquals(x._2, y._2, x._1, y._1)

            // assuming sufficient memory is allocated at dest
            private def strcpy(dest: CString, src: CString, length: Int): Unit =
                var i = 0
                while(i <= length) do
                    !(dest + i) = !(src + i)
                    i = i + 1

            // assuming sufficient memory is allcoated at dest
            private def strcat(dest: CString, src: CString, destlength: Int, srclength: Int): Unit = 
                var i = destlength
                while(i <= srclength + destlength) do
                    !(dest + i) = !(src + i - destlength)
                    i = i + 1


            override def show(x: S): String =
                if(x == top) then typeName
                else if(x == bottom) then s"$typeName.⊥"
                else fromCString(x._2)

            val top: S = 
                val struct = malloc(SnSize).asInstanceOf[S]
                struct._1 = 0
                struct
            val bottom: S = 
                val struct = malloc(SnSize).asInstanceOf[S]
                struct._1 = 1
                struct
            
            def inject(x: String): S =
                val struct = malloc(SnSize).asInstanceOf[S]
                val stringLength = x.length()
                struct._1 = stringLength
                struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
                var i = 0
                while(i < stringLength) do
                    !(struct._2 + i) = x(i).toByte
                    i = i +1
                !(struct._2 + stringLength) = 0.toByte
                //allocatedStrings += struct
                struct
            
            def length[I2: IntLattice](s: S): I2 = 
                if(s == top) then IntLattice[I2].top
                else if (s == bottom) then IntLattice[I2].bottom
                else IntLattice[I2].inject(s._1)


            def append(s1: S, s2: S): S = 
                if(s1 == bottom || s2 == bottom) then bottom
                else if(s1 == top || s2 == top) then top
                else 
                    val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
                    val stringLength = s1._1 + s2._1
                    struct._1 = stringLength  
                    struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
                    strcpy(struct._2, s1._2, s1._1)
                    strcat(struct._2, s2._2, s1._1, s2._1)
                    struct
            
            private def getSubString(s: CString, sub: CString, from: Int, to: Int): Unit =
                var i = 0
                while(i < to) do
                    !(sub + i) = !(s + from + i - 1)
                
                !(sub + i) = 0.toByte // 0-terminated string in C
                
            def substring[I2: IntLattice](s: S, from: I2, to: I2): S =
                if(s == top) then top
                else if(s == bottom) then bottom
                else if(IntLattice[I2].isBottom(from) || IntLattice[I2].isBottom(to)) then bottom
                else if(from == IntLattice[I2].top || to == IntLattice[I2].top) then top
                else
                    val stringLength = s._1
                    var from2 = from.asInstanceOf[Int]
                    var to2 = to.asInstanceOf[Int]
                    var struct = top
                    if(from2 < stringLength && to2 < stringLength && from2 < to2) then
                        val substringLength = to2 - from2 + 1
                        struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
                        struct._1 = 1
                        struct._2 = malloc(substringLength.toULong + 1.toULong).asInstanceOf[CString]
                        getSubString(s._2, struct._2, from2, to2)
                    struct 

            def ref[I2: IntLattice, C2: CharLattice](s: S, i: I2): C2 =
                if(s == top) then CharLattice[C2].top
                else if(s == bottom) then CharLattice[C2].bottom
                else if(i == IntLattice[I2].top) then CharLattice[C2].top
                else if(i == IntLattice[I2].bottom) then CharLattice[C2].bottom
                else 
                    var c = 0
                    val stringLength = s._1
                    // this could probably be written better
                    if(i.asInstanceOf[Int] < stringLength) then
                        val charInCString = !(s._2 + c)
                        CharLattice[C2].inject(charInCString.toChar)
                    else
                        CharLattice[C2].bottom

            def set[I2: IntLattice, C2: CharLattice](
                s: S,
                i: I2,
                c: C2
              ): S =
                if(s == bottom) then bottom
                else if (IntLattice[I2].isBottom(i) || CharLattice[C2].isBottom(c)) then bottom
                else if(s == top) then top
                else if (i == IntLattice[I2].top || c == CharLattice[C2].top) then top
                else 

                    val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
                    val stringLength = s._1
                    struct._1 = stringLength
                    struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
                    strcpy(struct._2, s._2, s._1)
                    // This could probably be written better
                    !(struct._2 + i.asInstanceOf[Int]) = c.asInstanceOf[CChar]
                    struct

            def lt[B2 : BoolLattice](s1: S, s2: S): B2 = 
                if(s1 == bottom || s2 == bottom) then BoolLattice[B2].bottom
                else if(s1 == top || s2 == top) then BoolLattice[B2].top
                else
                    val cmp = strcmp(s1._2, s2._2)
                    BoolLattice[B2].inject(cmp < 0)

            def toSymbol[Sym2: SymbolLattice](s: S): Sym2 = 
                if(s == top) then SymbolLattice[Sym2].top
                else if(s == bottom) then SymbolLattice[Sym2].bottom
                else 
                    val scalaS = fromCString(s._2)
                    SymbolLattice[Sym2].inject(scalaS)

            def toNumber[I2: IntLattice](s: S) = 
                if(s == bottom) then MayFail.success(IntLattice[I2].bottom)
                else if(s == top) then MayFail.success(IntLattice[I2].top).addError(NotANumberString)
                else
                    val l =  atol(s._2)
                    if(l == 0 && !(s._2) != 0.toChar) then MayFail.failure(NotANumberString)
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
        implicit val symLL: AbstractBaseInstance[String, Sym] with SymbolLattice[Sym] = new AbstractBaseInstance[String, Sym]("Symbol") with SymbolLattice[Sym] {
            
            val top: Sym = 
                val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
                struct._1 = 3
                struct
            val bottom: Sym = 
                val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
                struct._1 = 2
                struct

            def inject(x: String) = 
                val struct = malloc(SnSize).asInstanceOf[S]
                val stringLength = x.length()
                struct._1 = stringLength
                struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
                val str : Array[Byte] = (x + 0.toChar).getBytes().nn
                var i = 0
                while(i <= stringLength) do
                    !(struct._2 + i) = str(i)
                    i = i +1
                //allocatedStrings += struct
                
                struct


            def toString[S2: StringLattice](s: Sym): S2 = 
                if(s == top) then StringLattice[S2].top
                else if(s == bottom) then StringLattice[S2].bottom
                else StringLattice[S2].inject(fromCString(s._2))

            override def show(x: Sym): String =
                if(x == top) then typeName
                else if(x == bottom) then s"$typeName.⊥"
                else fromCString(x._2)

        }
