package maf.lattice

import maf.util._
import maf.core._
import maf.lattice.interfaces._
import NumOps._
import scala.scalanative.libc.stdlib.{malloc, free}
import scala.scalanative.libc.string.{strlen, strcpy, strcat}
import scalanative.unsigned.UnsignedRichInt

import scalanative.unsafe._
import scala.scalanative.unsigned.UByte

// Low-level implementation of the constant propagation implemenation.
// prefixed with ll == low-level implementation

// Boollattice 
@extern object boolLatticeOps:
    def boolIsTrue(b: CInt): CInt = extern
    def boolIsFalse(b: CInt): CInt = extern
    def boolNot(b: CInt): CInt = extern
    
    def boolJoin(a: CInt, b: CInt): CInt = extern
    def boolMeet(a: CInt, b: CInt): CInt = extern
    def boolSubsumes(a: CInt, b: CInt): CInt = extern


object NativeLattice:

    sealed trait L[+A]

    case object Top extends L[Nothing]

    case class Constant[A](x: A) extends L[A]

    case object Bottom extends L[Nothing]
      
    abstract class AbstractBaseInstance[T](val typeName: String) extends Lattice[T]:
        def show(x: T): String
        val bottom: T
        val top: T
        def join(x: T, y: => T): T
        def meet(x: T, y: => T): T
        def subsumes(x: T, y: => T): Boolean
        def eql[B2: BoolLattice](n1: T, n2: T): B2
    
    abstract class BaseInstance[A: Show](typeName: String) extends AbstractBaseInstance[L[A]](typeName):
        def show(x: L[A]): String = x match
            case Top         => typeName
            case Constant(x) => x.toString
            case Bottom      => s"$typeName.⊥"
        val bottom: L[A] = Bottom
        val top: L[A] = Top
        
        def join(x: L[A], y: => L[A]): L[A] = x match
            case Top => Top
            case Constant(_) =>
                y match
                    case Top => Top
                    case Constant(_) =>
                        if x == y then x
                        else Top
                    case Bottom => x
            case Bottom => y
            
        def meet(x: L[A], y: => L[A]): L[A] = x match
            case Bottom => Bottom
            case Constant(_) =>
                y match
                    case Top => x
                    case Constant(_) =>
                        if x == y then x
                        else Bottom
                    case Bottom => Bottom
            case Top => y


        def subsumes(x: L[A], y: => L[A]): Boolean = x match
            case Top => true
            case Constant(_) =>
                y match
                    case Top         => false
                    case Constant(_) => x == y
                    case Bottom      => true
            case Bottom =>
                y match
                    case Top         => false
                    case Constant(_) => false
                    case Bottom      => true

        def eql[B2: BoolLattice](n1: L[A], n2: L[A]): B2 = (n1, n2) match
            case (Top, Top)                 => BoolLattice[B2].top
            case (Top, Constant(_))         => BoolLattice[B2].top
            case (Constant(_), Top)         => BoolLattice[B2].top
            case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x == y)
            case (Bottom, _)                => BoolLattice[B2].bottom
            case (_, Bottom)                => BoolLattice[B2].bottom

    // First field: top (3), bottom (2), constant(1)
    // second field: pointer to the memory containing the string
    type Sn_struct = CStruct2[CInt, CString]
    type Sn = Ptr[Sn_struct]

    type B = CInt
    type S = L[String]
    type I = L[BigInt]
    type R = L[Double]
    type C = L[Char]
    type Sym = L[String]

    object L:
        def CInt2Boolean(i: B): Boolean = 
            i == 1
        implicit val boolCP: BoolLattice[B] = new AbstractBaseInstance[B]("Bool") with BoolLattice[B] {
            val top = 3
            val bottom = 2
            def inject(b: Boolean): B = if b then 1 else 0
            def isTrue(b: B): Boolean = 
                import boolLatticeOps.boolIsTrue
                val result = boolIsTrue(b)
                CInt2Boolean(result)
            def isFalse(b: B): Boolean =
                import boolLatticeOps.boolIsFalse
                val result = boolIsFalse(b)
                CInt2Boolean(result)
            def not(b: B): B =
                import boolLatticeOps.boolNot
                boolNot(b) 
            def join(x: B, y: => B): B =
                import boolLatticeOps.boolJoin
                boolJoin(x, y)
            def meet(x: B, y: => B): B =
                import boolLatticeOps.boolMeet
                boolMeet(x, y)
            def subsumes(x: B, y: => B): Boolean =
                import boolLatticeOps.boolSubsumes
                val result = boolSubsumes(x, y)
                CInt2Boolean(result)
            def show(x: B): String =
                if(x == top) then typeName
                else if(x == bottom) then s"$typeName.⊥"
                else CInt2Boolean(x).toString

            def eql[B2: BoolLattice](n1: B, n2: B): B2 =
                if (n1 == bottom || n2 == bottom) then BoolLattice[B2].bottom
                else if (n1 == top || n2 == top) then BoolLattice[B2].top
                else BoolLattice[B2].inject(n1 == n2)
        }

        // Ptr[CStruct2[CInt, CString]]

         implicit val nativeStringCP: StringLattice[Sn] = new AbstractBaseInstance[Sn]("Str") with StringLattice[Sn] {
            
            
            val top: Sn = 
                val struct = malloc(sizeof[Sn_struct]).asInstanceOf[Sn]
                struct._1 = 3
                struct
            val bottom: Sn = 
                val struct = malloc(sizeof[Sn_struct]).asInstanceOf[Sn]
                struct._1 = 2
                struct
            
            def inject(x: String): Sn =
                val struct = malloc(sizeof[Sn_struct]).asInstanceOf[Sn]
                /**
                  * TODO: Deze variabele kan je gebruiken om de lengte van de string bij te houden!
                  * Op die manier moet je niet telkens opnieuw strlenen
                  */
                val stringLength = x.length()
                struct._1 = stringLength
                /**
                  * TODO: dit kan zeker beter:
                    In plaats van de ingebouwde toCString te gebruiken,
                    zet de string om naar bytes; e.g.: (str + 0.toChar).getBytes().at(0)
                  */
                Zone {implicit z => 
                    val Cx: CString = toCString(x)
                    struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
                    strcpy( struct._2, Cx)
                }
                struct
            
            def length[I2: IntLattice](s: Sn): I2 = 
                if(s == top) then IntLattice[I2].top
                else if (s == bottom) then IntLattice[I2].bottom
                else IntLattice[I2].inject(s._1)

            def append(s1: Sn, s2: Sn): Sn = 
                if(s1 == top || s2 == top) then top
                else if(s1 == bottom || s2 == bottom) then bottom
                else 
                    val struct = malloc(sizeof[Sn_struct]).asInstanceOf[Sn]
                    val stringLength = s1._1 + s2._1
                    struct._1 = stringLength
                    struct._2 = malloc(stringLength.toULong).asInstanceOf[CString]
                    strcpy(struct._2, s1._2)
                    strcat(struct._2, s2._2)
                    struct
            
            private def getSubString(s: CString, sub: CString, from: Int, to: Int): Unit =
                var i = 0
                while(i < to) do
                    !(sub + i) = !(s + from + i - 1)
                
                !(sub + i) = 0.toByte // 0-terminated string in C
                
            def substring[I2: IntLattice](s: Sn, from: I2, to: I2): Sn =
                if(s == top) then top
                else if(s == bottom) then bottom
                else if(IntLattice[I2].isBottom(from) || IntLattice[I2].isBottom(to)) then bottom
                else
                    val stringLength = s._1
                    var i = 0
                    var from2 = -1
                    var to2 = -1
                    var struct = top

                    // whiles are more optimized in SN than for loops
                    while(i < stringLength) do
                        if BoolLattice[B].isTrue(IntLattice[I2].eql[B](from, IntLattice[I2].inject(i))) then
                            from2 = i
                        if BoolLattice[B].isTrue(IntLattice[I2].eql[B](to, IntLattice[I2].inject(i))) then
                            to2 = i
                        if(from2 >= 0 && to2 >= 0) then
                            val substringLength = to2 - from2 + 1
                            struct = malloc(sizeof[Sn_struct]).asInstanceOf[Sn]
                            struct._1 = 1
                            struct._2 = malloc(substringLength.toULong).asInstanceOf[CString]
                            getSubString(s._2, struct._2, from2, to2)
                            // breaks dont exist? no problem :p
                            i = stringLength
                        i = i + 1
                    struct      

            def ref[I2: IntLattice, C2: CharLattice](s: Sn, i: I2): C2 =
                if(s == top) then CharLattice[C2].top
                else if(s == bottom) then CharLattice[C2].bottom
                else 
                    var c = 0
                    val stringLength = s._1
                    var charref = CharLattice[C2].bottom
                    /**
                      * TODO: die while loops kunnen misschien anders
                      * we zijn minder geinteresseerd in genericiteit,
                      * dus mss generciteit verliezen ten koste van een betere performantie
                      * en in plaats van te zoeken naar c, kunnen we mss gwn checken of het top/bottom/constant 
                      * is in onze implementatie van de int lattice
                      */
                    while(c < stringLength) do
                        if BoolLattice[B].isTrue(IntLattice[I2].eql[B](i, IntLattice[I2].inject(c))) && c < stringLength then
                            val charInCString = !(s._2 + c)
                            charref = CharLattice[C2].inject(charInCString.toChar)
                            c = stringLength
                        c = c + 1
                    charref







            

            

        } 
        implicit val stringCP: StringLattice[S] = new BaseInstance[String]("Str") with StringLattice[S] {
            def inject(x: String): S = Constant(x)
            def length[I2: IntLattice](s: S): I2 = s match
                case Top         => IntLattice[I2].top
                case Constant(s) => IntLattice[I2].inject(s.length)
                case Bottom      => IntLattice[I2].bottom
            def append(s1: S, s2: S): S = (s1, s2) match
                case (Bottom, _) | (_, Bottom)  => Bottom
                case (Top, _) | (_, Top)        => Top
                case (Constant(x), Constant(y)) => Constant(x ++ y)
            def substring[I2: IntLattice](
                s: S,
                from: I2,
                to: I2
              ): S = (s, from, to) match
                case (Bottom, _, _)                                => Bottom
                case (_, from, _) if IntLattice[I2].isBottom(from) => Bottom
                case (_, _, to) if IntLattice[I2].isBottom(to)     => Bottom
                case (Top, _, _)                                   => Top
                case (Constant(s), from, to)                       =>
                    // This is duplicated code from ConcreteLattice, it should be refactored
                    (0.to(s.size)
                        .collect({
                            case from2 if BoolLattice[B].isTrue(IntLattice[I2].eql[B](from, IntLattice[I2].inject(from2))) =>
                                (from2
                                    .to(s.size)
                                    .collect({
                                        case to2 if BoolLattice[B].isTrue(IntLattice[I2].eql[B](to, IntLattice[I2].inject(to2))) =>
                                            inject(s.substring(from2, to2).nn)
                                    }))
                        })
                        .flatten)
                        .foldLeft(bottom)((s1, s2) => join(s1, s2))
            def ref[I2: IntLattice, C2: CharLattice](s: S, i: I2): C2 = s match
                case Bottom      => CharLattice[C2].bottom
                case Top         => CharLattice[C2].top
                case Constant(x) =>
                    // This is duplicated code from ConcreteLattice, it should be refactored
                    0.to(x.length)
                        .collect({
                            case i2
                                if BoolLattice[Concrete.B]
                                    .isTrue(IntLattice[I2].eql[Concrete.B](i, IntLattice[I2].inject(i2))) &&
                                    i2 < x.size =>
                                CharLattice[C2].inject(x.charAt(i2))
                        })
                        .foldLeft(CharLattice[C2].bottom)((c1, c2) => CharLattice[C2].join(c1, c2))
            def set[I2: IntLattice, C2: CharLattice](
                s: S,
                i: I2,
                c: C2
              ): S = s match
                case Bottom                                                         => Bottom
                case _ if IntLattice[I2].isBottom(i) || CharLattice[C2].isBottom(c) => Bottom
                case Top                                                            => Top
                case Constant(str) =>
                    (i, c) match
                        case (Constant(idx: BigInt), Constant(chr: Char)) => Constant(str.updated(idx.toInt, chr))
                        // If neither the index or character are constant, don't even bother
                        case _ => Top
            def lt[B2: BoolLattice](s1: S, s2: S): B2 = (s1, s2) match
                case (Bottom, _) | (_, Bottom)  => BoolLattice[B2].bottom
                case (Top, _) | (_, Top)        => BoolLattice[B2].top
                case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x < y)
            def toSymbol[Sym2: SymbolLattice](s: S): Sym2 = s match
                case Bottom      => SymbolLattice[Sym2].bottom
                case Top         => SymbolLattice[Sym2].top
                case Constant(x) => SymbolLattice[Sym2].inject(x)

            def toNumber[I2: IntLattice](s: S) = s match
                case Bottom        => MayFail.success(IntLattice[I2].bottom)
                case Constant(str) => MayFail.fromOption(NumOps.bigIntFromString(str).map(IntLattice[I2].inject))(NotANumberString)
                case Top           => MayFail.success(IntLattice[I2].top).addError(NotANumberString)
        }

        implicit val intCP: IntLattice[I] = new BaseInstance[BigInt]("Int") with IntLattice[I] {
            def inject(x: BigInt): I = Constant(x)

            def toReal[R2: RealLattice](n: I): R2 = n match
                case Top         => RealLattice[R2].top
                case Constant(x) => RealLattice[R2].inject(x.toDouble)
                case Bottom      => RealLattice[R2].bottom

            def random(n: I): I = n match
                case Bottom => Bottom
                case _      => Top

            private def binop(
                op: (BigInt, BigInt) => BigInt,
                n1: I,
                n2: I
              ) = (n1, n2) match
                case (Top, Top)                 => Top
                case (Top, Constant(_))         => Top
                case (Constant(_), Top)         => Top
                case (Constant(x), Constant(y)) => Constant(op(x, y))
                case _                          => Bottom
            def plus(n1: I, n2: I): I = binop(_ + _, n1, n2)
            def minus(n1: I, n2: I): I = binop(_ - _, n1, n2)
            def times(n1: I, n2: I): I = binop(_ * _, n1, n2)
            def div[F: RealLattice](n1: I, n2: I): F = (n1, n2) match
                case (Top, _) | (_, Top)                  => RealLattice[F].top
                case (Constant(x), Constant(y)) if y != 0 => RealLattice[F].inject(bigIntToDouble(x) / bigIntToDouble(y))
                // TODO: use MayFail here for when divide-by-zero occurs ...
                case _ => RealLattice[F].bottom
            def expt(n1: I, n2: I): I = binop((x, y) => Math.pow(x.toDouble, y.toDouble).toInt, n1, n2)
            def quotient(n1: I, n2: I): I = binop(_ / _, n1, n2)
            def modulo(n1: I, n2: I): I = (n1, n2) match
                case (Top, Top)                           => Top
                case (Top, Constant(_))                   => Top
                case (Constant(_), Top)                   => Top
                case (Constant(x), Constant(y)) if y != 0 => Constant(MathOps.modulo(x, y))
                case _                                    => Bottom
            def remainder(n1: I, n2: I): I = (n1, n2) match
                case (Top, Top)                           => Top
                case (Top, Constant(_))                   => Top
                case (Constant(_), Top)                   => Top
                case (Constant(x), Constant(y)) if y != 0 => Constant(MathOps.remainder(x, y))
                case _                                    => Bottom
            def lt[B2: BoolLattice](n1: I, n2: I): B2 = (n1, n2) match
                case (Top, Top)                 => BoolLattice[B2].top
                case (Top, Constant(_))         => BoolLattice[B2].top
                case (Constant(_), Top)         => BoolLattice[B2].top
                case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x < y)
                case _                          => BoolLattice[B2].bottom
            def valuesBetween(n1: I, n2: I): Set[I] = (n1, n2) match
                case (Top, _)                   => Set(Top)
                case (_, Top)                   => Set(Top)
                case (Constant(x), Constant(y)) => x.to(y).map(i => Constant(i)).toSet
                case _                          => Set()
            def makeString[C2: CharLattice, S2: StringLattice](length: I, char: C2): S2 = (length, char) match
                case (Bottom, _)                               => StringLattice[S2].bottom
                case (_, bot) if bot == CharLattice[C2].bottom => StringLattice[S2].bottom
                case (Top, _)                                  => StringLattice[S2].top
                case (Constant(n), _) =>
                    val c = CharLattice[C2].toString[S2](char)
                    1.to(NumOps.bigIntToInt(n)).foldLeft(StringLattice[S2].inject(""))((s, _) => StringLattice[S2].append(s, c))

            def toString[S2: StringLattice](n: I): S2 = n match
                case Top         => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x.toString)
                case Bottom      => StringLattice[S2].bottom
            def toChar[C2: CharLattice](n: I): C2 = n match
                case Top         => CharLattice[C2].top
                case Constant(x) => CharLattice[C2].inject(NumOps.bigIntToInt(x).asInstanceOf[Char])
                case Bottom      => CharLattice[C2].bottom
        }

        implicit val realCP: RealLattice[R] = new BaseInstance[Double]("Real") with RealLattice[R] {
            def inject(x: Double) = Constant(x)
            def toInt[I2: IntLattice](n: R): I2 = n match
                case Top         => IntLattice[I2].top
                case Constant(x) => IntLattice[I2].inject(x.toInt)
                case Bottom      => IntLattice[I2].bottom
            def ceiling(n: R): R = n match
                case Constant(x) => Constant(x.ceil)
                case _           => n
            def floor(n: R): R = n match
                case Constant(x) => Constant(x.floor)
                case _           => n
            def round(n: R): R = n match
                case Constant(x) => Constant(MathOps.round(x))
                case _           => n
            def random(n: R): R = n match
                case Constant(_) => Top
                case _           => n
            def log(n: R): R = n match // Todo: use MayFail here or support imaginary numbers.
                case Constant(x) if 0 <= x => Constant(scala.math.log(x))
                case Top                   => Top
                case _                     => Bottom
            def sin(n: R): R = n match
                case Constant(x) => Constant(scala.math.sin(x))
                case _           => n
            def asin(n: R): R = n match // TODO: use MayFail here for when x out of bounds
                case Constant(x) if -1 <= x && x <= 1 => Constant(scala.math.asin(x))
                case Top                              => Top
                case _                                => Bottom
            def cos(n: R): R = n match
                case Constant(x) => Constant(scala.math.cos(x))
                case _           => n
            def acos(n: R): R = n match // TODO: use MayFail here for when x out of bounds
                case Constant(x) if -1 <= x && x <= 1 => Constant(scala.math.acos(x))
                case Top                              => Top
                case _                                => Bottom
            def tan(n: R): R = n match // TODO: use MayFail here for when x out of bounds
                case Constant(x) =>
                    scala.math.tan(x) match
                        case Double.NaN => Bottom
                        case n          => Constant(n)
                case _ => n
            def atan(n: R): R = n match
                case Constant(x) => Constant(scala.math.atan(x))
                case _           => n
            def sqrt(n: R): R = n match // Todo: use MayFail here or support imaginary numbers.
                case Constant(x) if 0 <= x => Constant(scala.math.sqrt(x))
                case Top                   => Top
                case _                     => Bottom
            private def binop(
                op: (Double, Double) => Double,
                n1: R,
                n2: R
              ) = (n1, n2) match
                case (Top, Top)                 => Top
                case (Top, Constant(_))         => Top
                case (Constant(_), Top)         => Top
                case (Constant(x), Constant(y)) => Constant(op(x, y))
                case _                          => Bottom
            def plus(n1: R, n2: R): R = binop(_ + _, n1, n2)
            def minus(n1: R, n2: R): R = binop(_ - _, n1, n2)
            def times(n1: R, n2: R): R = binop(_ * _, n1, n2)
            def div(n1: R, n2: R): R = binop(_ / _, n1, n2)
            def expt(n1: R, n2: R): R = binop((x, y) => Math.pow(x, y), n1, n2)
            def lt[B2: BoolLattice](n1: R, n2: R): B2 = (n1, n2) match
                case (Top, Top)                 => BoolLattice[B2].top
                case (Top, Constant(_))         => BoolLattice[B2].top
                case (Constant(_), Top)         => BoolLattice[B2].top
                case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x < y)
                case _                          => BoolLattice[B2].bottom
            def toString[S2: StringLattice](n: R): S2 = n match
                case Top         => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x.toString)
                case Bottom      => StringLattice[S2].bottom
        }

        implicit val charCP: CharLattice[C] = new BaseInstance[Char]("Char") with CharLattice[C] {
            def inject(x: Char) = Constant(x)
            def downCase(c: C): C = c match
                case Constant(char) => Constant(char.toLower)
                case _              => c
            def upCase(c: C): C = c match
                case Constant(char) => Constant(char.toUpper)
                case _              => c
            def toString[S2: StringLattice](c: C): S2 = c match
                case Top            => StringLattice[S2].top
                case Constant(char) => StringLattice[S2].inject(char.toString)
                case Bottom         => StringLattice[S2].bottom
            def toInt[I2: IntLattice](c: C): I2 = c match
                case Bottom      => IntLattice[I2].bottom
                case Constant(c) => IntLattice[I2].inject(c.toInt)
                case Top         => IntLattice[I2].top
            def isLower[B2: BoolLattice](c: C): B2 = c match
                case Bottom         => BoolLattice[B2].bottom
                case Constant(char) => BoolLattice[B2].inject(char.isLower)
                case Top            => BoolLattice[B2].top
            def isUpper[B2: BoolLattice](c: C): B2 = c match
                case Bottom         => BoolLattice[B2].bottom
                case Constant(char) => BoolLattice[B2].inject(char.isUpper)
                case Top            => BoolLattice[B2].top
            override def charEq[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom)    => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) => BoolLattice[B2].inject(c1 == c2)
                case _                            => BoolLattice[B2].top
            override def charLt[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom)    => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) => BoolLattice[B2].inject(c1 < c2)
                case _                            => BoolLattice[B2].top
            override def charEqCI[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom) => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) =>
                    BoolLattice[B2].inject(c1.toUpper == c2.toUpper) // TODO implement better (see note in concrete lattice)
                case _ => BoolLattice[B2].top
            override def charLtCI[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom) => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) =>
                    BoolLattice[B2].inject(c1.toUpper < c2.toUpper) // TODO implement better (see note in concrete lattice)
                case _ => BoolLattice[B2].top
        }

        implicit val symCP: SymbolLattice[Sym] = new BaseInstance[String]("Symbol")(Show.symShow) with SymbolLattice[Sym] {
            def inject(x: String) = Constant(x)
            def toString[S2: StringLattice](s: Sym): S2 = s match
                case Top         => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x)
                case Bottom      => StringLattice[S2].bottom
        }
