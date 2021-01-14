package maf.lattice

import maf.lattice.interfaces.IntLattice

import scala.util.Random

/** Various implementations of mathematical utilities functions. */
object MathOps {
  def ceil(a: Double): Double = scala.math.ceil(a)

  /**
   * Modulo in Scheme and Scala are different.
   * This implements the same behavior as Scheme's modulo
   */
  def modulo(n1: BigInt, n2: BigInt): BigInt =
    if (n1.signum * n2.signum < 0) {
      /* different sign, behaviour not the same between Scheme and Scala, adjust it */
      (n2.abs - n1.abs % n2.abs) % n2.abs * (if (n2 < 0) -1 else 1)
    } else {
      /* same sign, same behaviour */
      n1 % n2
    }

  /** Remainder in Scheme has the same behavior of Scala's modulo. */
  def remainder(n1: BigInt, n2: BigInt): BigInt = n1 % n2

  def random(n: BigInt): BigInt = (BigInt(n.bitLength, new Random()) % n).abs

  def random(n: Double): Double = scala.math.abs(scala.util.Random.nextDouble() % n)

  /**
   * Round in Scheme and Scala are different.
   * This implements the same behaviour as Scheme's round.
   */
  def round(n: Double): Double = {
    val frac = n % 1 /* Fractional part of n */
    /* In the case of a fraction part equaling 0.5, rounding is done towards the even number. */
    if ((scala.math.abs(frac) == 0.5) && (((n > 0) && ((scala.math.abs(n - frac) % 2) == 0)) || ((n < 0) && (((n - frac) % 2) == -1)))) {
      scala.math.round(n).toDouble - 1
    } else {
      scala.math.round(n).toDouble
    }
  }

  /** Used to convert big integers to doubles, but warns if the value is too big/small. */
  implicit def bigIntToDouble(i: BigInt): Double = if (i.isValidDouble) i.doubleValue else throw new Exception(s"Illegal conversion of $i to double.")

  def bigIntToInt(i: BigInt): Int = if (i.isValidInt) i.intValue else throw new Exception(s"Illegal conversion of $i to int.")

  def bigIntFromString(s: String): Option[BigInt] =
    try Some(BigInt(s, 10)) // May throw a NumberFormatException.
    catch {
      case _: Throwable => None
    }
}
