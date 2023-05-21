package maf.test.lattice

import scala.scalanative.unsafe._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

@extern object boolLatticeOps:
    def boolIsTrue(b: CChar): CChar = extern
    def boolIsFalse(b: CChar): CChar = extern
    def boolNot(b: CChar): CChar = extern
    
    def boolJoin(a: CChar, b: CChar): CChar = extern
    def boolMeet(a: CChar, b: CChar): CChar = extern
    def boolSubsumes(a: CChar, b: CChar): CChar = extern

class BoolLatticeOpsTest extends AnyFlatSpec with should.Matchers {
    import boolLatticeOps._
    private def CChar2Boolean(i: CChar): Boolean =
        i == 1.toByte
    val bottom: CChar = 2.toByte
    val top: CChar = 4.toByte

    "boolIsTrue" should "check whether given element is true" in {
        assert(CChar2Boolean(boolIsTrue(1.toByte)))
        assert(!CChar2Boolean(boolIsTrue(0.toByte)))
        assert(CChar2Boolean(boolIsTrue(top)))
        assert(!CChar2Boolean(boolIsTrue(bottom)))
    }

    "boolIsFalse" should "check whether the given element false" in {
        assert(CChar2Boolean(boolIsFalse(top)))
        assert(!CChar2Boolean(boolIsFalse(bottom)))
        assert(!CChar2Boolean(boolIsFalse(1.toByte)))
        assert(CChar2Boolean(boolIsFalse(0.toByte)))
    }

    "boolNot" should "inverse a boolean" in {
        assert(boolNot(top) == top)
        assert(boolNot(bottom) == bottom)
        assert(boolNot(1) == 0)
        assert(boolNot(0) == 1)
    }

    "boolJoin" should "join elements" in {
        assert(boolJoin(top, 1) == top)
        assert(boolJoin(1, 1) == 1)
        assert(boolJoin(0, 1) == top)
        assert(boolJoin(1, 0) == top)
        assert(boolJoin(bottom, 1) == 1)
        assert(boolJoin(0, top) == top)
        assert(boolJoin(1, bottom) == 1)
        assert(boolJoin(top, top) == top)

    }

    "boolMeet" should "meet elements" in {
        assert(boolMeet(bottom, bottom) == bottom)
        assert(boolMeet(top, 1) == 1)
        assert(boolMeet(0, top) == 0)
        assert(boolMeet(1, bottom) == bottom)
        assert(boolMeet(1, 1) == 1)
        assert(boolMeet(top, top) == top)


    }

}
