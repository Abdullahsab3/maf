package maf.test.lattice

import maf.lattice.NativeString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable.ListBuffer
import scalanative.unsafe.*
class NativeStringTest extends AnyFlatSpec with should.Matchers {

    val Scalaa = "a word"
    val Scalab = "another word"
    val Scalaempty = ""
    val aNS: NativeString = NativeString(Scalaa)
    val a2NS: NativeString = NativeString(Scalaa)
    val bNS: NativeString = NativeString(Scalab)
    val emptyNS: NativeString= NativeString(Scalaempty)
    val n = "98"
    val nNS = NativeString(n)

    "NativeString" should "create a new NativeString using a Scala String" in {
        val ScalaString = "Scala"
        val a: NativeString = NativeString(ScalaString)
        assert(fromCString(a.underlying._2) == ScalaString)
        assert(a.length == ScalaString.length)
    }

    "length" should "return the length of the string"  in {
        assert(aNS.length == Scalaa.length)
        assert(bNS.length == Scalab.length)
        assert(emptyNS.length == Scalaempty.length)
    }

    "==" should "compare two NativeStrings using structural equality, i.e. same characters and length" in {
        assert(aNS == a2NS)
        assert(!(aNS == bNS))
        assert(!(bNS == emptyNS))
    }

    "eq" should "compare two NativeString using referential equality" in  {
        val _a = aNS
        assert(_a.eq(aNS))
        assert(!(aNS.eq(bNS)))
    }

    "substring" should "return the substring given the start and end" in  {
        val substr = bNS.substring(2, 9)
        val scalaSubstr = NativeString(Scalab.substring(2, 9).nn)
        assert(substr.length == scalaSubstr.length)
        println(substr)
        println(substr)
        assert(substr == scalaSubstr)
    }

    "ref" should "return the character at a given index" in  {
        val r = aNS.ref(3)
        assert(r == Scalaa(3).toByte)
    }

    "++" should "append two strings together" in {
        val aplusb = aNS ++ bNS
        assert(aplusb == NativeString(Scalaa ++ Scalab))

        val aplusempty = aNS ++ emptyNS
        assert(aplusempty == NativeString(Scalaa ++ ""))

        val emptyplusa = emptyNS ++ aNS
        assert(emptyplusa == NativeString("" ++ Scalaa))

        val aplusa = aNS ++ aNS
        assert(aplusa == NativeString(Scalaa ++ Scalaa))
    }

    "replace" should "produce a NativeString with a new c in an index i" in  {
        val repa = aNS.replace(3, 'a'.toByte)
        assert(repa == NativeString("a ward"))
    }

    "lt" should "compare two Nativestrings lexicographically" in  {
        assert(aNS.lt(bNS) == Scalaa < Scalab)
    }

    "toNumber" should "convert NativeString to number" in  {
        assert(nNS.toNumber == n.toInt)
    }

    "isEmpty" should "check if a string is empty" in  {
        assert(emptyNS.isEmpty)
        assert(!(aNS.isEmpty))
    }

    "toString" should "return a scala string from a NativeString" in  {
        assert(aNS.toString() == Scalaa)
    }



}
