package maf.lattice
import maf.lattice.NativeString.{bottom, top}

import scalanative.unsafe.*
import scala.scalanative.libc.stdlib.{atol, free, malloc}
import scala.scalanative.libc.string.strcmp
import scala.collection.mutable.ListBuffer
import scalanative.unsigned.UnsignedRichInt
import java.lang.annotation.Native
import scala.compiletime.ops.string


// First field: length of the string when we have a constant. A useless number otherwise
// second field: pointer to the memory containing the string
// third field: marked by the gc to survive the gc sweep
type Sn_struct = CStruct3[CInt, CString, CChar]
type S = Ptr[Sn_struct]

class NativeString(val underlying: S) extends AnyVal:

    def length = underlying._1

    def mark() =
        if(!(eq(top) || eq(bottom))) then 
            underlying._3 = 1

    def unmark() = 
        if(!(eq(top) || eq(bottom))) then
            underlying._3 = 0
    def isGarbage: Boolean =
        underlying._3 == 0

    def deallocate(): Unit =
        free(underlying._2)
        free(underlying.asInstanceOf[Ptr[Byte]])

    // assuming lengths are equals
    def cstrEquals(s1: CString, s2: CString): Boolean =
        if(!s1 == 0.toByte) then true
        else if (!s1 != !s2) then false
        else cstrEquals(s1 + 1, s2 + 1)

    def ==(other: NativeString): Boolean =
        val l1 = underlying._1
        val l2 = other.underlying._1
        val s1 = underlying._2
        val s2 = other.underlying._2
        eq(other) || (l1 == l2 && cstrEquals(s1, s2))

    def eq(other: NativeString): Boolean =
        this.underlying == other.underlying

    // assuming sufficient memory is allocated at dest
    private def strcpy(dest: CString, src: CString): Unit =
        if(!src == 0.toByte) then
            !dest == 0.toByte
        else
            !dest = !src
            strcpy(dest + 1, src + 1)


    // assuming sufficient memory is allcoated at dest
    private def strcat(dest: CString, src: CString, destlength: Int): Unit = 
        if(!src == 0.toByte) then
            !(dest + destlength) = 0.toByte
        else
            !(dest + destlength) = !src
            strcat(dest + 1, src + 1, destlength)

    private def getUnderlyingSubString(s: CString, sub: CString, from: Int, to: Int): Unit =
        var i = 0
        while(i < to) do
            !(sub + i) = !(s + from + i - 1)
        !(sub + i) = 0.toByte // 0-terminated string in C


    def substring(from: CInt, to: CInt): NativeString =
        val stringLength = underlying._1
        if(from < stringLength && to < stringLength && from < to) then
            val substringLength = to - from + 1
            val s = malloc(sizeof[Sn_struct]).asInstanceOf[S]
            s._1 = 1
            s._2 = malloc(substringLength.toULong + 1.toULong).asInstanceOf[CString]
            s._3 = 0
            getUnderlyingSubString(underlying._2, s._2, from, to)
            val ns = new NativeString(s)
            NativeString.allocatedStrings += ns
            ns
        else
            NativeString.top

    def ref(i: CInt): CChar = 
        var c = 0
        val stringLength = underlying._1
        if(i < stringLength) then
            !(underlying._2 + c)
        else 
            0.toByte
        
    def ++(other: NativeString): NativeString =
        val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
        val stringLength = this.length + other.length
        struct._1 = stringLength  
        struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
        struct._3 = 0
        strcpy(struct._2, underlying._2)
        strcat(struct._2, other.underlying._2, underlying._1)
        val ns = new NativeString(struct)
        NativeString.allocatedStrings += ns
        ns

    def replace(i: CInt, c: CChar): NativeString =
        val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
        val stringLength = underlying._1
        struct._1 = stringLength
        struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
        strcpy(struct._2, underlying._2)
        !(struct._2 + i.asInstanceOf[Int]) = c.asInstanceOf[CChar]
        val ns = new NativeString(struct)
        NativeString.allocatedStrings += ns
        ns


    def lt(other: NativeString): Boolean =
        strcmp(underlying._2, other.underlying._2) < 0

    def toNumber: CLong =
        atol(underlying._2)

    def isEmpty: Boolean = 
        !(underlying._2) == 0.toChar

    override def toString(): String =
        fromCString(underlying._2)

object NativeString:
    var allocatedStrings : ListBuffer[NativeString] = ListBuffer.empty

    val SnSize = sizeof[Sn_struct]

    // TODO: maybe change length to make sure these do not get compared with actual "top" and "bottom"
    private val ignore = 20
    val top =
        val temp = NativeString("top")
        temp.underlying._1 = ignore
        temp.underlying._3 = 1
        temp
    val bottom =
        val temp = NativeString("bottom")
        temp.underlying._1 = ignore
        temp.underlying._3 = 1
        temp

    def apply(x: String): NativeString =
        val struct = malloc(SnSize).asInstanceOf[S]
        val stringLength = x.length()
        struct._1 = stringLength
        struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
        struct._3 = 0
        var i = 0
        while(i < stringLength) do
            !(struct._2 + i) = x(i).toByte
            i = i +1
        !(struct._2 + stringLength) = 0.toByte
        val ns = new NativeString(struct)
        allocatedStrings += ns
        ns  


    def prepareNextGCIteration(): Unit =
        allocatedStrings.foreach(_.unmark())

    def gc(): Unit =
        val garbage = allocatedStrings.filter(_.isGarbage)
        garbage.foreach(s =>
            allocatedStrings -= s
            s.deallocate())
        prepareNextGCIteration()

    def deallocateExcept(stay: ListBuffer[NativeString]): Unit =
        val toStay = stay ++ ListBuffer(top, bottom)
        (allocatedStrings diff toStay).foreach(_.deallocate())
        allocatedStrings = toStay

    def deallocateAllStrings(): Unit =
        allocatedStrings.foreach(_.deallocate())
        allocatedStrings = ListBuffer.empty
