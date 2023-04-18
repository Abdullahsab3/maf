package maf.lattice
import scalanative.unsafe._
import scala.scalanative.libc.stdlib.{malloc, free, atol}
import scala.scalanative.libc.string.strcmp

import scala.collection.mutable.ListBuffer
import scalanative.unsigned.UnsignedRichInt
import java.lang.annotation.Native
import scala.compiletime.ops.string


// First field: length of the string when we have a constant. A useless number otherwise
// second field: pointer to the memory containing the string
type Sn_struct = CStruct2[CInt, CString]
type S = Ptr[Sn_struct]

class NativeString(val underlying: S) extends AnyVal:

    def length = underlying._1

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
        equals(other) || (l1 == l2 && cstrEquals(s1, s2))

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
        strcpy(struct._2, underlying._2)
        strcat(struct._2, other.underlying._2, other.underlying._1 - 1)
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

    val top = NativeString("top")
    val bottom = NativeString("bottom")

    def apply(x: String): NativeString =
        val struct = malloc(SnSize).asInstanceOf[S]
        val stringLength = x.length()
        struct._1 = stringLength
        struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
        var i = 0
        while(i < stringLength) do
            !(struct._2 + i) = x(i).toByte
            i = i +1
        !(struct._2 + stringLength) = 0.toByte
        val ns = new NativeString(struct)
        allocatedStrings += ns
        ns  


    def deallocateAllStrings() = 

        allocatedStrings.foreach(_.deallocate())
        allocatedStrings = ListBuffer.empty
