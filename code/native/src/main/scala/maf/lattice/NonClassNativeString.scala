package maf.lattice

import scalanative.unsafe.*
import scala.scalanative.libc.stdlib.{atol, free, malloc}
import scala.collection.mutable
import scala.scalanative.libc.string.strcmp
import scalanative.unsigned.UnsignedRichInt
import scala.annotation.tailrec


object NonClassNativeString:
    // First field: length of the string when we have a constant. A useless number otherwise
    // second field: pointer to the memory containing the string
    // third field: dependency counter for the memory management
    type Sn_struct = CStruct3[CInt, CString, CChar]
    type S = Ptr[Sn_struct]

    var allocatedStrings : mutable.HashSet[S] = mutable.HashSet.empty

    val SnSize = sizeof[Sn_struct]
    private val ignoreTopLength = Int.MaxValue
    private val ignoreBottomLength = Int.MinValue

    def initializeMemory(): Unit =
        allocatedStrings = mutable.HashSet.empty

    def gc(): Unit =
        val garbage = allocatedStrings.filter(s => isGarbage(s))
        garbage.foreach(s =>
            deallocate(s)
            allocatedStrings -= s)


    def deallocateAllStrings(): Unit =
        allocatedStrings.foreach(s => deallocate(s))

        
    var top: S =
        val temp = makeNativeString("top")
        temp._1 = ignoreTopLength
        temp._3 = 1
        temp

    var bottom: S =
        val temp = makeNativeString("bottom")
        temp._1 = ignoreBottomLength
        temp._3 = 1
        temp

    def freeBounds() =
        free(top._2)
        free(top.asInstanceOf[Ptr[Byte]])
        free(bottom._2)
        free(bottom.asInstanceOf[Ptr[Byte]])

    def makeNativeString(x: String): S =
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
        allocatedStrings += struct
        struct

    def eq(s: S, other: S): Boolean =
        s == other

    
    def increaseDependencyCounter(s: S) =
        if(!(eq(s, top) || eq(s, bottom))) then 
            s._3 = (s._3 + 1).toByte

    def decreaseDependencyCounter(s: S) =
        val ctr = s._3
        if(ctr > 0) then
            if(!(eq(s, top) || eq(s, bottom))) then
                s._3 = (ctr - 1).toByte

    def isGarbage(s: S): Boolean =
        s._3 == 0

    def makeGarbage(s: S): Unit =
        s._3 = 0.toByte

    def deallocate(s: S): Unit =
        if(!(eq(s, top) || eq(s, bottom))) then
            free(s._2.asInstanceOf[Ptr[Byte]])
            free(s.asInstanceOf[Ptr[Byte]])

     // assuming lengths are equals
    @tailrec
    private def cstrEquals(s1: CString, s2: CString): Boolean =
        if(!s1 == 0.toByte) then true
        else if (!s1 != !s2) then false
        else cstrEquals(s1 + 1, s2 + 1)

    def equals(s: S, other: S): Boolean =
        val l1 = s._1
        val l2 = other._1
        val s1 = s._2
        val s2 = other._2
        eq(s, other) || (l1 == l2 && cstrEquals(s1, s2))

    // assuming sufficient memory is allocated at dest
    @tailrec
    private def strcpy(dest: CString, src: CString): Unit =
        if(!src == 0.toByte) then
            !dest == 0.toByte
        else
            !dest = !src
            strcpy(dest + 1, src + 1)


    // assuming sufficient memory is allcoated at dest
    @tailrec
    private def strcat(dest: CString, src: CString): Unit = 
        if(!src == 0.toByte) then
            !dest = 0.toByte
        else
            !dest= !src
            strcat(dest + 1, src + 1)

    private def getUnderlyingSubString(s: CString, sub: CString, from: Int, to: Int): Unit =
        @tailrec
        def buildSubString(s: CString, sub: CString, i: Int): Unit =
            if(!s == 0.toByte || i == to) then
                !sub = 0.toByte
            else
                !sub = !s
                buildSubString(s + 1, sub + 1, i + 1)
        buildSubString(s + from, sub, from)

    def substring(s: S, from: CInt, to: CInt): S =
        val stringLength = s._1
        if(from < stringLength && to < stringLength && from < to) then
            val substringLength = to - from
            val sub = malloc(sizeof[Sn_struct]).asInstanceOf[S]
            sub._1 = substringLength
            sub._2 = malloc(substringLength.toULong + 1.toULong).asInstanceOf[CString]
            sub._3 = 0
            getUnderlyingSubString(s._2, sub._2, from, to)
            allocatedStrings += sub
            sub
        else
            top

    def ref(s: S, i: CInt): CChar =
        val stringLength = s._1
        if(i < stringLength) then
            !(s._2 + i)
        else 
            0.toByte

    def ++(s: S, other: S): S =
        val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
        val stringLength = s._1 + other._1
        struct._1 = stringLength  
        struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
        struct._3 = 0
        strcpy(struct._2, s._2)
        strcat(struct._2 + s._1, other._2)
        allocatedStrings += struct
        struct

    def replace(s: S, i: CInt, c: CChar): S =
        val struct = malloc(sizeof[Sn_struct]).asInstanceOf[S]
        val stringLength = s._1
        struct._1 = stringLength
        struct._2 = malloc(stringLength.toULong + 1.toULong).asInstanceOf[CString]
        struct._3 = 0
        strcpy(struct._2, s._2)
        !(struct._2 + i.asInstanceOf[Int]) = c.asInstanceOf[CChar]

        allocatedStrings += struct
        struct

    def lt(s: S, other: S): Boolean =
        strcmp(s._2, other._2) < 0

    def toNumber(s: S): CLong =
        atol(s._2)

    def isEmpty(s: S): Boolean = 
        !(s._2) == 0.toChar

    def toChar(s: S): CChar =
        (!(s._2)).toByte

    def toString(s: S): String =
        fromCString(s._2)
    
