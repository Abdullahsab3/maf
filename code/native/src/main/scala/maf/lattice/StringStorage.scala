package maf.lattice
import maf.lattice.NativeString.{bottom, top}

import scalanative.unsafe.*
import scala.scalanative.libc.stdlib.{atol, free, malloc}
import scala.scalanative.libc.string.strcmp
import scala.collection.mutable.ListBuffer
import scalanative.unsigned.UnsignedRichInt
import java.lang.annotation.Native
import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.ops.string


class StringStore:
    val SnSize = sizeof[Sn_struct]

    var size = 300

    var free = 0

    var allocatedStrings: Ptr[Sn_struct] = malloc(SnSize * size.toULong).asInstanceOf[Ptr[Sn_struct]]

    def gc(): Unit =
        val temp: Ptr[Sn_struct] = malloc(SnSize * size.toULong).asInstanceOf[Ptr[Sn_struct]]
        var tempfree = 0
        var i = 0
        while(i < free) do
            val sn: Sn_struct = !(allocatedStrings + i)
            if(sn._3 == 0) then
                println("not needed") // PAS AAN
            else
                !(temp + tempfree) = sn
                tempfree += 1
            i += 1



    def addStr(el: NativeString): Unit =
        if free > size - 1 then
            size = size * 2
            val temp: Ptr[Sn_struct] = malloc(SnSize * size.toULong).asInstanceOf[Ptr[Sn_struct]]


        !(allocatedStrings + free) = el.underlying
        free += 1






