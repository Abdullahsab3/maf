package maf.test.lattice

import org.scalacheck._
import maf.core.Lattice

import maf.lattice.NativeLattice._
import maf.lattice.NativeLattice
import maf.lattice.NativeLattice._
import maf.lattice.AltNativeLattice._
import maf.lattice.AbstractBaseInstance
import maf.lattice.AltNativeLattice

object NativeBoolGenerator extends BooleanGenerator[NativeLattice.B]

abstract class NativeGenerator[E, X](gen: Gen[E])(implicit lat: AbstractBaseInstance[E, X])
    extends LatticeGenerator[X]:
    def constgen: Gen[X] = for x <- gen yield lat.inject(x)
    def botgen: Gen[X] = lat.bottom
    def topgen: Gen[X] = lat.top
    def any: Gen[X] = Gen.oneOf(constgen, botgen, topgen)
    def le(l: X) = if l == lat.top then { any }
    else if l == lat.bottom then { botgen }
    else { Gen.oneOf(l, lat.bottom) }

object NativeStringGenerator extends NativeGenerator[String, NativeLattice.S](Generators.str)(NativeLattice.StringLL)
object NativeStringGenerator2 extends NativeGenerator[String, AltNativeLattice.S2](Generators.str)(StringLL2)
object NativeIntGenerator extends NativeGenerator[BigInt, NativeLattice.I](Generators.int)(intLL)
object NativeRealGenerator extends NativeGenerator[Double, NativeLattice.R](Generators.double)(realLL)
object NativeCharGenerator extends NativeGenerator[Char, NativeLattice.C](Generators.char)(charLL)
object NativeSymGenerator extends NativeGenerator[String, NativeLattice.Sym](Generators.sym)(NativeLattice.symLL)
object NativeSymGenerator2 extends NativeGenerator[String, AltNativeLattice.Sym2](Generators.sym)(symLL2)
