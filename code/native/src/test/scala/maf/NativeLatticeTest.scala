package maf.test.lattice

import maf.lattice.NativeLattice
import maf.lattice.NativeLattice._
import maf.lattice.NativeLattice.L._

class NativeBoolLattice extends BoolLatticeTest[NativeLattice.B](NativeBoolGenerator)
class NativeStringLattice extends StringLatticeTest[NativeLattice.S, NativeLattice.I](NativeStringGenerator)
class NativeIntLattice extends IntLatticeTest[NativeLattice.I, NativeLattice.B, NativeLattice.R, NativeLattice.S](NativeIntGenerator)
class NativeRealLattice extends RealLatticeTest[NativeLattice.R, NativeLattice.B, NativeLattice.I, NativeLattice.S](NativeRealGenerator)
class NativeCharLattice extends CharLatticeTest[NativeLattice.C](NativeCharGenerator)
class NativeSymLattice extends SymbolLatticeTest[NativeLattice.Sym](NativeSymGenerator)
