package maf.test.lattice
import maf.lattice.NativeLattice
import maf.lattice.NativeLattice._
import maf.lattice.AltNativeLattice._
import maf.lattice.AltNativeLattice

class NativeIntLattice 
    extends IntLatticeTest[NativeLattice.I, NativeLattice.B, NativeLattice.R, NativeLattice.S](NativeIntGenerator)
class NativeBoolLattice extends BoolLatticeTest[NativeLattice.B](NativeBoolGenerator)
class NativeStringLattice extends StringLatticeTest[NativeLattice.S, NativeLattice.I](NativeStringGenerator)
class NativeStringLattice2 extends StringLatticeTest[AltNativeLattice.S2, NativeLattice.I](NativeStringGenerator2)
class NativeRealLattice extends RealLatticeTest[NativeLattice.R, NativeLattice.B, NativeLattice.I, NativeLattice.S](NativeRealGenerator)
class NativeCharLattice extends CharLatticeTest[NativeLattice.C](NativeCharGenerator)
class NativeSymLattice extends SymbolLatticeTest[NativeLattice.Sym](NativeSymGenerator)
