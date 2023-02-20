package maf.test.lattice

import maf.lattice.NativeLattice
import maf.lattice.NativeLattice.L.boolCP

class NativeBoolLattice extends BoolLatticeTest[NativeLattice.B](NativeBoolGenerator)
