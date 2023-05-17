package maf.modular.scheme

import maf.lattice.NativeLattice
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.language.scheme.primitives._
import maf.lattice.NativeLattice.{boolLL, intLL, realLL, charLL}
import maf.lattice.AltNativeLattice.{StringLL, symLL}
import maf.lattice.AltNativeLattice

object NonClassNativeSchemeDomain extends ModularSchemeLatticeWrapper:
    type S = AltNativeLattice.S
    type B = NativeLattice.B
    type I = NativeLattice.I
    type R = NativeLattice.R
    type C = NativeLattice.C
    type Sym = AltNativeLattice.Sym
    // make the scheme lattice
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait NonClassNativeSchemeDomain extends ModularSchemeDomain:
    
    val modularLatticeWrapper = NonClassNativeSchemeDomain
    override def domainName: String = "modular native Scheme domain"