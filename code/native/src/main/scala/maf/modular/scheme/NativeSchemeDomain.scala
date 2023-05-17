package maf.modular.scheme

import maf.lattice.NativeLattice
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.language.scheme.primitives._
import maf.lattice.NativeLattice._

object NativeSchemeDomain extends ModularSchemeLatticeWrapper:
    type S = NativeLattice.S
    type B = NativeLattice.B
    type I = NativeLattice.I
    type R = NativeLattice.R
    type C = NativeLattice.C
    type Sym = NativeLattice.Sym
    // make the scheme lattice
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait NativeSchemeDomain extends ModularSchemeDomain:
    
    val modularLatticeWrapper = NativeSchemeDomain
    override def domainName: String = "modular native Scheme domain"