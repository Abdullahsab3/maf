package maf.modular.scheme

import maf.lattice.NativeLattice
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.language.scheme.primitives._
import maf.lattice.NativeLattice.L._

object NativeDomainWSStrings extends ModularSchemeLatticeWrapper:
    type S = NativeLattice.S2
    type B = NativeLattice.B
    type I = NativeLattice.I
    type R = NativeLattice.R
    type C = NativeLattice.C
    type Sym = NativeLattice.Sym2
    // make the scheme lattice
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait NativeDomainWSStrings extends ModularSchemeDomain:

    val modularLatticeWrapper = NativeDomainWSStrings
    override def domainName: String = "modular native Scheme domain"