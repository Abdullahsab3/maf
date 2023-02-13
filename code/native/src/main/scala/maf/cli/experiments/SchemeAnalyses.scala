package maf.cli.experiments

import maf.language.scheme._
import maf.modular._
import maf.language.symbolic.lattices.*
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.util.MonoidImplicits.setMonoid
import maf.modular.scheme.modflocal._
import maf.modular.scheme.modconc._
import maf.modular.worklist._

object SchemeAnalysesBoundedDomain:
    object NoSensitivity:
        def boundAnalysis(
            bnd: Int
          )(
            prg: SchemeExp
          ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeBoundedDomain(bnd) with LIFOWorklistAlgorithm[SchemeExp] {
            override def toString = "no-sensitivity"
        }
    object CallSiteSensitivity:
        def boundAnalysis(
            bnd: Int
          )(
            prg: SchemeExp
          ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFCallSiteSensitivity with SchemeBoundedDomain(bnd) with LIFOWorklistAlgorithm[SchemeExp] {
            val bound = bnd
            override def toString = "call-site-sensitivity"
        }
    object TwoCallSiteSensitivity:
        def boundAnalysis(
            bnd: Int
          )(
            prg: SchemeExp
          ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFCallSiteSensitivity with SchemeBoundedDomain(bnd) with LIFOWorklistAlgorithm[SchemeExp] {
            override def toString = "call-site-sensitivity"
        }

object SchemeAnalyses:

    // Incremental analyses in maf.modular.incremental.scheme.SchemeAnalyses

    def contextInsensitiveAnalysis(
        prg: SchemeExp
      ) = new SimpleSchemeModFAnalysis(prg) with SchemeModFNoSensitivity with SchemeConstantPropagationDomain with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = "no-sensitivity"
    }

    def callSiteContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
        with SchemeModFCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = "call-site-sensitivity"
    }
    def kCFAAnalysis(prg: SchemeExp, kcfa: Int) = new SimpleSchemeModFAnalysis(prg)
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = s"kCFA (k = $kcfa)"
        val k = kcfa
    }
    def fullArgContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
        with SchemeModFFullArgumentSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString = "full-argument-sensitivity"
    }

    def modConcAnalysis(prg: SchemeExp, kcfa: Int) = new SimpleSchemeModConcAnalysis(prg)
        with SchemeModConcStandardSensitivity
        with SchemeConstantPropagationDomain
        with CallDepthFirstWorklistAlgorithm[SchemeExp] {
        override def toString = s"base modconc"
        override def modFAnalysis(
            intra: SchemeModConcIntra
          ) = new InnerModFAnalysis(intra) with SchemeModFKCallSiteSensitivity with CallDepthFirstWorklistAlgorithm[SchemeExp] {
            val k = kcfa
        }
    }