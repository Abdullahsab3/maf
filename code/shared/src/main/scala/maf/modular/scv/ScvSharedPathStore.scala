package maf.modular.scv

import maf.core.Identity
import maf.language.symbolic.*
import maf.core.Address

case class PcAddr(pc: PathCondition) extends Address:
    def idn: Identity = Identity.none
    def printable: Boolean = false
    override def toString: String = s"pc($pc)"

/**
 * Shares the entire path store (after restoring renames of symbolic variables) across function call components.
 *
 * It is implemented by writing all the path conditions (a disjunction of conjunctions) to the global store. Then this path condition is used by the
 * caller to generate successor states.
 */
trait ScvSharedPathStore extends maf.modular.scv.BaseScvBigStepSemantics with ScvSymbolicStore.GlobalSymbolicStore:
    override def intraAnalysis(component: Component): SharedPathStoreIntra

    trait SharedPathStoreIntra extends BaseIntraScvSemantics with GlobalMapStoreIntra:
        import scvMonadInstance.*
        import maf.core.Monad.MonadSyntaxOps

        private def readPathCondition(targetCmp: Component): PathCondition =
          PathCondition(readMapAddr(targetCmp))

        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathCondition)] =
            //println(s"=== intra sem $cmp ==")
            val answers = super.runIntraSemantics(initialState)
            // answers.map(_._2).foreach(answer => println(s"+++ answer: ${answer.pc}"))
            val formulas = answers.map(_._2.formula).toList
            val values = answers.map(_._1._2).toList
            writeMapAddr(cmp, Formula.join(formulas: _*))

            // TODO: do not register when reading
            val readValue = readMapAddr(cmp)
            values.foreach(writeAddr(PcAddr(PathCondition(readValue)), _))

            formulas.foreach { formula => println(s"+++ formula $formula") }
            //println(s"answer $answers")
            answers

        override protected def afterCall(vlu: Value, targetCmp: Component): EvalM[Value] =
            import FormulaAux.*
            // this is a very crude approximation, we propebably don't need the entire path condition from the target
            context(targetCmp) match
                case Some(k: KPathCondition[_]) =>
                  val readPc = readPathCondition(targetCmp)
                  val gcPc = readPc.gc(k.symArgs.values.toSet)

                  val revertedPc = k.changes.reverse.foldLeft(gcPc)((pc, change) => pc.revert(change))

                  for
                      pc <- getPc
                      _ <- putPc(PathCondition(DNF.dnf(conj(pc.formula, revertedPc.formula))))
                  yield vlu
                case _ => unit(vlu)