package maf.modular.incremental

import akka.io.Tcp.SO.KeepAlive
import maf.core.*
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.language.sexp.Value
import maf.modular.incremental.ProgramVersionExtracter.getVersion
import maf.util.{Reader, Writer}

import scala.util.Random

/** Automatically add change expressions to programs. */
object ProgramChanger {

  private val rand = new Random()

  private enum StatementAction:
      case Add, Remove, Swap, None

  private enum VariableAction:
      case Keep, Rename

  import StatementAction.*
  import VariableAction.*

  // Gets a StatementAction with a certain probability:
  // None: 85%
  // Add: 5%
  // Remove: 5%
  // Swap: 5%
  private def getStatementAction(): StatementAction =
      val n = rand.nextDouble()
      if n < 0.85 then None
      else if n < 0.9 then Add
      else if n < 0.95 then Remove
      else Swap

  private def getVariableAction(): VariableAction =
      val n = rand.nextDouble()
      if n < 0.925 then Keep
      else Rename

  private def changeBody(lst: List[SchemeExp], nested: Boolean): List[SchemeExp] =
    (lst, getStatementAction()) match {
      case (Nil, _) => Nil

      case (l @ (h :: Nil), None) => changeExpression(h, nested) :: Nil
      case (h :: t, None)         => changeExpression(h, nested) :: changeBody(t, nested)

      case (h :: t, Remove) if nested => changeBody(t, nested)
      case (h :: t, Remove)           => SchemeCodeChange(h, SchemeValue(Value.Nil, Identity.none), Identity.none) :: changeBody(t, nested)

      case (l @ (h :: t), Add) if nested => changeExpression(h, nested) :: changeExpression(h, nested) :: changeBody(t, nested)
      case (l @ (h :: t), Add) =>
        SchemeCodeChange(SchemeBegin(l, Identity.none),
                         SchemeBegin(changeExpression(h, true) :: l.map(changeExpression(_, true)), Identity.none),
                         Identity.none
        ) :: changeBody(t, nested)

      case (l @ (h :: Nil), Swap) if nested => changeExpression(h, nested) :: changeExpression(h, nested) :: Nil
      case (l @ (h :: Nil), Swap) =>
        SchemeCodeChange(SchemeBegin(l, Identity.none),
                         SchemeBegin(changeExpression(h, true) :: l.map(changeExpression(_, true)), Identity.none),
                         Identity.none
        ) :: Nil // When there is only one statement, swap equals add.

      case (l @ (h1 :: h2 :: t), Swap) if nested => changeExpression(h2, nested) :: changeExpression(h1, nested) :: changeBody(t, nested)
      case (l @ (h1 :: h2 :: t), Swap) =>
        SchemeCodeChange(h1, changeExpression(h2, true), Identity.none) :: SchemeCodeChange(h2,
                                                                                            changeExpression(h1, true),
                                                                                            Identity.none
        ) :: changeBody(t, nested)
    }

  // Nested indicated whether we are already in a changed expression (the "new" expression), and hence the changes can be made without introducing a change expression again.
  private def changeExpression(e: SchemeExp, nested: Boolean): SchemeExp = e match {
    case SchemeLambda(name, args, body, idn)               => SchemeLambda(name, args, changeBody(body, nested), idn)
    case SchemeVarArgLambda(name, args, vararg, body, idn) => SchemeVarArgLambda(name, args, vararg, changeBody(body, nested), idn)
    case SchemeFuncall(f, args, idn)                       => SchemeFuncall(f, args.map(changeExpression(_, nested)), idn)
    case SchemeIf(cond, cons, alt, idn) =>
      SchemeIf(changeExpression(cond, nested), changeExpression(cons, nested), changeExpression(alt, nested), idn)
    case SchemeLet(bindings, body, idn) => SchemeLet(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, nested), idn)
    case SchemeLetStar(bindings, body, idn) =>
      SchemeLetStar(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, nested), idn)
    case SchemeLetrec(bindings, body, idn) =>
      SchemeLetrec(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, nested), idn)
    case SchemeSet(variable, value, idn)        => SchemeSet(variable, changeExpression(value, nested), idn)
    case SchemeBegin(exps, idn)                 => SchemeBegin(changeBody(exps, nested), idn)
    case SchemeDefineVariable(name, value, idn) => SchemeDefineVariable(name, changeExpression(value, nested), idn)
    //case SchemeVar(id) =>
    //case SchemeValue(value, idn) =>
    case exp => exp

  }

  def changeBodyStatements(in: String, out: String): Unit =
      val parsed = CSchemeParser.parseProgram(Reader.loadFile(in))
      val newProgram = changeExpression(parsed, false).prettyString()
      val writer = Writer.open(out)
      Writer.write(writer, newProgram)
      Writer.close(writer)
}

object Changer {

  def main(args: Array[String]): Unit =
      val inputFile = "test/R5RS/ad/selsort.scm"
      def outputFile(n: Int = 0) = s"test/changes/scheme/generated/selsort-nested-$n.scm"
      val times = 10
      for (i <- 0 to 10) do ProgramChanger.changeBodyStatements(inputFile, outputFile(i))
}
