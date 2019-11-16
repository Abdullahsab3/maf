package scalaam.language.scheme

import scalaam.core.{Position, NoPosition, Identifier, Expression}
import scalaam.language.sexp._

/**
  * Abstract syntax of Scheme programs
  */
trait SchemeExp extends Expression

/**
  * A lambda expression: (lambda (args...) body...)
  * Not supported: "rest"-arguments, of the form (lambda arg body), or (lambda (arg1 . args) body...)
  */
trait SchemeLambdaExp extends SchemeExp {
  // a lambda takes arguments, and has a non-empty body
  val args: List[Identifier]
  val body: List[SchemeExp]
  require(body.nonEmpty)
  // does the lambda support a variable number of arguments
  def varArgId: Option[Identifier]
  // can this lambda be called with a given number of arguments
  def check(argc: Int) =
    if (varArgId.isDefined) {
      argc == args.length
    } else {
      argc >= args.length
    }
  // free variables
  def fv: Set[String] = body.flatMap(_.fv).toSet -- args.map(_.name).toSet -- varArgId.map(id => Set(id.name)).getOrElse(Set[String]())
  // height
  override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
}

case class SchemeLambda(args: List[Identifier], body: List[SchemeExp], pos: Position) extends SchemeLambdaExp {
  override def toString: String = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
  }
  def varArgId = None
}

case class SchemeVarArgLambda(args: List[Identifier], vararg: Identifier, body: List[SchemeExp], pos: Position) extends SchemeLambdaExp {
  override def toString: String = {
    val a = if (args.isEmpty) {
      vararg.toString
    } else {
      s"(${args.mkString(" ")} . $vararg)"
    }
    val b = body.mkString(" ")
    s"(lambda $a $b)"
  }
  def varArgId = Some(vararg)
}

/**
  * A function call: (f args...)
  */
case class SchemeFuncall(f: SchemeExp, args: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
  def fv: Set[String] = f.fv ++ args.flatMap(_.fv).toSet
  override val height: Int = 1 + args.foldLeft(0)((mx, a) => mx.max(a.height).max(f.height))
}

/**
  * An if statement: (if cond cons alt)
  * If without alt clauses need to be encoded with an empty begin as alt clause
  */
case class SchemeIf(cond: SchemeExp, cons: SchemeExp, alt: SchemeExp, pos: Position)
    extends SchemeExp {
  override def toString: String = s"(if $cond $cons $alt)"
  def fv: Set[String] = cond.fv ++ cons.fv ++ alt.fv
  override val height: Int = 1 + cond.height.max(cons.height).max(alt.height)
}

/**
  * Let-bindings: (let ((v1 e1) ...) body...)
  */
case class SchemeLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
  def fv: Set[String] =
    bindings.map(_._2).flatMap(_.fv).toSet ++ (body.flatMap(_.fv).toSet -- bindings
      .map(_._1.name)
      .toSet)
  override val height: Int = 1 + bindings.foldLeft(0)((mx, b) => mx.max(b._2.height).max(body.foldLeft(0)((mx, e) => mx.max(e.height))))
}

/**
  * Let*-bindings: (let* ((v1 e1) ...) body...)
  */
case class SchemeLetStar(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
  def fv: Set[String] =
    bindings
      .foldLeft((Set.empty[String] /* bound variables */, Set.empty[String] /* free variables */ ))(
        (acc, binding) =>
          binding match {
            case (id, e) => (acc._1 + id.name, acc._2 ++ (e.fv -- acc._1))
          }
      )
      ._2 ++ (body.flatMap(_.fv).toSet -- bindings.map(_._1.name).toSet)
  override val height: Int = 1 + bindings.foldLeft(0)((mx, b) => mx.max(b._2.height).max(body.foldLeft(0)((mx, e) => mx.max(e.height))))
}

/**
  * Letrec-bindings: (letrec ((v1 e1) ...) body...)
  */
case class SchemeLetrec(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
  def fv: Set[String] =
    (bindings.map(_._2).flatMap(_.fv).toSet ++ body.flatMap(_.fv).toSet) -- bindings
      .map(_._1.name)
      .toSet
  override val height: Int = 1 + bindings.foldLeft(0)((mx, b) => mx.max(b._2.height).max(body.foldLeft(0)((mx, e) => mx.max(e.height))))
}

/**
  * Named-let: (let name ((v1 e1) ...) body...)
  * TODO: desugar to letrec according to R5RS
  */
case class SchemeNamedLet(name: Identifier, bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let $name ($bi) $bo)"
  }
  def fv: Set[String] =
    bindings.map(_._2).flatMap(_.fv).toSet ++ (body
      .flatMap(_.fv)
      .toSet -- (bindings.map(_._1.name).toSet + name.name))
  override val height: Int = 1 + bindings.foldLeft(0)((mx, b) => mx.max(b._2.height).max(body.foldLeft(0)((mx, e) => mx.max(e.height))))
}

/**
  * A set! expression: (set! variable value)
  */
trait SchemeSetExp extends SchemeExp {
  val variable: Identifier
  val value: SchemeExp
  def fv: Set[String] = value.fv + variable.name
  override val height: Int = 1 + value.height
}

case class SchemeSet(variable: Identifier, value: SchemeExp, pos: Position) extends SchemeSetExp {
  override def toString: String = s"(set! $variable $value)"
}

case class SchemeSetLex(variable: Identifier, lexAddr: LexicalRef, value: SchemeExp, pos: Position) extends SchemeSetExp {
  override def toString = s"(set! $lexAddr $value)"
}

/**
  * A begin clause: (begin body...)
  */
case class SchemeBegin(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
  def fv: Set[String] = exps.flatMap(_.fv).toSet
  override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
}

/**
  * Used to create a begin if there are multiple statements, and a single exp if there is only one
  */
object SchemeBody {
  def apply(exps: List[SchemeExp]): SchemeExp = exps match {
    case Nil        => SchemeValue(ValueBoolean(false), NoPosition) /* undefined */
    case exp :: Nil => exp
    case exp :: _   => SchemeBegin(exps, exp.pos)
  }
}

/**
  * A cond expression: (cond (test1 body1...) ...).
  * Desugared according to R5RS.
  */
object SchemeCond {
  def apply(clauses: List[(SchemeExp, List[SchemeExp])], pos: Position): SchemeExp =
    if (clauses.isEmpty) {
      throw new Exception(s"Invalid Scheme cond without clauses ($pos)")
    } else {
      clauses.foldRight[SchemeExp](SchemeValue(ValueBoolean(false /* undefined */ ), NoPosition))(
        (clause, acc) =>
          clause match {
            case (SchemeValue(ValueBoolean(true), _), body) => SchemeBody(body)
            case (cond, Nil)                                =>
              /* Body is empty. R5RS states that "If the selected clause contains only the
               * test and no expressions ,then the value of the test is returned
               * as the result" */
              val id = Identifier("__cond-empty-body", cond.pos)
              SchemeLet(
                List((id, cond)),
                List(SchemeIf(SchemeVar(id), SchemeVar(id), acc, cond.pos)),
                cond.pos
              )
            case (cond, body) => SchemeIf(cond, SchemeBody(body), acc, cond.pos)
          }
      )
    }
}

/**
  * A case expression: (case key ((vals1...) body1...) ... (else default...)).
  * Desugared according to R5RS.
  */
object SchemeCase {
  def apply(
      key: SchemeExp,
      clauses: List[(List[SchemeValue], List[SchemeExp])],
      default: List[SchemeExp],
      pos: Position
  ): SchemeExp = key match {
    case _: SchemeVar | _: SchemeValue =>
      /** Atomic key */
      val eqv = SchemeVar(Identifier("eq?", NoPosition)) /* TODO: should be eqv? instead of eq? */
      clauses.foldRight[SchemeExp](SchemeBody(default))(
        (clause, acc) =>
          /** In R5RS, the condition is desugared into a (memv key '(atoms ...)) call. This
           * would mean we would have to construct a list and go through it,
           * which would badly impact precision. Hence, we instead explicitly do
           * a big-or with eq? */
          SchemeIf(
            SchemeOr(
              clause._1.map(
                atom =>
                  SchemeFuncall(eqv, List(key, atom), atom.pos)
              ),
              pos
            ),
            SchemeBody(clause._2),
            acc,
            pos
          )
      )
    case _ =>
      /** Non-atomic key, let-bind it */
      val id = Identifier("__case-atom-key", key.pos)
      SchemeLet(List((id, key)), List(SchemeCase(SchemeVar(id), clauses, default, pos)), key.pos)
  }
}

/**
  * An and expression: (and exps...)
  */
case class SchemeAnd(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
  def fv: Set[String] = exps.flatMap(_.fv).toSet
  override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
}

/**
  * An or expression: (or exps...)
  */
case class SchemeOr(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
  def fv: Set[String] = exps.flatMap(_.fv).toSet
  override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
}

/**
  * A variable definition: (define name value)
  */
case class SchemeDefineVariable(name: Identifier, value: SchemeExp, pos: Position) extends SchemeExp {
  override def toString: String = s"(define $name $value)"
  def fv: Set[String] = value.fv
  override val height: Int = 1 + value.height
}

/**
  * A function definition: (define (name args...) body...)
  */
case class SchemeDefineFunction(name: Identifier, args: List[Identifier], body: List[SchemeExp], pos: Position)
extends SchemeExp {
  override def toString: String = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
  def fv: Set[String] = body.flatMap(_.fv).toSet -- (args.map(_.name).toSet + name.name)
  override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
}

case class SchemeDefineVarArgFunction(name: Identifier, args: List[Identifier], vararg: Identifier, body: List[SchemeExp], pos: Position)
extends SchemeExp {
  override def toString: String = {
    val a = s"${args.mkString(" ")} . $vararg"
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
  def fv: Set[String] = body.flatMap(_.fv).toSet -- (args.map(_.name).toSet + name.name) - vararg.name
  override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
}

/**
  * Do notation: (do ((<variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...).
  * Desugared according to R5SR.
  */
object SchemeDo {
  def apply(
      vars: List[(Identifier, SchemeExp, Option[SchemeExp])],
      test: SchemeExp,
      finals: List[SchemeExp],
      commands: List[SchemeExp],
      pos: Position
  ): SchemeExp = {
    val loopId = Identifier("__do_loop", pos)
    SchemeLetrec(
      List(
        (
          loopId,
          SchemeLambda(
            vars.map(_._1),
            List(
              SchemeIf(
                test,
                SchemeBody(finals),
                SchemeBody(
                  commands :::
                    List(SchemeFuncall(SchemeVar(loopId), vars.map({
                      case (_, _, Some(step)) => step
                      case (id, _, None)      => SchemeVar(id)
                    }), pos))
                ),
                pos
              )
            ),
            pos
          )
        )
      ),
      List(SchemeFuncall(SchemeVar(loopId), vars.map(_._2), pos)),
      pos
    )
  }
}

/**
  * An identifier: name
  */
trait SchemeVarExp extends SchemeExp {
  val id: Identifier
  override val height: Int = 1
  val   pos: Position = id.pos
  def fv: Set[String] = Set(id.name)
}

case class SchemeVar(id: Identifier)
  extends SchemeVarExp {
    override def toString: String = id.name
  }

case class SchemeVarLex(id: Identifier, lexAddr: LexicalRef)
  extends SchemeVarExp {
    override def toString = lexAddr.toString
  }

case class SchemePair(car: SchemeExp, cdr: SchemeExp, pos: Position) extends SchemeExp {
  override def toString: String = s"`(,$car . ,$cdr)"
  def fv: Set[String] = car.fv ++ cdr.fv
}

case class SchemeSplice(splice: SchemeExp, cdr: SchemeExp, pos: Position) extends SchemeExp {
  override def toString: String = s"`(,@$splice . ,$cdr)"
  def fv: Set[String] = splice.fv ++ cdr.fv
}

/**
  * A literal value (number, symbol, string, ...)
  */
case class SchemeValue(value: Value, pos: Position) extends SchemeExp {
  override def toString: String = value.toString
  def fv: Set[String] = Set()
  override val height: Int = 1
}
