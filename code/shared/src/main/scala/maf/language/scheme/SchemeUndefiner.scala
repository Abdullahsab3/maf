package maf.language.scheme

import maf.core._

/**
 * Remove defines from a Scheme expression, replacing them by let bindings. For example: (define foo 1) (define (f x) x) (f foo) Will be converted to:
 * (letrec ((foo 1) (f (lambda (x) x))) (f foo)) Which is semantically equivalent with respect to the end result
 */
trait BaseSchemeUndefiner {
  import scala.util.control.TailCalls._

  def undefine(exps: List[SchemeExp]): SchemeExp =
    undefine(exps, List(), None).result

  def undefine(
      exps: List[SchemeExp],
      defs: List[(Identifier, SchemeExp)],
      idn: Option[Identity]
    ): TailRec[SchemeExp] =
    exps match {
      case Nil => done(SchemeBegin(Nil, Identity.none))
      case SchemeDefineVariable(name, value, pos) :: rest =>
        tailcall(undefine1(value)).flatMap(v => tailcall(undefine(rest, (name, v) :: defs, idn.orElse(Some(pos)))))
      case _ :: _ =>
        tailcall(undefineBody(exps)).map { bdy =>
          if (defs.isEmpty) {
            SchemeBody(bdy)
          } else {
            SchemeLetrec(defs.reverse, if (bdy.nonEmpty) bdy else List(SchemeBody(bdy)), idn.get)
          }
        }
    }

  def trampolineM[A, B](f: A => TailRec[B], l: List[A]): TailRec[List[B]] = l match {
    case Nil => done(Nil)
    case x :: xs =>
      tailcall(f(x)).flatMap(y => tailcall(trampolineM(f, xs)).flatMap(ys => done(y :: ys)))
  }

  def undefine1(exp: SchemeExp): TailRec[SchemeExp] = undefine(List(exp), List(), None)

  def undefineExp(exp: SchemeExp): TailRec[SchemeExp] = exp match {
    case SchemeLambda(name, args, body, pos) =>
      tailcall(undefineBody(body)).map(b => SchemeLambda(name, args, b, pos))
    case SchemeVarArgLambda(name, args, vararg, body, pos) =>
      tailcall(undefineBody(body)).map(b => SchemeVarArgLambda(name, args, vararg, b, pos))
    case SchemeFuncall(f, args, pos) =>
      tailcall(undefine1(f)).flatMap(fun => trampolineM(undefine1, args).map(argsv => SchemeFuncall(fun, argsv, pos)))
    case SchemeIf(cond, cons, alt, pos) =>
      tailcall(undefine1(cond)).flatMap(condv =>
        tailcall(undefine1(cons)).flatMap(consv => tailcall(undefine1(alt)).map(altv => SchemeIf(condv, consv, altv, pos)))
      )
    case SchemeLet(bindings, body, pos) =>
      trampolineM(
        (x: (Identifier, SchemeExp)) =>
          x match {
            case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
          },
        bindings
      ).flatMap(bindingsv => tailcall(undefineBody(body)).map(bodyv => SchemeLet(bindingsv, bodyv, pos)))
    case SchemeLetStar(bindings, body, pos) =>
      trampolineM(
        (x: (Identifier, SchemeExp)) =>
          x match {
            case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
          },
        bindings
      ).flatMap(bindingsv => tailcall(undefineBody(body)).map(bodyv => SchemeLetStar(bindingsv, bodyv, pos)))
    case SchemeLetrec(bindings, body, pos) =>
      trampolineM(
        (x: (Identifier, SchemeExp)) =>
          x match {
            case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
          },
        bindings
      ).flatMap(bindingsv => tailcall(undefineBody(body)).map(bodyv => SchemeLetrec(bindingsv, bodyv, pos)))
    case SchemeSet(variable, value, pos) =>
      tailcall(undefine1(value)).map(v => SchemeSet(variable, v, pos))
    case SchemeBegin(exps, pos) =>
      tailcall(undefineBody(exps)).map(expsv => SchemeBegin(expsv, pos))
    case SchemeAssert(exp, pos) =>
      for {
        expUndef <- tailcall(undefine1(exp))
      } yield SchemeAssert(expUndef, pos)
    case SchemeVar(id)           => done(SchemeVar(id))
    case SchemeValue(value, pos) => done(SchemeValue(value, pos))
    case _                       => throw new Exception(s"Unsupported scheme expression $exp")
  }

  def undefineBody(exps: List[SchemeExp]): TailRec[List[SchemeExp]] = exps match {
    case Nil                                => done(Nil)
    case SchemeDefineVariable(_, _, _) :: _ => tailcall(undefine(exps, List(), None)).map(v => List(v))
    case exp :: rest                        => undefineExp(exp).flatMap(e1 => tailcall(undefineBody(rest)).flatMap(e2 => done(e1 :: e2)))
  }
}

object SchemeUndefiner extends BaseSchemeUndefiner
