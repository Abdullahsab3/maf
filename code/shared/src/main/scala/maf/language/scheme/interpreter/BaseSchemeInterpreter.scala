package maf.language.scheme.interpreter

import maf.core.Identity
import maf.language.scheme._
import maf.language.sexp
import maf.language.sexp.{SExp, SExpId, SExpPair, SExpValue}

/** Common functionality for different Scheme interpreters, and interface methods needed for the primitives. */
trait BaseSchemeInterpreter[V] {
  // TODO: Maybe not all these definitions need to be abstract as some can be shared with the CPS interpreter.

  import ConcreteValues._

  // Both access to 'lastAddr' and 'store' should be synchronized on 'this'!
  var lastAddr = 0

  def newAddr(meta: AddrInfo): (Int, AddrInfo) = synchronized {
    lastAddr += 1
    (lastAddr, meta)
  }

  var store = Map[Addr, Value]()

  def extendStore(a: Addr, v: Value): Unit = synchronized {
    store = store + (a -> v)
  }

  def lookupStore(a: Addr): Value = synchronized {
    store(a)
  }

  def lookupStoreOption(a: Addr): Option[Value] = synchronized {
    store.get(a)
  }

  def setStore(s: Map[Addr, Value]): Unit = synchronized {
    store = s
  }

  def allocateVal(exp: SchemeExp, value: Value): Value.Pointer = {
    val addr = newAddr(AddrInfo.PtrAddr(exp))
    extendStore(addr, value)
    Value.Pointer(addr)
  }

  def allocateCons(
      exp: SchemeExp,
      car: Value,
      cdr: Value
    ): Value =
    allocateVal(exp, Value.Cons(car, cdr))

  def allocateStr(exp: SchemeExp, str: String): Value.Pointer =
    allocateVal(exp, Value.Str(str))

  def getString(addr: Addr): String = lookupStore(addr) match {
    case Value.Str(str) => str
    case v              => throw new UnexpectedValueTypeException[Value](v)
  }

  def makeList(values: List[(SchemeExp, Value)]): Value = values match {
    case Nil                  => Value.Nil
    case (exp, value) :: rest => allocateCons(exp, value, makeList(rest))
  }

  val stack: Boolean

  def stackedException[R](msg: String): R

  val io: IO

  def evalSExp(sexp: SExp, exp: SchemeExp): Value = sexp match {
    case SExpId(id)          => Value.Symbol(id.name)
    case SExpValue(value, _) => evalLiteral(value, exp)
    case SExpPair(car, cdr, _) =>
      val carValue = evalSExp(car, exp)
      val cdrValue = evalSExp(cdr, exp)
      allocateCons(exp, carValue, cdrValue)
  }

  def evalLiteral(lit: sexp.Value, exp: SchemeExp): ConcreteValues.Value = lit match {
    case maf.language.sexp.Value.String(s)    => allocateStr(exp, s)
    case maf.language.sexp.Value.Symbol(s)    => Value.Symbol(s)
    case maf.language.sexp.Value.Integer(n)   => Value.Integer(n)
    case maf.language.sexp.Value.Real(r)      => Value.Real(r)
    case maf.language.sexp.Value.Boolean(b)   => Value.Bool(b)
    case maf.language.sexp.Value.Character(c) => Value.Character(c)
    case maf.language.sexp.Value.Nil          => Value.Nil
  }
}
