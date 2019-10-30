package scalaam.modular.scheme

import scalaam.core._
import scalaam.modular._
import scalaam.language.scheme._

trait SchemeModFSemantics extends ModAnalysis[SchemeExp]
                          with GlobalStore[SchemeExp]
                          with ReturnResult[SchemeExp] {
  // local addresses are simply made out of lexical information
  trait LocalAddr extends Address { def pos(): Position }
  case class VarAddr(id: Identifier)          extends LocalAddr { def printable = true;  def pos(): Position = id.pos  }
  case class PtrAddr[E <: Expression](exp: E) extends LocalAddr { def printable = false; def pos(): Position = exp.pos }
  case class PrmAddr(name: String)            extends LocalAddr { def printable = false; def pos(): Position = Position.none }
  // abstract values come from a Scala-AM Scheme lattice (a type lattice)
  implicit val lattice: SchemeLattice[Value, SchemeExp, Addr]
  // Some glue code to Scala-AM to reuse the primitives and environment
  // not actually used, but required by the interface of SchemeSemantics
  implicit case object TimestampAdapter extends Timestamp[SchemeModFSemanticsIntra,Unit] {
    def initial(s: String)                  = throw new Exception("Operation not allowed!")
    def tick(cmp: SchemeModFSemanticsIntra) = throw new Exception("Operation not allowed!")
  }
  // The AllocAdapter makes sure the right dependencies are registered upon address allocation.
  case object AllocAdapter extends Allocator[Addr,SchemeModFSemanticsIntra,Unit] {
    def variable(id: Identifier, intra: SchemeModFSemanticsIntra): Addr =
      intra.allocAddr(VarAddr(id))
    def pointer[E <: Expression](exp: E, intra: SchemeModFSemanticsIntra): Addr =
      intra.allocAddr(PtrAddr(exp))
    def primitive(name: String): Addr =
      GlobalAddr(PrmAddr(name))
  }
  lazy val schemeSemantics = new BaseSchemeSemantics[Addr, Value, SchemeModFSemanticsIntra, Unit](AllocAdapter)
  // setup initial environment and install the primitives in the global store
  def initialEnv = Environment.initial(schemeSemantics.initialEnv)
  schemeSemantics.initialStore.foreach { case (a,v) => store = store + (a -> v) }
  // in ModF, components are function calls in some context
  trait IntraComponent {
    def env: Environment[Addr]
  }
  case object MainComponent extends IntraComponent {
    val env = initialEnv
    override def toString = "main"
  }
  case class CallComponent(lambda: SchemeLambda, env: Environment[Addr], nam: Option[String], ctx: Context) extends IntraComponent {
    override def toString = nam match {
      case None => s"anonymous@${lambda.pos} [${ctx.toString()}]"
      case Some(name) => s"$name [${ctx.toString()}]"
    }
  }
  lazy val initialComponent = MainComponent
  // this abstract class is parameterized by the choice of Context and allocation strategy of Contexts
  type Context
  def allocCtx(lambda: SchemeLambda, env: Environment[Addr], args: List[Value]): Context
  // extension for the intraAnalysis
  trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {
    // primitives glue code
    // TODO[maybe]: while this should be sound, it might be more precise to not immediately write every value update to the global store ...
    case object StoreAdapter extends Store[Addr,Value] {
      def lookup(a: Addr)                       = Some(readAddr(a))
      def extend(a: Addr, v: Value)             = { writeAddr(a,v) ; this }
      // all the other operations should not be used by the primitives ...
      def content                               = throw new Exception("Operation not allowed!")
      def keys                                  = throw new Exception("Operation not allowed!")
      def restrictTo(a: Set[Addr])              = throw new Exception("Operation not allowed!")
      def forall(p: ((Addr, Value)) => Boolean) = throw new Exception("Operation not allowed!")
      def join(that: Store[Addr, Value])        = throw new Exception("Operation not allowed!")
      def subsumes(that: Store[Addr, Value])    = throw new Exception("Operation not allowed!")
    }
  }
}

trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                  with SchemeModFSemantics
                                  with AdaptiveGlobalStore[SchemeExp]
                                  with AdaptiveReturnResult[SchemeExp] {
  def alpha(cmp: IntraComponent) = ???
}
