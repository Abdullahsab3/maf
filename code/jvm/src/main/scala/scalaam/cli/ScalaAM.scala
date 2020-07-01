package scalaam.cli

import scalaam.core.Position._
import scalaam.language.CScheme._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives.SchemePrelude
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.semantics._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

import scala.concurrent.duration._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/R5RS/fact.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new SimpleSchemeModFAnalysis(prg)
                                        with NoSensitivity
                                        with SchemeConstantPropagationDomain
                                        with LIFOWorklistAlgorithm[SchemeExp] {
      //override def workers = 4
      override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
      var i = 0
      override def step(t: Timeout.T = Timeout.none): Unit = {
        i = i + 1
        val cmp = workList.head
        println(s"[$i] $cmp")
        super.step(t)
      }
    }
    analysis.analyze(Timeout.start(Duration(3600,SECONDS)))
    //debugClosures(analysis)
    debugResults(analysis, false)
  }

  type SchemeModFAnalysis = ModAnalysis[SchemeExp] with SchemeModFSemantics with StandardSchemeModFComponents

  def debugResults(machine: SchemeModFAnalysis, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (ComponentAddr(cmp, ReturnAddr),result) if cmp == machine.initialComponent =>
        println(s"[$cmp] ${machine.view(cmp)} => $result")
      case (ComponentAddr(_, _: PrmAddr), _) => 
        () //don't print primitive addresses
      case (addr,value) if printMore =>
        println(s"$addr => $value")
      case _ => ()
    }
}

/*
object Incrementor extends App {

  type Analysis = IncrementalModAnalysis[SchemeExp] with SmallStepSemantics with ConstantPropagationDomain with NoSensitivity with IncrementalSchemeModFSemantics
  var analyzer: Analysis = _

  def analyze(file: String): Unit = analyze(SchemeParser.parse(Reader.loadFile(file)))

  private def analyze(text: SchemeExp): Unit = {
    analyzer = new IncrementalModAnalysis(text) with SmallStepSemantics
                                                    with ConstantPropagationDomain
                                                    with NoSensitivity
                                                    with IncrementalSchemeModFSemantics {
    }
    analyzer.analyze()//Timeout.start(Duration(2, "MINUTES")))
    println(s"Number of components: ${analyzer.allComponents.size}")
  }

  def reanalyse(text: SchemeExp): Unit = analyzer.updateAnalysis(text)

  val a = ModuleInferencer.inferModules(SchemeParser.parse(Reader.loadFile("./test/ad/inssort.scm")))
  val b = ModuleInferencer.inferModules(SchemeParser.parse(Reader.loadFile("./test/ad/inssort2.scm")))
  println(a)
  println(b)
  val mapping = GumtreeModuleDiff.computeMapping(a, b)
  mapping.map(println)

}
*/


object Run extends App {
  val text = Reader.loadFile("test/concurrentScheme/threads/minimax.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), true)
  val res = interpreter.run(CSchemeUndefiner.undefine(List(SchemePrelude.addPrelude(CSchemeParser.parse(text), Set("newline", "display")))), Timeout.none)
  println(res)
}
/*
object Analyze extends App {
  val text = Reader.loadFile("test/DEBUG.scm")
  val a = new ModAnalysis(CSchemeParser.parse(text))
    with KAExpressionContext
    with ModConcConstantPropagationDomain
   // with NoSensitivity
    with LIFOWorklistAlgorithm[SchemeExp] {
  }
  a.analyze(Timeout.none)
  val r = a.store(a.ReturnAddr(a.initialComponent))
  println(r)
}
*/