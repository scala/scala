import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.reflect.runtime.universe.Flag._
import scala.tools.reflect.ToolBox

object Test extends App {
  val stdout = System.out
  val output = new java.io.ByteArrayOutputStream()
  System.setOut(new java.io.PrintStream(output))
  val toolBox = currentMirror.mkToolBox(options = "-Yreify-copypaste")
  val reify = Select(Select(Select(Select(Ident(ScalaPackage), TermName("reflect")), TermName("runtime")), TermName("universe")), TermName("reify"))
  val reifee = Block(List(ValDef(Modifiers(LAZY), TermName("x"), TypeTree(), Apply(Ident(ListModule), List(Literal(Constant(1)), Literal(Constant(2)))))), Ident(TermName("x")))
  toolBox.eval(Apply(reify, List(reifee)))
  val Block(List(tpeCopypaste, exprCopypaste @ ModuleDef(_, _, Template(_, _, (_ :: stats) :+ expr))), Literal(Constant(()))) = toolBox.parse(output.toString())
  output.reset()
  toolBox.eval(Block(stats, expr))
  stdout.println(output.toString)
}