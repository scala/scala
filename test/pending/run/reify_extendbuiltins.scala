import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    def fact(n: Int): BigInt =
      if (n == 0) 1 else fact(n-1) * n
    class Factorizer(n: Int) {
      def ! = fact(n)
    }
    implicit def int2fact(n: Int) = new Factorizer(n)

    println("10! = " + (10!))
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
