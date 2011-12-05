import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    def factorial(n: BigInt): BigInt =
      if (n == 0) 1 else n * factorial(n-1)

    val f50 = factorial(50); val f49 = factorial(49)
    println("50! = " + f50)
    println("49! = " + f49)
    println("50!/49! = " + (f50 / f49))
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
