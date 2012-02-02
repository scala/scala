import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import scala.reflect.runtime.Mirror.ToolBox

object Test extends App{
  case class Queryable2[T]() { def filter(predicate: T => Boolean) = ??? }
  trait CoffeesTable{ def sales : Int }
  val q = Queryable2[CoffeesTable]()
  val code = scala.reflect.Code.lift{q.filter(_.sales > 5)}

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
}
