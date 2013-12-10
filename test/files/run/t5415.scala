object Test extends App{
  case class Queryable2[T]() { def filter(predicate: T => Boolean) = ??? }
  trait CoffeesTable{ def sales : Int }
  val q = Queryable2[CoffeesTable]()
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{universe => ru}
  val code = reify{q.filter(_.sales > 5)}
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val toolbox = cm.mkToolBox()
  val ttree = toolbox.typecheck(code.tree)
}
