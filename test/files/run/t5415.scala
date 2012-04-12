object Test extends App{
  case class Queryable2[T]() { def filter(predicate: T => Boolean) = ??? }
  trait CoffeesTable{ def sales : Int }
  val q = Queryable2[CoffeesTable]()
  import scala.reflect.mirror._
  val code = reify{q.filter(_.sales > 5)}

  val toolbox = mkToolBox()
  val ttree = toolbox.typeCheck(code.tree)
}
