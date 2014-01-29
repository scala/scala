import scala.reflect.runtime.universe._
object Test extends App {
  val x = TermName("x")
  q"def foo($x)"
}
