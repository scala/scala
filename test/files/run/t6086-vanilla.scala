case class X(s: String)

object Test extends App {
  import scala.reflect.runtime.universe._
  println(typeOf[X])
}