import scala.reflect.runtime.universe._

object Test extends App {
  println(showRaw(typeOf[String]))
}