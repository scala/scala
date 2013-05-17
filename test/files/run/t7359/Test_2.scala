import scala.reflect.runtime.universe._

object Test extends App {
  typeOf[Cyclic].members
  println("ok")
}