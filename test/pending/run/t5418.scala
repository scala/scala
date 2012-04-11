import scala.reflect.mirror._

object Test extends App {
  reify {
    new Object().getClass
  }.eval
}