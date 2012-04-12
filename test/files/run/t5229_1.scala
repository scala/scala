import scala.reflect.mirror._

object Test extends App {
  reify {
    object C
  }.eval
}
