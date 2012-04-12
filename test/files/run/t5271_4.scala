import scala.reflect.mirror._

object Test extends App {
  reify {
    case object C
  }.eval
}
