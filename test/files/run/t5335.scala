import scala.reflect.mirror._

object Test extends App {
  reify {
    println(new {def x = 2}.x)
  }.eval
}
