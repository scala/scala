import scala.reflect.mirror._

object Test extends App {
  reify {
    println(new {def x = 2; def y = x * x}.y)
  }.eval
}
