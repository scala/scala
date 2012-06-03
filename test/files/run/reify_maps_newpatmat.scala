import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    val colors = Map("red" -> 0xFF0000,
                     "turquoise" -> 0x00FFFF,
                     "black" -> 0x000000,
                     "orange" -> 0xFF8040,
                     "brown" -> 0x804000)
    for (name <- List("red", "green", "blue", "turquoise")) println(
      colors.get(name) match {
        case Some(code) =>
          name + " has code: " + code
        case None =>
          "Unknown color: " + name
      }
    )
  }.eval
}