import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    val msg = java.text.MessageFormat.format(
      "On {1} there was {2} on planet {0}.",
      "Hoth", "the fifth of August", "a disturbance in the Force")
    println("Message="+msg)
  }.eval
}