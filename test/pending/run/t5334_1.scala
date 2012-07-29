import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    class C { override def toString = "C" }
    new C
  }.eval
}