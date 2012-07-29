import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    class C { override def toString() = "C" }
    List((new C, new C))
  }.eval
}