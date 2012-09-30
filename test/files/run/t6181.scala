import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class C { def test(x: => Int) = println(x) }
  val mm = cm.reflect(new C).reflectMethod(typeOf[C].member(newTermName("test")).asMethod)
  mm(2)
}