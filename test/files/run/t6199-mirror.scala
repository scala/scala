import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class C { def foo = () }
  println(cm.reflect(new C).reflectMethod(typeOf[C].member(TermName("foo")).asMethod)())
}