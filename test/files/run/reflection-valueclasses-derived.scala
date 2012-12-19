import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class C(val x: Int) extends AnyVal {
  def foo(y: Int) = x + y
}

object Test extends App {
  println(cm.reflect(new C(2)).reflectMethod(typeOf[C].member(TermName("foo")).asMethod)(2))
  println(cm.reflect(new C(2)).reflectMethod(typeOf[C].member(TermName("getClass")).asMethod)())
  println(cm.reflect(new C(2)).reflectMethod(typeOf[C].member(TermName("toString")).asMethod)())
}