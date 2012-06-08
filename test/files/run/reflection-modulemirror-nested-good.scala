import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.ClassTag

class Foo{
 import Test._
 def foo = {
  val classTag = implicitly[ClassTag[R.type]]
  val sym = cm.moduleSymbol(classTag.runtimeClass)
  val cls = cm.reflectModule(sym)
  try {
    cls.instance
  } catch {
    case ex: Throwable =>
      println(ex.getMessage)
  }
 }
}

object Test extends App{
  object R { override def toString = "R" }
  val foo = new Foo
  println(foo.foo)
}