import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.ClassTag

class Foo{
 object R { override def toString = "R" }
 def foo = {
  val classTag = implicitly[ClassTag[R.type]]
  val sym = cm.moduleSymbol(classTag.runtimeClass)
  val cls = cm.reflect(this).reflectModule(sym)
  try {
    cls.instance
  } catch {
    case ex: Throwable =>
      println(ex.getMessage)
  }
 }
}

object Test extends App{
  val foo = new Foo
  println(foo.foo)
}