import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.ClassTag

object R { override def toString = "R" }

class Foo{
 import Test._
 def foo = {
  val classTag = implicitly[ClassTag[R.type]]
  val sym = cm.objectSymbol(classTag.runtimeClass)
  val cls = cm.reflectObject(sym)
  cls.instance
 }
}

object Test extends App{
  val foo = new Foo
  println(foo.foo)
}