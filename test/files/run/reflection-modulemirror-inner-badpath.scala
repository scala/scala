import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.ClassTag

class Foo{
 object R { override def toString = "R" }
 def foo = {
  val classTag = implicitly[ClassTag[R.type]]
  val sym = cm.moduleSymbol(classTag.runtimeClass)
  try {
    val cls = cm.reflectModule(sym)
    cls.instance
    println("this indicates a failure")
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