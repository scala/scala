import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

case class Foo(x: Int) extends AnyVal
case class Bar(foo: Foo)

object Test extends App {
  val foo = typeOf[Bar].decl(TermName("foo")).asMethod
  println(foo.returnType) // Foo

  val bar = Bar(Foo(3))
  println(bar.foo) // Foo(3)

  val im = cm.reflect(bar)
  println(im.reflectField(foo).get) // incorrectly gives java.lang.Integer(3) not Foo(3)
  im.reflectField(foo).set(Foo(5)) // java.lang.IllegalArgumentException: Can not set int field Bar.foo to Foo
  println(im.reflectMethod(foo)()) // incorrectly gives java.lang.Integer(3) not Foo(3)
}