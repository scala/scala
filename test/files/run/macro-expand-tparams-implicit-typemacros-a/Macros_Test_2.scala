import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

object Test extends App {
  type Foo[T](x: T) = macro Impls.foo[T]

  class D extends Foo(42)
  val x: Foo(42) = new D

  def typeOf[T: TypeTag] = ru.typeOf[T]
  def typeOf[T: TypeTag](x: T) = ru.typeOf[T]

  println(typeOf[D].baseClasses)
  println(typeOf(x))
}