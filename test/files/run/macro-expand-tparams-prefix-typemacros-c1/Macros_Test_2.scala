import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class D[T] {
  class C[U] {
    type Foo[V] = macro Impls.foo[T, U, V]
  }
}

object Test extends App {
  val outer1 = new D[Int]
  val outer2 = new outer1.C[String]
  class D1 extends outer2.Foo[Boolean]
  val x1: outer2.Foo[Boolean] = new D1

  def typeOf[T: TypeTag] = ru.typeOf[T]
  def typeOf[T: TypeTag](x: T) = ru.typeOf[T]

  println(typeOf[D1].baseClasses)
  println(typeOf(x1))
}