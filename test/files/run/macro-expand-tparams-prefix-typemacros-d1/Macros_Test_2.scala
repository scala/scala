import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

object Test extends App {
  def typeOf[T: TypeTag] = ru.typeOf[T]
  def typeOf[T: TypeTag](x: T) = ru.typeOf[T]

  class D[T] {
    class C[U] {
      type Foo[V] = macro Impls.foo[T, U, V]
      class D1 extends Foo[Boolean]
      val x1: Foo[Boolean] = new D1
      println(typeOf[D1].baseClasses)
      println(typeOf(x1))
    }
  }

  val outer1 = new D[Int]
  new outer1.C[String]
}