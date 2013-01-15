import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

object Test extends App {
  class C[T] {
    type Foo[U](x: U) = macro Impls.foo[T, U]
  }
  object D extends C[Boolean]

  def typeOf[T: TypeTag] = ru.typeOf[T]
  def typeOf[T: TypeTag](x: T) = ru.typeOf[T]

  val prefix1 = new C[Int]()
  class D1 extends D.Foo(42)
  val x1: D.Foo(42) = new D1
  println(typeOf[D1].baseClasses)
  println(typeOf(x1))

  val prefix2 = new C[Boolean]()
  class D2 extends D.Foo("42")
  val x2: D.Foo("42") = new D2
  println(typeOf[D2].baseClasses)
  println(typeOf(x2))
}