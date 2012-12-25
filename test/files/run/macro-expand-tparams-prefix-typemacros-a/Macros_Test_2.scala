import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

object Test extends App {
  class C[T] {
    type Foo[U](x: U) = macro Impls.foo[U]
  }

  def typeOf[T: TypeTag] = ru.typeOf[T]
  def typeOf[T: TypeTag](x: T) = ru.typeOf[T]

  val prefix1 = new C[Int]()
  class D1 extends prefix1.Foo(42)
  val x1: prefix1.Foo(42) = new D1
  println(typeOf[D1].baseClasses)
  println(typeOf(x1))

  val prefix2 = new C[Boolean]()
  class D2 extends prefix2.Foo(42)
  val x2: prefix2.Foo(42) = new D2
  println(typeOf[D2].baseClasses)
  println(typeOf(x2))

  val prefix3 = new C[Int]()
  class D3 extends prefix3.Foo("42")
  val x3: prefix3.Foo("42") = new D3
  println(typeOf[D3].baseClasses)
  println(typeOf(x3))

  val prefix4 = new C[String]()
  class D4 extends prefix4.Foo(true)
  val x4: prefix4.Foo(true) = new D4
  println(typeOf[D4].baseClasses)
  println(typeOf(x4))
}