import scala.language.higherKinds

object Macros {
  def foo1(x: Int): Int = macro Impls.foo
  def foo2[T](x: Int): Int = macro Impls.foo
  def foo3[T, U](x: Int): Int = macro Impls.foo
  def foo4[T[_]](x: Int): Int = macro Impls.foo
  def foo5[T[U[_]]](x: Int): Int = macro Impls.foo
}

object Test extends App {
  import Macros._
  foo1[String](42)
  foo2[String, String](42)
  foo3[String](42)
  foo4[String](42)
  foo5[List](42)
}