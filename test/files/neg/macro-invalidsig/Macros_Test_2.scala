object Macros1 {
  def foo[U]: Int = macro Impls1.foo[U]
}

object Macros2 {
  def foo = macro Impls2.foo
}

object Macros3 {
  def foo = macro Impls3.foo
}

object Macros4 {
  def foo = macro Impls4.foo
}

object Macros5 {
  def foo(x: Any) = macro Impls5.foo
}

class Macros6[T] {
  def foo[U](x: Int) = macro Impls6.foo[T, U]
}

object Macros7 {
  def foo(x: Int) = macro Impls7.foo
}

object Macros8 {
  def foo(x: Int) = macro Impls8.foo
}

object Macros9 {
  def foo(x: Int, y: Int) = macro Impls9.foo
}

object Macros10 {
  def foo(x: Int, y: Int) = macro Impls10.foo
}

object Macros11 {
  def foo[U] = macro Impls11.foo[U]
}

object Macros12 {
  def foo[U] = macro Impls12.foo[U]
}

object Macros13 {
  def foo[U <: Int] = macro Impls13.foo[U]
}

object Macros14 {
  def foo = macro Impls14.foo
}

class D[T] {
  class C[U] {
    def foo15[V]: Unit = macro Impls15.foo
    def foo16[V]: Unit = macro Impls16.foo[V]
  }
}

object Test extends App {
  println(Macros1.foo[String])
  println(Macros2.foo)
  println(Macros3.foo)
  println(Macros4.foo)
  println(Macros5.foo(42))
  println(new Macros6[Int]().foo[String](42))
  println(Macros7.foo(42))
  println(Macros8.foo)
  println(Macros9.foo(4, 2))
  println(Macros10.foo(4, 2))
  println(Macros11.foo[Int])
  println(Macros12.foo[Int])
  println(Macros13.foo[Int])
  println(Macros14.foo)
  val outer1 = new D[Int]
  val outer2 = new outer1.C[String]
  outer2.foo15[Boolean]
  outer2.foo16[Boolean]
}