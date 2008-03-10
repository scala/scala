import scala.reflect.Manifest

object Test {
  def foo[T](x: T)(implicit m: Manifest[T]) {
    foo(List(x))
  }
  foo(1)
  foo("abc")
  foo(List(1, 2, 3))
  val x: List[Int] with Ordered[List[Int]] = null
  foo(x)
  foo[x.type](x)
  abstract class C { type T = String; val x: T }
  val c = new C { val x = "abc" }
  foo(c.x)
  abstract class D { type T; val x: T }
  val d: D = new D { type T = String; val x = "x" }
  foo(d.x)

}
