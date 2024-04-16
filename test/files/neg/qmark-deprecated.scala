//> using options -deprecation -Xfatal-warnings
//

class Foo[?] // error
class Foo2[`?`] // ok
class Bar[M[?] <: List[?]] // error on the definition
class Bar2[M[`?`] <: List[`?`]] // ok

object G {
  class ? { val x = 1 } // error
}
object G2 {
  class `?` { val x = 1 } // ok
}
object H {
  trait ? // error
}
object H2 {
  trait `?` // ok
}
object I {
  type ? = Int // error
}
object I2 {
  type `?` = Int // ok

  val x: Array[?] = new Array[?](0) // no error reported here because we stop before running typer
  val y: Array[`?`] = new Array[`?`](0) // ok

  def foo1[T <: Array[?]](x: T): Array[?] = x // ok
  def foo2[T <: Array[`?`]](x: T): Array[`?`] = x // ok

  def bar1[?] = {} // error
  def bar2[`?`] = {} // ok
  def bar3[M[?]] = {} // error
  def bar4[M[`?`]] = {} // error

  type A[?] = Int // error
  type B[`?`] = Int // ok
}
