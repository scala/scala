
//> using options -Yrangepos
//
trait A
trait B

class Foo[A2, B2 <: A2] {
  class Bar
  object Bar {
    implicit def fromString(a: String) = new Bar
  }
  def andThen(b : Bar) = b
}

object Test {
  def lub = if (true) (null: Foo[A, A]) else (null: Foo[B, B])
  (lub) andThen ("Bar")
}
