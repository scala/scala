package foo {
  trait Abstract1[C <: Abstract2[C]]
  trait Abstract2[C <: Abstract2[C]] extends Abstract1[C]
  class Parametrized1[T] extends Abstract1[Parametrized2[T]] {
    def bar(a: AnyRef) { a match { case d: Parametrized1[_] => println("ok") } }
  }
  class Parametrized2[T] extends Parametrized1[T] with Abstract2[Parametrized2[T]]
}
