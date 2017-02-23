object Test {
  trait Inf {
    def infix_===[T](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
    abstract class Rep[T] {
      def reify: T
    }
    def unit[T](const: T): Rep[T]
    implicit def foo(x: Int): Iterable[(Int, Int)]
  }
  trait Imp {
    abstract class Equalizer(lhs: Any) {
      def ===(rhs: Any): Option[String]
    }
    implicit def equalizer(x: Any): Equalizer
  }

  def assert(x: Option[String]) {}
  def assert(x: Boolean) {}

  trait Prog extends Inf with Imp {
    def test(x: Rep[Int]) = {
      assert(x.reify === Iterable(1 -> 2, 3 -> 4))
    }
  }
}
