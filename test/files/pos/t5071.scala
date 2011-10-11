// abstract
trait Foo[@specialized A, Repr] {
  self: Repr =>
}
trait Bar[A] extends Foo[A, Object] { }
class Baz extends Foo[Int, Baz] { }

// concrete
trait Bippy {
  def f(x: Int) = 5
}
trait FooC[@specialized A] {
  self: Bippy =>

  f(10)
}

class BazC extends FooC[Int] with Bippy { }
