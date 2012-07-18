trait Foo[@specialized(Int) A] {
  def foo(x: A): A
}

abstract class Inter extends Foo[Int]
