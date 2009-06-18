class Foo[@specialized T] {
  var v: T = _

  def foo(x: T): T = x

  println("abc")

  class Bar[@specialized U] {
    def bar(x: U): T = v
//    def barInt(x: Int): T = bar(x.asInstanceOf[U])
  }
}

class Test {
  def test {
    val a = new Foo[Int]
    val b = new a.Bar[Int]
    a.foo(10)
    b.bar(11)
  }
}

/*
abstract class Foo[@specialized T] {
  def foo(x: T): T
  def foo$Int(x: Int): Int

  abstract class Bar[@specialized U] {
    def bar(x: U): T
    def bar$Int(x: Int): T
  }
  abstract class Bar$Int extends Bar[Int] {
    def bar(x: Int): T = bar$Int(x)
    def bar$Int(x: Int): T
  }
}

abstract class Foo$Int extends Foo[Int] {
  def foo(x: Int): Int = foo$Int(x)
  def foo$Int(x: Int): Int

  abstract class Bar[@specialized U] {
    def bar(x: U): Int
    def bar$Int(x: Int): Int
  }
  abstract class Bar$Int extends Bar[Int] {
    def bar(x: Int): Int = bar$Int(x)
    def bar$Int(x: Int): Int
  }
}
*/
