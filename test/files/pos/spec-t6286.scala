trait Foo[@specialized(Int) A] {
  def fun[@specialized(Int) B](init: B)(f: (B, A) => B): B
}

class Bar(values: Array[Int]) extends Foo[Int] {
  def fun[@specialized(Int) C](init: C)(f: (C, Int) => C): C = {
    val arr = values
    f(init, arr(0))
  }
}
