class Foo {
  implicit def pair2fun2[A, B, C](f: (A, B) => C) =
     {p: (A, B) => f(p._1, p._2) }

  def foo(f: ((Int, Int)) => Int) = f
  def bar(x: Int, y: Int) = x + y

  foo({ (x: Int, y: Int) => x + y }) // works
  foo(pair2fun2(bar _)) // works
  foo(bar _) // error
  foo(bar)   // same error
}
