object Test {
  def foo[A](a: A) = new { def bar(x: A): A = x }
}
