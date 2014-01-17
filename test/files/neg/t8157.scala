object Test {
  def foo(printer: Any, question: => String, show: Boolean = false)(op: => Any): Any = ???
  def foo[T](question: => String, show: Boolean)(op: => Any = ()): Any = ???
}
