object Test { // PolyTYped function default arg unicity check,
              // fails in 2.11, authorized under -Xsource:2.10
  def foo(printer: Any, question: => String, show: Boolean = false)(op: => Any): Any = ???
  def foo[T](question: => String, show: Boolean)(op: => Any = ()): Any = ???
}
