trait A { self: Any { def p: Any } =>
  def f(b: => Unit) {}
  f { p }
}
