object X {
  def bar = {
    def foo(x: Any) = ""
    val foo = foo(null)
    foo(null) // cycle in isApplicableBasedOnArity
  }
}
