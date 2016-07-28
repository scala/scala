class EtaExpandZeroArg {
  def foo(): () => String = () => ""
  val f: () => Any = foo

  // f() would evaluate to <function0> instead of ""
}
