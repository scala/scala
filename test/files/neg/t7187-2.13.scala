class EtaExpandZeroArg {
  def foo() = ""
  val f: () => Any = foo   // error (not eta-expanded)
  val g: () => Any = foo _ // ok
  val h = foo _            // error (not eta-expanded)
}
