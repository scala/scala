object Macros1 {
  def foo[U <: String]: Unit = macro Impls1.foo[U]
}

object Macros2 {
  def foo[T <: D]: Unit = macro Impls2.foo[T]
}

object Test extends App {
  Macros1.foo[String]
  Macros2.foo[D]
}
