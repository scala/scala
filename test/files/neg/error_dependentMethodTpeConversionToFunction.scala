// test DependentMethodTpeConversionToFunctionError
object Test {
  def foo(x: AnyRef): x.type = x
  val x: Any => Any = foo
}