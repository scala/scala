object Test extends App {
  implicit def foo: Int = macro Impls.foo
  def bar(implicit x: Int) = println(x)
  bar
}