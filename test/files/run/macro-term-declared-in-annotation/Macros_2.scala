class foo(val bar: String) extends annotation.StaticAnnotation

object Api {
  // foo in ann must have a different name
  // otherwise, we get bitten by https://github.com/scala/bug/issues/5544
  @foo({def fooInAnn = macro Impls.foo; fooInAnn})
  def foo = println("it works")
}