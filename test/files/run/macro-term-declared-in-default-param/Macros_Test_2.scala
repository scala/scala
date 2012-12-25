object Test extends App {
  def foo(bar: String = { def foo = macro Impls.foo; foo }) = println(bar)

  foo()
  foo("it works")
  foo()
}