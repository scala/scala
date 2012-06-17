class Foo (x: AnyRef) {
  def this(x: Int) = this(new { println(Foo.this)}) // error
}
