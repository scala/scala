class Foo (x: AnyRef) {
  def this(x: Int) = this(new { println(Foo.this)}) // error
}

class TypeArg[X](val x: X)(a: AnyRef) {
  def this() = { this(???)(new { println(TypeArg.this.x) } ); println("next") } // error
}
