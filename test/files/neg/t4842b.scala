class TypeArg[X](val x: X)(a: AnyRef) {
  def this() = { this(???)(new { println(TypeArg.this.x) } ); println("next") } // error
}
