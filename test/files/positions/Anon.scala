object Anon {
  trait Foo {
	val bar : Int
  }

  def foo = new Foo {
	override val bar = 23
  }
}
