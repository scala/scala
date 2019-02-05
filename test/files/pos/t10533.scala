object Foo {
  val b @ Bar(_) = Bar(1)(2)(3)
}

case class Bar(a: Int)(b: Int)(c: Int)
