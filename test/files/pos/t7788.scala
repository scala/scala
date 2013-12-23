object Foo {
  def conforms = 0

  implicitly[Int => Int]
}
