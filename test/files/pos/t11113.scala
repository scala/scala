object Foo {
  implicit class IntOps(self: Int) { def apply[T] = self }
  val s = 1234[Int]
}
