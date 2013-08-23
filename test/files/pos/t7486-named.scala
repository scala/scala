
object Test {
  def fold(empty: Any) = ()
  implicit val notAnnotatedImplicit = new {
    fold(empty = 0)
    def empty[A]: Any = ???
  }
}
