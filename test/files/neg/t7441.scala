object Test {
  object Bar {
    def apply(xs: List[Any]): Int = 0
    def test = apply(1)
  }
  implicit def foo = 1
}
