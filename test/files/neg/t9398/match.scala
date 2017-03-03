class Test {
  // Should warn that CC(B2) isn't matched
  def test(c: CC): Unit = c match {
    case CC(B) => ()
  }
}
