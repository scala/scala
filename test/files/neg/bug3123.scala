object NotAValue {
  def test[T](t : T) {
    t match { case Int => true }
  }
}
