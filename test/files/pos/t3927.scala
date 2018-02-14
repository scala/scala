object A {
  def x: Unit = {
    implicit lazy val e: Equiv[Int] = sys.error("")
    implicitly[Equiv[Int]]
  }
}
