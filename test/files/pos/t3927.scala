object A {
  def x {
    implicit lazy val e: Equiv[Int] = sys.error("")
    implicitly[Equiv[Int]]
  }
}
