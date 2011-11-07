object A {
  def x {
    implicit lazy val e: Equiv[Int] = error("")
    implicitly[Equiv[Int]]
  }
} 
