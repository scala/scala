object Test {
  def foo(i: Int, l: Long): Unit = {
    val i_f: Float = i  // deprecated
    val i_d: Double = i // OK
    val l_f: Float = l  // deprecated
    val l_d: Double = l // deprecated
  }

  def imp: Unit = {
    implicitly[Int => Float]   // deprecated
    implicitly[Int => Double]  // OK
    implicitly[Long => Float]  // deprecated
    implicitly[Long => Double] // deprecated
  }
}
