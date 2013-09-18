object Test {
  case class A(l: List[_]*)

  def main(args: Array[String]): Unit = {
    /** Kind of sneaking a slightly different test in here as well as
     *  testing 2867.  How subversive.
     */
    val xs1 = List(1, 2, 3)
    val xs2 = List(1.0, 2.0, 3.0)
    val xs3 = List[Any](1.0f, 2.0f, 3.0f)
    val xs4 = List[Byte](1, 2, 3)

    assert(A(List(xs1, xs2)) == A(List(xs3, xs4)))
  }
}
