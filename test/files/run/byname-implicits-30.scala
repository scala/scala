object Test {
  class Loop[T, U](self0: => Loop[T, U], swap0: => Loop[U, T]) {
    lazy val self: Loop[T, U] = self0
    lazy val swap: Loop[U, T] = swap0
  }

  object Loop {
    implicit def mkLoop[T, U](implicit tu: => Loop[T, U], ut: => Loop[U, T]): Loop[T, U] = new Loop(tu, ut)
  }

  def main(args: Array[String]): Unit = {
    val loop = implicitly[Loop[Int, String]]
    assert(loop.self eq loop)
    assert(loop.swap.self eq loop.swap)
    assert(loop.swap.swap eq loop)
  }
}
