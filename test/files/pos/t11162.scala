class Ops[X] {
  type T
}

object Meh {
  // unapply result type depends on an implicit arg
  def unapply[X](i: Int)(implicit ops: Ops[X]): Option[ops.T] = None
}

class Test {
  def foo[X](implicit oops: Ops[X]): Unit = {
    /* error: error during expansion of this match (this is a scalac bug).
        The underlying error was: type mismatch;
         found   : oops.T
         required: ops.T
    */
    def bar() = 1 match {
      case Meh(z) => z
    }

    bar()
  }
}
