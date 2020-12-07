package tastytest

/** test references to type parameters in an opaque type alias */
object Vecs {

  opaque type Vec[+A] = List[A]

  object Vec {
    def single[A](elem: A): Vec[A] = elem :: Nil

    final implicit class VecOps[A](val vec: Vec[A]) extends AnyVal {
      def safeHead = vec.head
    }
  }

}
