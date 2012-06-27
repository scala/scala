import scala.collection.{ mutable, immutable, generic, GenTraversableOnce }

package object foo {
  @inline implicit class TravOps[A, CC[A] <: GenTraversableOnce[A]](val coll: CC[A]) extends AnyVal {
    def build[CC2[X]](implicit cbf: generic.CanBuildFrom[Nothing, A, CC2[A]]): CC2[A] = {
      cbf() ++= coll.toIterator result
    }
  }
}

package foo {
  object Test {
    def f1[T](xs: Traversable[T]) = xs.to[immutable.Vector]
    def f2[T](xs: Traversable[T]) = xs.build[immutable.Vector]
  }
}
