//> using options -Werror

// this is remodeling of the scala package object and scala.collection.immutable.{ List, ::, Nil }
// in order to:
// * avoid the scala package, which is auto-imported
// * avoid List, which is rewritten/fudged in the pattern matcher
package skala.collect {
  sealed trait Xs[+A]
  final case class  Cons[+A](head: A, tail: Xs[A]) extends Xs[A]
  final case object Done                           extends Xs[Nothing]
  object Xs
}
package object skala {
  type Cons[+A]                     = skala.collect.Cons[A]
  type Xs[+A]                       = skala.collect.Xs[A]
  val Cons                          = skala.collect.Cons
  val Done: skala.collect.Done.type = skala.collect.Done
  val Xs                            = skala.collect.Xs
}

import skala._

class Test {
  def test(xs: Xs[Int]): Boolean = xs match {
    case Cons(_, _)                 => true
    case _: Done.type               => false
  //case _: skala.collect.Done.type => false // done this way it already works
  }
}
