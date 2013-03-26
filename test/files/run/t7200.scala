import language.higherKinds

object Test extends App {

  // Slice of comonad is where this came up
  trait Foo[F[_]] {
    def coflatMap[A, B](f: F[A] => B): F[A] => F[B]
  }

  // A non-empty list
  case class Nel[A](head: A, tail: List[A])

  object NelFoo extends Foo[Nel] {

    // It appears that the return type for recursive calls is not inferred
    // properly, yet no warning is issued. Providing a return type or
    // type arguments for the recursive call fixes the problem.

    def coflatMap[A, B](f: Nel[A] => B) = // ok w/ return type
      l => Nel(f(l), l.tail match {
        case Nil => Nil
        case h :: t => {
          val r = coflatMap(f)(Nel(h, t)) // ok w/ type args
          r.head :: r.tail
        }
      })
  }

  // Without a recursive call all is well, but with recursion we get a
  // ClassCastException from Integer to Nothing
  NelFoo.coflatMap[Int, Int](_.head + 1)(Nel(1, Nil)) // Ok
  NelFoo.coflatMap[Int, Int](_.head + 1)(Nel(1, List(2))) // CCE

}
