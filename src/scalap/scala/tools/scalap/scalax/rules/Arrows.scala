package scala.tools.scalap
package scalax
package rules

trait Arrows extends UnitFunctors  {
  type Arr[-A, +B] <: Arrow[A, B]
  type M[+B] = Arr[Nothing, B]

  def arrow[A, B](f : A => B) : Arr[A, B]
  def diag[A] = arrow[A, (A, A)] { a => (a, a) }

  override def unit[B](b : => B) : M[B] = arrow { any : Any => b }

  trait Arrow[-A, +B] extends Functor[B] { this : Arr[A, B] =>

    def map[C](f : B => C) = comp(arrow(f))
    def comp[C](bc : => Arr[B, C]) : Arr[A, C]
    def fst[C] : Arr[(A, C), (B, C)]
  }
}

trait ApplicativeArrows extends Arrows {
  type Arr[-A, +B] <: ApplicativeArrow[A, B]

  def app[A, B] : Arr[(Arr[A, B], A), B]

  trait ApplicativeArrow[-A, +B] extends Arrow[A, B] { self : Arr[A, B] =>
    def flatMap[SubA <: A, C](f : B => Arr[SubA, C]) : Arr[SubA, C] =
      diag[SubA].comp(map(f).fst[SubA]).comp(app[SubA, C])
  }
}

trait ArrowMonads extends ApplicativeArrows with Monads {
  type Arr[-A, +B] <: ApplicativeArrow[A, B] with Monad[B]

  override def unit[A](a : => A) : M[A] = arrow[Unit, A](Unit => a)
}
