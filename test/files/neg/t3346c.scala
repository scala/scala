object Test extends App {
  //
  // An attempt to workaround SI-2712, foiled by SI-3346
  //
  trait TC[M[_]]

  type EitherInt[A] = Either[Int, A]

  implicit object EitherTC extends TC[EitherInt]

  def foo[M[_]: TC, A](ma: M[A]) = ()

  val eii: Either[Int, String] = Right("")

  foo[EitherInt, String](eii)

  // This one needs SI-2712 Higher Order Unification
  //foo(eii) // not inferred

  // A workaround is to provide a set of implicit conversions that take values
  // based on type constructors of various shapes, and search for the
  // type class instances.
  //
  // This is the approach taken by scalaz7.

  trait TCValue[M[_], A] {
    implicit def self: M[A]
    def M: TC[M]

    // instead of `foo(eii)`, we'll try `eii.foo`
    def foo[M[_], A] = ()
  }


  implicit def ToTCValue[M[_], A](ma: M[A])(implicit M0: TC[M]) = new TCValue[M, A] {
    implicit val M = M0
    val self = ma
  }
  implicit def ToTCValueBin1[M[_, _], A, B](ma: M[A, B])(implicit M0: TC[({type λ[α]=M[A, α]})#λ]): TCValue[({type λ[α] = M[A, α]})#λ, B] = new TCValue[({type λ[α]=M[A, α]})#λ, B] {
    implicit val M = M0
    val self = ma
  }
  implicit def ToTCValueBin2[M[_, _], A, B](ma: M[A, B])(implicit M0: TC[({type λ[α]=M[α, B]})#λ]): TCValue[({type λ[α]=M[α, B]})#λ, A] = new TCValue[({type λ[α]=M[α, B]})#λ, A] {
    implicit val M = M0
    val self = ma
  }


  ToTCValueBin1(eii).foo

  // as expected, could not find implicit parameter
  // ToTCValueBin2(eii).bar

  // error: implicit conversions are not applicable because they are ambiguous, both method ToTCValueBin1 ... and  method ToTCValueBin2
  //        annoying!!
  //        https://issues.scala-lang.org/browse/SI-3346
  //
  // Works if we remove ToTCValueBin2
  //
  eii.bar
}
