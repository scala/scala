trait Foo[F[_]]
trait Bar[F[_], A]

trait Or[A, B]

class Test {
  implicit def orFoo[A]: Foo[({type L[X] = Or[A, X]})#L] = ???
  implicit def barFoo[F[_]](implicit f: Foo[F]): Foo[({type L[X] = Bar[F, X]})#L] = ???

  // Now we can define a couple of type aliases:
  type StringOr[X] = Or[String, X]
  type BarStringOr[X] = Bar[StringOr, X]

  // ok
  implicitly[Foo[BarStringOr]]
  barFoo[StringOr](null) : Foo[BarStringOr]
  barFoo(null) : Foo[BarStringOr]

  // nok
  implicitly[Foo[({type L[X] = Bar[StringOr, X]})#L]]
  // Let's write the application explicitly, and then
  // compile with just this line enabled and -explaintypes.
  barFoo(null) : Foo[({type L[X] = Bar[StringOr, X]})#L]

  // Foo[[X]Bar[F,X]] <: Foo[[X]Bar[[X]Or[String,X],X]]?
  //   Bar[[X]Or[String,X],X] <: Bar[F,X]?
  //     F[_] <: Or[String,_]?
  //     false
  //   false
  // false

  // Note that the type annotation above is typechecked as
  // Foo[[X]Bar[[X]Or[String,X],X]], ie the type alias `L`
  // is eta expanded.
  //
  // This is done so that it does not escape its defining scope.
  // However, one this is done, higher kinded inference
  // no longer is able to unify F with `StringOr` (SI-2712)
}
