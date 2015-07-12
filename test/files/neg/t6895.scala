trait Foo[F1[F1_P]]
trait Bar[F2[F2_P]]

class Test {
  def barFoo[F3[F3_P]]: Foo[F3] = ???

  // Now we can define a couple of type aliases:
  type M[X1] = String
  type N[X2] = Bar[M]

  // val ok1: Foo[N] = barFoo
  // Foo[?F3] <:< Foo[Test.this.N]
  //   [X2]Test.this.N[X2] <:< [F3_P]?F3[F3_P]
  //      Test.this.N[X2] <:< ?F3[X2]
  //        true, ?F3=N

  // val ok2: Foo[({type L[X] = Bar[M]})#L] = barFoo[N]

  val nok: Foo[({type L[X3] = Bar[M]})#L] = barFoo /* Type inference can't unify F with L */
  // Foo[?F3] <:< Foo[[X3]Bar[[X1]String]]
  //   [X3]Bar[[X1]String] <:< ?F3
  //     [X3]Bar[[X1]String] <:< [F3_P]?F3[F3_P]
  //        Bar[[X1]String] <:< ?F3[X3]
  //          X3 <:< [X1]String
  //            false
}
