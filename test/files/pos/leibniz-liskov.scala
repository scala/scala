trait LeibnizLiskov {
  type A // instead of picking some concrete type, use a totally unknown, abstract one
  type B
  type SA <: A
  type SB >: B

  implicitly[A =:= A]
  implicitly[B =:= B]
  def aEqB: A =:= B

  type SASub[+X] = SA <:< X
  (implicitly[B <:< SB].compose(aEqB.substituteCo[SASub](implicitly[SASub[A]]))): SA <:< SB
  (aEqB.substituteCo[SASub](implicitly[SASub[A]]).andThen(implicitly[B <:< SB])): SA <:< SB

  // checks that inference is working (no explicit types on xs)
  def A(): A
  def B(): B
  locally { val xs = aEqB.substituteCo     (List(A(), A(), A())); implicitly[xs.type <:< List[B]] }
  locally { val xs = aEqB.substituteContra (List(B(), B(), B())); implicitly[xs.type <:< List[A]] }
  locally { val xs = aEqB.flip.liftCo[List](List(B(), B(), B())); implicitly[xs.type <:< List[A]] }
  def convert1[T, U](l: List[T])(ev: T =:= U): List[U] = ev.substituteCo    (l)
  def convert2[T, U](l: List[U])(ev: T =:= U): List[T] = ev.substituteContra(l)

  implicitly[A <:< A]
  implicitly[B <:< B]
  val aSubB = { implicit val aEqB0 = aEqB     ; implicitly[A <:< B] }
  val bSubA = { implicit val bEqA0 = aEqB.flip; implicitly[B <:< A] }
  type From[X] = { type L[+Y] = X => Y }
  type To  [X] = { type L[-Y] = Y => X }
  locally { val f = bSubA.substituteCo    [From[A]#L](aSubB(_)); implicitly[f.type <:< (A => A)] }
  locally { val f = aSubB.substituteContra[To  [A]#L](bSubA(_)); implicitly[f.type <:< (A => A)] }
  def convertSub[T, U](l: List[T])(ev: T <:< U): List[U] = ev.liftCo[List](l)
  type Consumes[-X] = X => Unit
  def convertConsume1[U, T](c: Consumes[T])(ev: U <:< T): Consumes[U] = ev.substituteContra(c)
  def convertConsume2[U, T](c: Consumes[T])(ev: U <:< T): Consumes[U] = ev.liftContra[Consumes](c)
}
