trait LeibnizLiskov {
  type A // instead of picking some concrete type, use a totally unknown, abstract one
  type B
  type SA <: A
  type SB >: B

  implicitly[A =:= B]
  implicitly[B =:= A]
  def aEqB: A =:= B

  implicitly[A <:< SA]
  implicitly[SB <:< B]
  implicitly[SA <:< B]
  implicitly[A <:< SB]

  def A(): A
  def B(): B
  aEqB.substituteCo    (List(B(), B(), B()))
  aEqB.substituteContra(List(A(), A(), A()))
  locally { val xs = aEqB.flip.liftCo[List](List(B(), B(), B())); implicitly[xs.type <:< List[B]] }
  def convert1[T, U](l: List[T])(ev: T =:= U): List[U] = ev.substituteContra(l)
  def convert2[T, U](l: List[U])(ev: T =:= U): List[T] = ev.substituteCo(l)

  implicitly[A <:< A]
  implicitly[B <:< B]
  val aSubB: A <:< B = aEqB
  val bSubA: B <:< A = aEqB.flip
  type From[X] = { type L[+Y] = X => Y }
  type To  [X] = { type L[-Y] = Y => X }
  locally { val f = bSubA.substituteCo    [To  [A]#L](aSubB(_)); implicitly[f.type <:< (A => A)] }
  locally { val f = aSubB.substituteContra[From[A]#L](bSubA(_)); implicitly[f.type <:< (A => A)] }
  def convertSub[T, U](l: List[T])(ev: T <:< U): List[U] = ev.liftContra[List](l)
  type Consumes[-X] = X => Unit
  def convertConsume1[U, T](c: Consumes[T])(ev: U <:< T): Consumes[U] = ev.liftCo[Consumes](c)
  def convertConsume2[U, T](c: Consumes[T])(ev: U <:< T): Consumes[U] = ev.substituteCo(c)
}
