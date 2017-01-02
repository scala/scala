object LeibnizLiskov {
  val isInt = implicitly[Int =:= Int]

  isInt.substitute(List(1, 2, 3))

  def convertInt1[T, U](l: List[T])(ev: T =:= U): List[U] =
    ev.substitute(l)

  def convertInt2[T, U](l: List[T])(ev: T =:= U): List[U] =
    ev.liftTo[List](l)

  val isIntSub = implicitly[Int <:< Int]

  isIntSub.liftCo(List(1, 2, 3))

  def convertIntSub[T, U](l: List[T])(ev: T <:< U): List[U] =
    ev.liftCo[List](l)

  trait Consumes[-T] {
    def consume(t: T): Unit = ()
  }

  isIntSub.liftContra(new Consumes[Int] { }): Consumes[Int]
  isIntSub.substitute(new Consumes[Int] { }): Consumes[Int]

  def convertConsume1[U, T](c: Consumes[T])(ev: U <:< T): Consumes[U] =
    ev.substitute(c)

  def convertConsume2[U, T](c: Consumes[T])(ev: U <:< T): Consumes[U] = {
    val ev2 = ev.liftContra[Consumes]
    ev2(c)
  }
}
