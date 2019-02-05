trait Generic[T] {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}

object Generic {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }
}

object ListInstances {
  type LRepr[T] = Either[::[T], Either[Nil.type, Unit]]
  type CRepr[T] = (T, (List[T], Unit))
  type NRepr = Unit

  implicit def genList[T]: Generic.Aux[List[T], LRepr[T]] = new Generic[List[T]] {
    type Repr = LRepr[T]
    def to(t: List[T]): Repr = t match {
      case hd :: tl => Left(::(hd, tl))
      case n@Nil => Right(Left(Nil))
    }
    def from(r: Repr): List[T] = (r: @unchecked) match {
      case Left(c) => c
      case Right(Left(n)) => n
    }
  }

  implicit def genCons[T]: Generic.Aux[::[T], CRepr[T]] = new Generic[::[T]] {
    type Repr = CRepr[T]
    def to(t: ::[T]): Repr = (t.head, (t.tail, ()))
    def from(r: Repr): ::[T] = ::(r._1, r._2._1)
  }

  implicit def genNil: Generic.Aux[Nil.type, NRepr] = new Generic[Nil.type] {
    type Repr = NRepr
    def to(t: Nil.type): Repr = ()
    def from(r: Repr): Nil.type = Nil
  }
}

trait Show[T] {
  def show(t: T): String
}

object Show {
  def apply[T](implicit st: => Show[T]): Show[T] = st

  implicit def showUnit: Show[Unit] = new Show[Unit] {
    def show(u: Unit): String = "()"
  }

  implicit def showInt: Show[Int] = new Show[Int] {
    def show(i: Int): String = i.toString
  }

  implicit def showPair[T, U](implicit st: Show[T], su: Show[U]): Show[(T, U)] = new Show[(T, U)] {
    def show(t: (T, U)): String = s"(${st.show(t._1)}, ${su.show(t._2)}"
  }

  implicit def showEither[T, U](implicit st: Show[T], su: Show[U]): Show[Either[T, U]] = new Show[Either[T, U]] {
    def show(t: Either[T, U]): String = t match {
      case Left(t) => s"Left(${st.show(t)})"
      case Right(u) => s"Right(${su.show(u)})"
    }
  }

  implicit def showGen[T, R](implicit gen: Generic.Aux[T, R], sr: => Show[R]): Show[T] = new Show[T] {
    def show(t: T) = sr.show(gen.to(t))
  }
}

object Test extends App {
  import ListInstances._
  val sl = Show[List[Int]]
  assert(sl.show(List(1, 2, 3)) == "Left((1, (Left((2, (Left((3, (Right(Left(())), ()), ()), ())")
}
