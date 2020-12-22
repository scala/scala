object Test {
  sealed trait AbstractProven {
    type Proven[+A, TypeClass[_]] <: A
  }

  val abstractProven: AbstractProven = new AbstractProven {
    override type Proven[+A, TypeClass[_]] = A
  }

  import abstractProven._
  def x(a: Any Proven Ordering): Unit = a match {
    case i: Int =>
  }
}
