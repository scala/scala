trait OpacityTypes {
  type T
  def orderingT: Ordering[T]
}

object OpacityTypes {
  implicit def orderingT: Ordering[Test.pimp.T] = Test.pimp.orderingT
}

object Test extends App {
  val pimp: OpacityTypes = new OpacityTypes {
    override type T = Int
    override def orderingT = Ordering.Int
  }

  implicitly[Ordering[pimp.T]]
}
