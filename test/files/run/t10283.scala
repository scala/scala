//> using options -Xsource:2.13
//
trait OpacityTypes {
  type T
  def orderingT: Ordering[T]
}

object OpacityTypes {
  implicit def orderingT: Ordering[Test.extension.T] = Test.extension.orderingT
}

object Test extends App {
  val extension: OpacityTypes = new OpacityTypes {
    override type T = Int
    override def orderingT = Ordering.Int
  }

  implicitly[Ordering[extension.T]]
}
