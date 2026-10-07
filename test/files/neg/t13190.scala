package hmm

class TC[A]

object TC {
  def apply[A]: Unit = ()
}

object test {

  sealed trait HList extends Product with Serializable
  case class ::[+H, +T <: HList](head : H, tail : T) extends HList
  sealed trait HNil extends HList
  case object HNil extends HNil

  TC[Int :: () + Int :: HNil]
}
