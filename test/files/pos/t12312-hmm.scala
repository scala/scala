package hmm

// Taken from https://github.com/typelevel/kind-projector/blob/7ad46d6ca995976ae2ff18215dbb32cd7ad0dd7a/src/test/scala/hmm.scala
// As a regression test for the issue spotted in https://github.com/scala/community-build/pull/1400

class TC[A]

object TC {
  def apply[A]: Unit = ()
}

object test {

  sealed trait HList extends Product with Serializable
  case class ::[+H, +T <: HList](head : H, tail : T) extends HList
  sealed trait HNil extends HList
  case object HNil extends HNil

  TC[Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
   Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
   Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil]

  TC[Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil]

  TC[Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil]
}
