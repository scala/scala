//> using options -Werror

// aka t12438.scala

sealed trait SealedTrait
class SomeClass
trait UnsealedTrait

sealed abstract class O
class O1 extends O

class Test {
  def a(c: Option[Int]) = c match { case _: SealedTrait =>; case _ =>  }
  def b(c: Option[Int]) = c match { case _: SomeClass =>; case _ =>  }
  def c(c: Option[Int]) = c match { case _: UnsealedTrait =>; case _ =>  }
  // O1 is not final , so there could be a value of type O1 with UnsealedTrait
  def nowarn(c: O) = c match { case _: UnsealedTrait =>; case _ =>  }
}
