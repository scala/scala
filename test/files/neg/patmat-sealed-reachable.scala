// scalac: -Xfatal-warnings

sealed trait SealedTrait
class SomeClass
trait UnsealedTrait

sealed abstract class O
class O1 extends O
final class O2 extends UnsealedTrait
sealed class O3
class O4 extends O3 with UnsealedTrait

class Test {

  def a(c: Option[Int]) = c match { case _: SealedTrait => ; case _ => }
  def b(c: Option[Int]) = c match { case _: SomeClass => ; case _ => }
  def c(c: Option[Int]) = c match { case _: UnsealedTrait => ; case _ => }

  //   O1 is not final , so there could be a value of type O1 with UnsealedTrait
  def nowarn1(c: O) = c match { case _: UnsealedTrait => ; case _ => }

  // maybe there a subclass which mix with UnsealedTrait in domain
  def nowarn2(c: O3) = c match { case _: UnsealedTrait => ; case _ => }
  def nowarn3(c: O4) = c match { case _: UnsealedTrait => ; case _ => }


}