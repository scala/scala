//> using options -Werror

sealed trait Trait1
sealed trait Trait2

class Class1 extends Trait1
class Class2 extends Trait2

object Test extends App {
  def test(x: Trait1): Unit =
    x match {
      case y: Trait2 =>
      case _ =>
    }
}
