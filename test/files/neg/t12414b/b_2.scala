//> using options -Werror

object Test extends App {
  def test(x: Trait1): Unit =
    x match {
      case y: Trait2 =>
      case _ =>
    }
}
