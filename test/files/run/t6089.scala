case class Foo(x: Int)

object Test {
  def bippo(result: Boolean): Boolean = result
  def bungus(m: Foo): Boolean         =
    bippo((m: @unchecked) match { case Foo(2) => bungus(m) })

  def main(args: Array[String]): Unit = try {
    bungus(Foo(0))
  } catch {
    case x: MatchError => println(x)
  }
}