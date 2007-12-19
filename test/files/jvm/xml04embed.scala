import scala.testing.SUnit._

object Test extends AnyRef with Assert {
  def main(args: Array[String]) {
    val ya = <x>{{</x>
    assertEquals(ya.text, "{")

    val ua = <x>}}</x>
    assertEquals(ua.text, "}")

    val za = <x>{{}}{{}}{{}}</x>
    assertEquals(za.text, "{}{}{}")

  }
}
