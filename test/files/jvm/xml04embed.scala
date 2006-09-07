object Test {

  import scala.testing.UnitTest._
  import scala.xml._

  def main(args:Array[String]) = {
    val ya = <x>{{</x>
    assertEquals(ya.text, "{");

    val ua = <x>}}</x>
    assertEquals(ua.text, "}");

    val za = <x>{{}}{{}}{{}}</x>
    assertEquals(za.text, "{}{}{}");

  }
}
