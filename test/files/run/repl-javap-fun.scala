import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |object Betty {
    | List(1,2,3) filter (_ % 2 != 0) map (_ * 2)
    |}
    |:javap -fun Betty
  """.stripMargin

  // two anonfuns of Betty
  override def yah(res: Seq[String]) = {
    def filtered = res filter (_ contains "public final class Betty")
    2 == filtered.size
  }
}
