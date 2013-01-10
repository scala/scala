import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |object Betty {
    | List(1,2,3) count (_ % 2 != 0)
    | def f = List(1,2,3) filter (_ % 2 != 0) map (_ * 2)
    | def g = List(1,2,3) filter (_ % 2 == 0) map (_ * 3) map (_ + 1)
    |}
    |:javap -fun Betty#g
  """.stripMargin

  // three anonfuns of Betty#g
  override def yah(res: Seq[String]) = {
    def filtered = res filter (_ contains "public final class Betty")
    3 == filtered.size
  }
}
