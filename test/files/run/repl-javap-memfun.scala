import scala.tools.partest.JavapTest
import scala.tools.nsc.Settings

// see repl-javap-lambdas.scala for the complementary version
object Test extends JavapTest {
  // asserting the default
  override def transformSettings(s: Settings) = { s.Ydelambdafy.value = "inline" ; s }
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
