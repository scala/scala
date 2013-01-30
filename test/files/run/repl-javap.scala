import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |case class Betty(i: Int) { def next = Betty(i+1) }
    |:javap Betty
  """.stripMargin

  override def yah(res: Seq[String]) = {
    def filtered = res filter (_ contains "public class Betty")
    1 == filtered.size
  }
}
