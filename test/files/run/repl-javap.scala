import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |case class Betty(i: Int) { def next = Betty(i+1) }
    |:javap Betty
  """.stripMargin

  override def yah(res: Seq[String]) = {
    val r = """public class \S*Betty""".r.unanchored
    def filtered = res filter { case r(_*) => true ; case _ => false }
    1 == filtered.size
  }
}
