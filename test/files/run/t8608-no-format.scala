
import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |f"hello, world"
    |:javap -prv -
  """.stripMargin

  // no format
  override def yah(res: Seq[String]) = {
    // note: avoid the word "information"
    res forall (!_.contains("StringOps.format"))
  }
}
