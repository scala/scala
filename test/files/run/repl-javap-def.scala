import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |def f = 7
    |:javap -public f
  """.stripMargin

  // it should find f wrapped in repl skins. replstiltskin.
  override def yah(res: Seq[String]) = {
    // replstiltskin: what be my name?
    val keywords = List("public", "class", "line")
    def isLineClass(s: String) = keywords forall (s contains _)
    def filtered = res filter isLineClass
    1 == filtered.size
  }
}
