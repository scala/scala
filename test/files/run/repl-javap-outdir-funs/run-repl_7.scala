import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |:javap -fun disktest/Foo.class
  """.stripMargin

  override def yah(res: Seq[String]) = {
    def filtered = res filter (_ contains "public final class disktest.Foo")
    1 == filtered.size
  }
}
