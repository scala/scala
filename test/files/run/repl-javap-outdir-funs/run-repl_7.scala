import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |:javap -fun disktest/Foo.class
  """.stripMargin

  override def yah(res: Seq[String]) =
    // It's currently unknown why this test fails on Avian with
    // “Failed: No anonfuns found.”, skip it for now. See SI-7630.
    if (scala.tools.partest.utils.Properties.isAvian)
      true
    else {
      def filtered = res filter (_ contains "public final class disktest.Foo")
      1 == filtered.size
    }
}
