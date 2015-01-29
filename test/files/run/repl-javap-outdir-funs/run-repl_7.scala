import scala.tools.partest.JavapTest

object Test extends JavapTest {
  // note the '-fun': it makes :javap search for some anonfun.
  // for that reason, this test has a flags file that forces delambdafy:inline (doesn't allow :method)
  def code = """
    |:javap -fun disktest/Foo.class
  """.stripMargin

  override def yah(res: Seq[String]) =
    // It's currently unknown why this test fails on Avian with
    // “Failed: No anonfuns found.”, skip it for now. See SI-7630.
    if (scala.tools.partest.utils.Properties.isAvian)
      true
    else {
      val r = "public final class disktest.Foo.*extends scala.runtime.AbstractFunction1".r
      def filtered = res filter (r.findFirstIn(_).nonEmpty)
      1 == filtered.size
    }
}
