import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |val x1 = 1
    |val x2 = 2
    |val x3 = 3
    |case class BippyBungus()
    |x1 + x2 + x3
    |:reset
    |x1 + x2 + x3
    |val x1 = 4
    |new BippyBungus
    |class BippyBungus() { def f = 5 }
    |{ new BippyBungus ; x1 }
  """.stripMargin

    // would be nice if we could count on javap being present
    // but no
    //
    // |:javap BippyBungus
}
