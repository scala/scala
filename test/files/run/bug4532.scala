import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |object Bippy { class Dingus ; object Bop }
    |:javap Bippy.Dingus
    |:javap Bippy.Bop
  """.stripMargin
}
