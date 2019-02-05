import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code =
    """|// completions!
       |object O { def x_y_x = 1; def x_y_z = 2; def getFooBarZot = 3}
       |:completions O.x
       |:completions O.x_y_x
       |:completions O.x_y_a
       |import O._
       |:completions x_y_
       |:completions x_y_a
       |:completions fBZ
       |:completions object O2 { val x = O.
       |:completions :completion
       |""".stripMargin
}
