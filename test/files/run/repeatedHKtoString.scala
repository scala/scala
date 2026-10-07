import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
:power
RepeatedParamClass.tpe.toString
JavaRepeatedParamClass.tpe.toString
ByNameParamClass.tpe.toString
RepeatedParamClass.tpeHK.toString
JavaRepeatedParamClass.tpeHK.toString
ByNameParamClass.tpeHK.toString
  """.trim
}
