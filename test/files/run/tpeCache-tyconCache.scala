import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
:power

AnyRefClass.tpe eq AnyRefClass.typeConstructor
AnyRefClass.tpe eq AnyRefClass.typeConstructor
  """.trim
}
