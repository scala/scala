import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-feature -language:_"

  def code = """
:type 42
val x: 23 = 23
:type x
:type (23: 23)
val y = x
:type y
final val z = x
:type z
def xx: 23 = 23
:type xx
final val yy = xx
:type yy
  """.trim
}
