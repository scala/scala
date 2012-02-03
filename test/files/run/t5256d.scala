import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.reflect.mirror._
class A
val c = classToType(classOf[A])
println(c.typeSymbol == classToSymbol(classOf[A]))
  """
}
