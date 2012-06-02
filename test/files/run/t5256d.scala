import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
class A { def foo = ??? }
val c = cm.reflectClass(classOf[A]).symbol
println(c)
println(c.fullName)
println(c.typeSignature)
  """
}