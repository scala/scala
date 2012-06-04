import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xlog-free-types"
  def code = """
import scala.reflect.runtime.universe._
def foo[T: TypeTag] = reify{List[T]()}
println(foo)
  """
}
