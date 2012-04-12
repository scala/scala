import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xlog-free-types"
  def code = """
import scala.reflect.mirror._
def foo[T]{
  val tt = implicitly[TypeTag[List[T]]]
  println(tt)
}
foo[Int]
  """
}
