import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xlog-free-types"
  def code = """
def foo[T]{
  import scala.reflect.runtime.universe._
  val tt = implicitly[TypeTag[List[T]]]
  println(tt)
}
foo[Int]
  """
}