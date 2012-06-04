import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xlog-free-terms"
  def code = """
{
  import scala.reflect.runtime.universe._
  val x = "2"
  val tt = implicitly[TypeTag[x.type]]
  println(tt)
}
  """
}