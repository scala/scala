
import scala.tools.nsc._
import scala.tools.partest.{Lambdaless, ReplTest}


object Test extends ReplTest with Lambdaless {
  override def extraSettings = "-opt:inline:**"
  def code = """
    val n = 2
    () => n
  """
}
