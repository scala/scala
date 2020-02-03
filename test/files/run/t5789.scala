
import scala.tools.nsc._
import interpreter.ILoop
import scala.tools.partest._


object Test extends ReplTest with Lambdaless {
  override def extraSettings = "-opt:l:inline -opt-inline-from:**"
  def code = """
    val n = 2
    () => n
  """
}
