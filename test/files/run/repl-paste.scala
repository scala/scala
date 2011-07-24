import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = ":paste\n" + (
    """
class Dingus
{
  private val x = 5
  def y = Dingus.x * 2
}
object Dingus
{
  private val x = 55
}

val x = (new Dingus).y
    """
  )
}