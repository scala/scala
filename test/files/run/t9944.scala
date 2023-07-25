
import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Ystop-after:parser -Vprint:parser"

  def code = s"""class C { def g = 42 ; def f = s""\"123\r\n$${ g }\r\n123\r\n""\"}"""

  override def show(): Unit = if (!compile()) println("Compilation failed!")
}
