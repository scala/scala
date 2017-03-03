
import scala.tools.partest.ParserTest

object Test extends ParserTest {

  def code = s"""class C { def g = 42 ; def f = s""\"123\r\n$${ g }\r\n123\r\n""\"}"""
}
