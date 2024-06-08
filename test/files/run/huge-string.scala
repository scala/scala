//> abusing options -Vdebug -Xverify

import scala.tools.partest.DirectTest

object Test extends DirectTest {
  def genStr = "ohohoh" * 0xFFFF
  def code = s"""
    class C {
      def s = "$genStr"
    }
  """
  def show() = assert(!compile())
}
