
import scala.tools.partest.DirectTest

object Test extends DirectTest {
  // for reference, UTF-8 of U0
  //val data = Array(0xed, 0xa0, 0x81).map(_.asInstanceOf[Byte])
  def U0 = "\ud801"
  def U1 = "\udc00"
  def code =
    s"""class C {
       |  def x = "$U0"
       |  def y = "$U1"
       |  def $U0 = x
       |  def $U1 = y
       |}""".stripMargin

  def show(): Unit = {
    assert(U0.length == 1)
    assert(!compile())
  }
}

