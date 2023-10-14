
import scala.tools.partest.DirectTest

object Test extends DirectTest {
  // for reference, UTF-8 of U0
  //val data = Array(0xed, 0xa0, 0x81).map(_.asInstanceOf[Byte])
  def U0 = "\ud801"
  def U1 = "\udc00"
  // \u10428 isLetter and isLowerCase
  def U2 = "\ud801"
  def U3 = "\udc28"
  // symbol operator So with supplementary char
  def U4 = "\ud834"
  def U5 = "\udd97"
  // cyclone 1f300
  def U6 = "\ud83c"
  def U7 = "\udf00"
  // rocket 1f680
  def U8 = "\ud83d"
  def U9 = "\ude80"
  // quintessence 1f700
  def UA = "\ud83d"
  def UB = "\udf00"

  // 1d4c5 Mathematical Script Small P
  def UC = "\ud835"
  def UD = "\udcc5"

  def code =
    s"""class Identifiers {
       |  def x = "$U0"
       |  def y = "$U1"
       |  def `$U0` = x
       |  def `$U1` = y
       |
       |  def f(x: Any): Boolean = x match {
       |    case ${U2}${U3}XYZ: String => true
       |    case $U2$U3                => true
       |  }
       |  def g(x: Any) = x match {
       |    case $U2$U3 @ _ => $U2$U3
       |  }
       |}
       |class Ops {
       |  def $U4$U5 = 42        // was error: illegal character
       |  def op_$U4$U5 = 42     // was error: illegal character
       |  def $U6$U7 = 42
       |  def op_$U6$U7 = 42
       |  def $U8$U9 = 42
       |  def op_$U8$U9 = 42
       |  def $UA$UB = 42
       |  def op_$UA$UB = 42
       |  def $UC$UD = 42
       |  def op_$UC$UD = 42
       |}
       |class Strings {
       |  implicit class Interps(sc: StringContext) {
       |    def $UC$UD(parts: Any*) = "done"
       |  }
       |  def $UC$UD = 42
       |  def interpolated = s"$$$UC$UD"
       |  def e = "a $UC$UD b"
       |  def f = $UC$UD"one"
       |}""".stripMargin

  def show(): Unit = {
    assert(U0.length == 1)
    assert(compile())
  }
}
