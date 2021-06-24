// java: -Xss128M

import scala.tools.partest.ReplTest

// ReplTest so that the long concatenation is compiled at test-run-time with the larger `Xss`.
// Tests are always compiled in the partest VM.
object Test extends ReplTest {
  def code =
    """// This should generally obey 15.18.1. of the JLS (String Concatenation Operator +)
      |def concatenatingVariousTypes(): String = {
      |  val str: String = "some string"
      |  val sb: StringBuffer = new StringBuffer("some stringbuffer")
      |  val cs: CharSequence = java.nio.CharBuffer.allocate(50).append("charsequence")
      |  val i: Int = 123456789
      |  val s: Short = 345
      |  val b: Byte = 12
      |  val z: Boolean = true
      |  val f: Float = 3.14f
      |  val j: Long = 98762147483647L
      |  val d: Double = 3.1415d
      |
      |  "String " + str + "\n" +
      |    "StringBuffer " + sb + "\n" +
      |    "CharSequence " + cs + "\n" +
      |    "Int " + i + "\n" +
      |    "Short " + s + "\n" +
      |    "Byte " + b + "\n" +
      |    "Boolean " + z + "\n" +
      |    "Float " + f + "\n" +
      |    "Long " + j + "\n" +
      |    "Double " + d + "\n"
      |}
      |// The characters `\u0001` and `\u0002` play a special role in `StringConcatFactory`
      |def concatenationInvolvingSpecialCharacters(): String = {
      |  val s1 = "Qux"
      |  val s2 = "Quux"
      |
      |  s"Foo \u0001 $s1 Bar \u0002 $s2 Baz"
      |}
      |// Concatenation involving more than 200 elements
      |def largeConcatenation(): String = {
      |  val s00 = "s00"
      |  val s01 = "s01"
      |  val s02 = "s02"
      |  val s03 = "s03"
      |  val s04 = "s04"
      |  val s05 = "s05"
      |  val s06 = "s06"
      |  val s07 = "s07"
      |  val s08 = "s08"
      |
      |  // 24 rows follow
      |  s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n" +
      |    s00 + "," + s01 + "," + s02 + "," + s03 + "," + s04 + "," + s05 + "," + s06 + "," + s07 + "," + s08 + "\n"
      |}
      |println("----------")
      |println(concatenatingVariousTypes())
      |println("----------")
      |println(concatenationInvolvingSpecialCharacters())
      |println("----------")
      |println(largeConcatenation())
      |println("----------")
      |""".stripMargin
}
