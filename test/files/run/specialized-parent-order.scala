// A class extending a specialized parent at a specialized type argument should
// extend the specialized subclass of the parent, regardless of whether it is
// declared before or after that parent, and regardless of whether it is generic.

class GenericBefore[U] extends P1[Int]
class P1[@specialized(Int) T]

class P2[@specialized(Int) T]
class GenericAfter[U] extends P2[Int]

class MonoBefore extends P3[Int]
class P3[@specialized(Int) T]

class P4[@specialized(Int) T]
class MonoAfter extends P4[Int]

object Test {
  def check(c: Class[_], expected: String): Unit = {
    val actual = c.getSuperclass.getName
    assert(actual == expected, s"${c.getName} extends $actual, expected $expected")
  }
  def main(args: Array[String]): Unit = {
    check(classOf[GenericBefore[_]], "P1$mcI$sp")
    check(classOf[GenericAfter[_]],  "P2$mcI$sp")
    check(classOf[MonoBefore],       "P3$mcI$sp")
    check(classOf[MonoAfter],        "P4$mcI$sp")
  }
}
