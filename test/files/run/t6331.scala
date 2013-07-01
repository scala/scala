import scala.tools.partest.DirectTest

// Test of Constant#equals, which must must account for floating point intricacies.
object Test extends DirectTest {

  override def code = ""

  override def show() {
    val global = newCompiler()
    import global._

    def check(c1: Any, c2: Any): Unit = {
      val const1 = Constant(c1)
      val const2 = Constant(c2)
      val equal = const1 == const2
      def show(a: Any) = "" + a + (a match {
        case _: Byte   => ".toByte"
        case _: Short  => ".toShort"
        case _: Long   => "L"
        case _: Float  => "f"
        case _: Double => "d"
        case _ => ""
      })
      val op = if (equal) "==" else "!="
      println(f"${show(c1)}%12s $op ${show(c2)}")

      val hash1 = const1.hashCode
      val hash2 = const2.hashCode
      val hashesEqual = hash1 == hash2
      val hashBroken = equal && !hashesEqual
      if (hashBroken) println(f"$hash1%12s != $hash2 // hash codes differ for equal objects!!")
    }

    check((), ())

    check(true, true)
    check(true, false)
    check(false, true)

    check(0.toByte, 0.toByte)
    check(0.toByte, 1.toByte)

    check(0.toShort, 0.toShort)
    check(0.toShort, 1.toShort)

    check(0, 0)
    check(0, 1)

    check(0L, 0L)
    check(0L, 1L)

    check(0f, 0f)
    check(0f, -0f)
    check(-0f, 0f)
    check(Float.NaN, Float.NaN)

    check(0d, 0d)
    check(0d, -0d)
    check(-0d, 0d)
    check(Double.NaN, Double.NaN)

    check(0, 0d)
    check(0, 0L)
    check(0d, 0f)
  }
}
