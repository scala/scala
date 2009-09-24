//############################################################################
// Literals
//############################################################################

//############################################################################

object Test {

  /* I add a couple of Unicode identifier tests here temporarily */

  def \u03b1\u03c1\u03b5\u03c4\u03b7 = "alpha rho epsilon tau eta"

  case class GGG(i: Int) {
    def \u03b1\u03b1(that: GGG) = i + that.i
  }

  def check_success[a](name: String, closure: => a, expected: a) {
    print("test " + name)
    try {
      val actual: a = closure
      if (actual == expected) {
        print(" was successful");
      } else {
        print(" failed: expected "+ expected +", found "+ actual);
      }
    } catch {
      case exception: Throwable => {
        print(" raised exception " + exception);
      }
    }
    println
  }

  def main(args: Array[String]) {
    // char
    check_success("'\\u0024' == '$'", '\u0024', '$')
    check_success("'\\u005f' == '_'", '\u005f', '_')
    check_success("65.asInstanceOf[Char] == 'A'", 65.asInstanceOf[Char], 'A')
    check_success("\"\\141\\142\" == \"ab\"", "\141\142", "ab")
    check_success("\"\\0x61\\0x62\".trim() == \"x61\\0x62\"", "\0x61\0x62".substring(1), "x61\0x62")

    println

    // boolean
    check_success("(65 : Byte) == 'A'", (65: Byte) == 'A', true) // contrib #176

    println

    // int
    check_success("01 == 1", 01, 1)
    check_success("010 == 8", 010, 8)
    check_success("0X01 == 1", 0X01, 1)
    check_success("0x01 == 1", 0x01, 1)
    check_success("0x10 == 16", 0x10, 16)
    check_success("0xa == 10", 0xa, 10)
    check_success("0x0a == 10", 0x0a, 10)

    check_success("+01 == 1", +01, 1)
    check_success("+010 == 8", +010, 8)
    check_success("+0x01 == 1", +0x01, 1)
    check_success("+0x10 == 16", +0x10, 16)
    check_success("+0xa == 10", +0xa, 10)
    check_success("+0x0a == 10", +0x0a, 10)

    check_success("-01 == -1", -01, -1)
    check_success("-010 == -8", -010, -8)
    check_success("-0x01 == -1", -0x01, -1)
    check_success("-0x10 == -16", -0x10, -16)
    check_success("-0xa == -10", -0xa, -10)
    check_success("-0x0a == -10", -0x0a, -10)

    check_success("017777777777 == 2147483647", 017777777777, 2147483647)
    check_success("020000000000 == -2147483648", 020000000000, -2147483648)
    check_success("037777777777 == -1", 037777777777, -1)

    check_success("0x7fffffff == 2147483647", 0x7fffffff, 2147483647)
    check_success("0x80000000 == -2147483648", 0x80000000, -2147483648)
    check_success("0xffffffff == -1", 0xffffffff, -1)

    println

    // long
    check_success("1l == 1L", 1l, 1L)
    check_success("1L == 1l", 1L, 1l)
    check_success("1.asInstanceOf[Long] == 1l", 1.asInstanceOf[Long], 1l)

    check_success("0777777777777777777777L == 9223372036854775807L",
      0777777777777777777777L, 9223372036854775807L)
    check_success("01000000000000000000000L == -9223372036854775808L",
      01000000000000000000000L, -9223372036854775808L)
    check_success("01777777777777777777777L == -1L",
      01777777777777777777777L, -1L)

    check_success("0x7fffffffffffffffL == 9223372036854775807L",
      0x7fffffffffffffffL, 9223372036854775807L)
    check_success("0x8000000000000000L == -9223372036854775808L",
      0x8000000000000000L, -9223372036854775808L)
    check_success("0xffffffffffffffffL == -1L",
      0xffffffffffffffffL, -1L)

    println

    // see JLS at address:
    // http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#230798

    // float
    check_success("1e1f == 10.0f", 1e1f, 10.0f)
    check_success("2.f == 2.0f", 2.f, 2.0f)
    check_success(".3f == 0.3f", .3f, 0.3f)
    check_success("0f == 0.0f", 0f, 0.0f)
    check_success("3.14f == 3.14f", 3.14f, 3.14f)
    check_success("6.022e23f == 6.022e23f", 6.022e23f, 6.022e23f)
    check_success("09f == 9.0f", 09f, 9.0f)
    check_success("1.asInstanceOf[Float] == 1.0", 1.asInstanceOf[Float], 1.0f)
    check_success("1l.asInstanceOf[Float] == 1.0", 1l.asInstanceOf[Float], 1.0f)

    println

    // double
    check_success("1e1 == 10.0", 1e1, 10.0)
    check_success("2. == 2.0", 2., 2.0)
    check_success("2.d == 2.0", 2.d, 2.0)
    check_success(".3 == 0.3", .3, 0.3)
    check_success("0.0 == 0.0", 0.0, 0.0)
    check_success("0d == 0.0", 0d, 0.0)
    check_success("3.14 == 3.14", 3.14, 3.14)
    check_success("1e-9d == 1.0e-9", 1e-9d, 1.0e-9)
    check_success("1e137 == 1.0e137", 1e137, 1.0e137)
    check_success("1.asInstanceOf[Double] == 1.0", 1.asInstanceOf[Double], 1.0)
    check_success("1l.asInstanceOf[Double] == 1.0", 1l.asInstanceOf[Double], 1.0)

    println
    check_success("\"\".length()", "\u001a".length(), 1)

    val ggg = GGG(1) \u03b1\u03b1 GGG(2)
    check_success("ggg == 3", ggg, 3)

  }
}

//############################################################################
