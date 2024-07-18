//> using options -deprecation
//
//############################################################################
// Literals
//############################################################################

//############################################################################

object Test {

  def check_success[A](name: String, closure: => A, expected: A): Unit = {
    val res: Option[String] =
      try {
        val actual: A = closure
        if (actual == expected) None  //print(" was successful")
        else Some(s" failed: expected $expected, found $actual")
      } catch {
        case exception: Throwable => Some(s" raised exception $exception")
      }
    for (e <- res) println(s"test $name $e")
  }

  def main(args: Array[String]): Unit = {
    // char

    //unicode escapes escape in char literals
    check_success("'\\u0024' == '$'", '\u0024', '$')
    check_success("'\\u005f' == '_'", '\u005f', '_')

    //unicode escapes escape in interpolations
    check_success("""s"\\u0024" == "$"""", s"\u0024", "$")
    check_success("s\"\"\"\\u0024\"\"\" == \"$\"", s"""\u0024""", "$")


    //Int#asInstanceOf[Char] gets the char at the codepoint
    check_success("65.asInstanceOf[Char] == 'A'", 65.asInstanceOf[Char], 'A')
    
    // boolean
    check_success("(65 : Byte) == 'A'", (65: Byte) == 'A', true) // contrib #176

    // int
    check_success("0X01 == 1", 0X01, 1)
    check_success("0x01 == 1", 0x01, 1)
    check_success("0x10 == 16", 0x10, 16)
    check_success("0xa == 10", 0xa, 10)
    check_success("0x0a == 10", 0x0a, 10)

    check_success("+0x01 == 1", +0x01, 1)
    check_success("+0x10 == 16", +0x10, 16)
    check_success("+0xa == 10", +0xa, 10)
    check_success("+0x0a == 10", +0x0a, 10)

    check_success("-0x01 == -1", -0x01, -1)
    check_success("-0x10 == -16", -0x10, -16)
    check_success("-0xa == -10", -0xa, -10)
    check_success("-0x0a == -10", -0x0a, -10)

    check_success("0x7fffffff == 2147483647", 0x7fffffff, 2147483647)
    check_success("0x80000000 == -2147483648", 0x80000000, -2147483648)
    check_success("0xffffffff == -1", 0xffffffff, -1)

    check_success("0b_0000 == 0x0", 0b_0000, 0x0)
    check_success("0b_0001 == 0x1", 0b_0001, 0x1)
    check_success("0b_0010 == 0x2", 0b_0010, 0x2)
    check_success("0b_0011 == 0x3", 0b_0011, 0x3)
    check_success("0b_0100 == 0x4", 0b_0100, 0x4)
    check_success("0b_0101 == 0x5", 0b_0101, 0x5)
    check_success("0b_0110 == 0x6", 0b_0110, 0x6)
    check_success("0b_0111 == 0x7", 0b_0111, 0x7)
    check_success("0b_1000 == 0x8", 0b_1000, 0x8)
    check_success("0b_1001 == 0x9", 0b_1001, 0x9)
    check_success("0b_1010 == 0xa", 0b_1010, 0xa)
    check_success("0b_1011 == 0xb", 0b_1011, 0xb)
    check_success("0b_1100 == 0xc", 0b_1100, 0xc)
    check_success("0b_1101 == 0xd", 0b_1101, 0xd)
    check_success("0b_1110 == 0xe", 0b_1110, 0xe)
    check_success("0b_1111 == 0xf", 0b_1111, 0xf)

    check_success("0B_1000 == 0x8", 0B_1000, 0x8)

    assert(0b0001_0000 == 16)
    assert(0b0010_0000 == 32)
    assert(0b0100_0000 == 64)
    assert(0b1000_0000 == 128)

    assert(0b0001_0000_0000 == 256)
    assert(0b0010_0000_0000 == 512)
    assert(0b0100_0000_0000 == 1024)
    assert(0b1000_0000_0000 == 2048)

    assert(0b0001_0000_0000_0000 == 4096)
    assert(0b0010_0000_0000_0000 == 8192)
    assert(0b0100_0000_0000_0000 == 16384)
    assert(0b1000_0000_0000_0000 == 32768)

    assert(0b0001__0000_0000_0000_0000 == 65536)
    assert(0b0010__0000_0000_0000_0000 == 131072)
    assert(0b0100__0000_0000_0000_0000 == 262144)
    assert(0b1000__0000_0000_0000_0000 == 524288)

    assert(0b0001_0000__0000_0000_0000_0000 == 1048576)
    assert(0b0010_0000__0000_0000_0000_0000 == 2097152)
    assert(0b0100_0000__0000_0000_0000_0000 == 4194304)
    assert(0b1000_0000__0000_0000_0000_0000 == 8388608)

    assert(0b0001_0000_0000__0000_0000_0000_0000 == 16777216)
    assert(0b0010_0000_0000__0000_0000_0000_0000 == 33554432)
    assert(0b0100_0000_0000__0000_0000_0000_0000 == 67108864)
    assert(0b1000_0000_0000__0000_0000_0000_0000 == 134217728)

    assert(0b0001_0000_0000_0000__0000_0000_0000_0000 == 268435456)
    assert(0b0010_0000_0000_0000__0000_0000_0000_0000 == 536870912)
    assert(0b0100_0000_0000_0000__0000_0000_0000_0000 == 1073741824)
    assert(0b1000_0000_0000_0000__0000_0000_0000_0000L == 2147483648L)

    assert(0b1000_0000_0000_0000__0000_0000_0000_0000 == -2147483648) // Signed !
    assert(0b1111_1111_1111_1111__1111_1111_1111_1111 == -1)

    // Randomly generated using https://numbergenerator.org/random-32-bit-binary-number#!numbers=10&length=32&addfilters=
    // Converted to signed decimal using https://onlinetoolz.net/unsigned-signed#base=2&bits=32
    assert(0b0110_1000_1100_0101_0010_1100_0100_0011 ==  1757752387)
    assert(0b1111_0101_0100_1011_0101_1000_0011_0110 == -179611594)
    assert(0b0000_0011_0000_1010_1010_0011_0000_0000 ==  51028736)
    assert(0b0101_0010_1111_1001_0100_0101_1101_1011 ==  1392068059)
    assert(0b1001_0000_1111_1001_1011_1101_1100_1111 == -1862681137)

    assert(0B0000_0111_1110_1100_0111_1100_1000_0010 ==  132938882)
    assert(0B0000_1011_0111_1011_0001_1010_1010_1000 ==  192617128)
    assert(0B1100_1100_1000_1010_1111_0111_0100_1101 == -863307955)
    assert(0B1000_0000_0001_0010_0001_1001_0101_1110 == -2146297506)
    assert(0B1110_0000_0110_1100_0111_0110_1100_1111 == -529762609)

    assert(0b0010_1001_0101_1001__1010_0100_1000_1010__1001_1000_0011_0111__1100_1011_0111_0101L ==  2979593543648529269L)
    assert(0b1101_1110_0100_1000__0010_1101_1010_0010__0111_1000_1111_1001__1010_1001_0101_1000L == -2429641823128802984L)

    // long
    check_success("1l == 1L", 1l, 1L)
    check_success("1L == 1l", 1L, 1l)
    check_success("1.asInstanceOf[Long] == 1l", 1.asInstanceOf[Long], 1l)

    check_success("0x7fffffffffffffffL == 9223372036854775807L",
      0x7fffffffffffffffL, 9223372036854775807L)
    check_success("0x8000000000000000L == -9223372036854775808L",
      0x8000000000000000L, -9223372036854775808L)
    check_success("0xffffffffffffffffL == -1L",
      0xffffffffffffffffL, -1L)

    // see JLS at address:
    // https://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#230798

    // float
    check_success("1e1f == 10.0f", 1e1f, 10.0f)
    check_success(".3f == 0.3f", .3f, 0.3f)
    check_success("0f == 0.0f", 0f, 0.0f)
    check_success("0f == -0.000000000000000000e+00f", 0f, -0.000000000000000000e+00f)
    check_success("0f == -0.000000000000000000e+00F", 0f, -0.000000000000000000e+00F)
    check_success("0f == -0.0000000000000000e14f", 0f, -0.0000000000000000e14f)
    check_success("01.23f == 1.23f", 01.23f, 1.23f)
    check_success("3.14f == 3.14f", 3.14f, 3.14f)
    check_success("6.022e23f == 6.022e23f", 6.022e23f, 6.022e23f)
    check_success("09f == 9.0f", 09f, 9.0f)
    check_success("1.00000017881393421514957253748434595763683319091796875001f == 1.0000001f",
      1.00000017881393421514957253748434595763683319091796875001f,
      1.0000001f)
    check_success("3.4028235E38f == Float.MaxValue", 3.4028235E38f, Float.MaxValue)
    check_success("1.asInstanceOf[Float] == 1.0", 1.asInstanceOf[Float], 1.0f)
    check_success("1L.asInstanceOf[Float] == 1.0", 1L.asInstanceOf[Float], 1.0f)

    // double
    check_success("1e1 == 10.0", 1e1, 10.0)
    check_success(".3 == 0.3", .3, 0.3)
    check_success("0.0 == 0.0", 0.0, 0.0)
    check_success("0d == 0.0", 0d, 0.0)
    check_success("0d == 0.000000000000000000e+00d", 0d, 0.000000000000000000e+00d)
    check_success("0d == -0.000000000000000000e+00d", 0d, -0.000000000000000000e+00d)
    check_success("0d == -0.000000000000000000e+00D", 0d, -0.000000000000000000e+00D)
    check_success("0.0 == 0.000000000000000000e+00", 0.0, 0.000000000000000000e+00)
    check_success("0.0 == -0.000000000000000000e+00", 0.0, -0.000000000000000000e+00)
    check_success("01.23 == 1.23", 01.23, 1.23)
    check_success("01.23d == 1.23d", 01.23d, 1.23d)
    check_success("3.14 == 3.14", 3.14, 3.14)
    check_success("1e-9d == 1.0e-9", 1e-9d, 1.0e-9)
    check_success("1e137 == 1.0e137", 1e137, 1.0e137)
    check_success("1.7976931348623157e308d == Double.MaxValue", 1.7976931348623157e308d, Double.MaxValue)
    check_success("1.asInstanceOf[Double] == 1.0", 1.asInstanceOf[Double], 1.0)
    check_success("1l.asInstanceOf[Double] == 1.0", 1l.asInstanceOf[Double], 1.0)

    check_success("\"\".length()", "\u001a".length(), 1)
  }
}

//############################################################################
