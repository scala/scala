//############################################################################
// Literals
//############################################################################
// $Id$

//############################################################################

object Test {
  import java.lang.System.out;

  /* I add a couple of Unicode identifier tests here temporarily */

  def \u03b1\u03c1\u03b5\u03c4\u03b7 = "alpha rho epsilon tau eta";

  case class GGG(i:int) {
    def \u21a1\u21a1( that:GGG ) = that;
  }
  def check_success[a](name: String, closure: => a, expected: a): Unit = {
    out.print("test " + name);
    try {
      val actual: a = closure;
      if (actual == expected) {
        out.print(" was successful");
      } else {
        out.print(" failed: expected "+ expected +", found "+ actual);
      }
    } catch {
      case exception: Throwable => {
        out.print(" raised exception " + exception);
      }
    }
    out.println();
  }

  def main(args: Array[String]) = {
    // char
    check_success("'\\u0024' == '$'", '\u0024', '$');
    check_success("'\\u005f' == '_'", '\u005f', '_');
    check_success("65.asInstanceOf[char] == 'A'", 65.asInstanceOf[char], 'A');

    out.println();

    // int
    check_success("01 == 1", 01, 1);
    check_success("010 == 8", 010, 8);
    check_success("0X01 == 1", 0X01, 1);
    check_success("0x01 == 1", 0x01, 1);
    check_success("0x10 == 16", 0x10, 16);
    check_success("0xa == 10", 0xa, 10);
    check_success("0x0a == 10", 0x0a, 10);

    check_success("+01 == 1", +01, 1);
    check_success("+010 == 8", +010, 8);
    check_success("+0x01 == 1", +0x01, 1);
    check_success("+0x10 == 16", +0x10, 16);
    check_success("+0xa == 10", +0xa, 10);
    check_success("+0x0a == 10", +0x0a, 10);

    check_success("-01 == -1", -01, -1);
    check_success("-010 == -8", -010, -8);
    check_success("-0x01 == -1", -0x01, -1);
    check_success("-0x10 == -16", -0x10, -16);
    check_success("-0xa == -10", -0xa, -10);
    check_success("-0x0a == -10", -0x0a, -10);

    check_success("017777777777 == 2147483647", 017777777777, 2147483647);
    check_success("020000000000 == -2147483648", 020000000000, -2147483648);
    check_success("037777777777 == -1", 037777777777, -1);

    check_success("0x7fffffff == 2147483647", 0x7fffffff, 2147483647);
    check_success("0x80000000 == -2147483648", 0x80000000, -2147483648);
    check_success("0xffffffff == -1", 0xffffffff, -1);

    out.println();

    // long
    check_success("1l == 1L", 1l, 1L);
    check_success("1L == 1l", 1L, 1l);
    check_success("1.asInstanceOf[long] == 1l", 1.asInstanceOf[long], 1l);

    check_success("0777777777777777777777L == 9223372036854775807L",
      0777777777777777777777L, 9223372036854775807L);
    check_success("01000000000000000000000L == -9223372036854775808L",
      01000000000000000000000L, -9223372036854775808L);
    check_success("01777777777777777777777L == -1L",
      01777777777777777777777L, -1L);

    check_success("0x7fffffffffffffffL == 9223372036854775807L",
      0x7fffffffffffffffL, 9223372036854775807L);
    check_success("0x8000000000000000L == -9223372036854775808L",
      0x8000000000000000L, -9223372036854775808L);
    check_success("0xffffffffffffffffL == -1L",
      0xffffffffffffffffL, -1L);

    out.println();

    // see JLS at address:
    // http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#230798

    // float
    check_success("1e1f == 10.0f", 1e1f, 10.0f);
    check_success("2.f == 2.0f", 2.f, 2.0f);
    check_success(".3f == 0.3f", .3f, 0.3f);
    check_success("0f == 0.0f", 0f, 0.0f);
    check_success("3.14f == 3.14f", 3.14f, 3.14f);
    check_success("6.022e23f == 6.022e23f", 6.022e23f, 6.022e23f);
    check_success("09f == 9.0f", 09f, 9.0f);
    check_success("1.asInstanceOf[float] == 1.0", 1.asInstanceOf[float], 1.0f);
    check_success("1l.asInstanceOf[float] == 1.0", 1l.asInstanceOf[float], 1.0f);

    out.println();

    // double
    check_success("1e1 == 10.0", 1e1, 10.0);
    check_success("2. == 2.0", 2., 2.0);
    check_success("2.d == 2.0", 2.d, 2.0);
    check_success(".3 == 0.3", .3, 0.3);
    check_success("0.0 == 0.0", 0.0, 0.0);
    check_success("0d == 0.0", 0d, 0.0);
    check_success("3.14 == 3.14", 3.14, 3.14);
    check_success("1e-9d == 1.0e-9", 1e-9d, 1.0e-9);
    check_success("1e137 == 1.0e137", 1e137, 1.0e137);
    check_success("1.asInstanceOf[double] == 1.0", 1.asInstanceOf[double], 1.0);
    check_success("1l.asInstanceOf[double] == 1.0", 1l.asInstanceOf[double], 1.0);

    out.println();
    check_success("\"\\u001a\".length()", "\u001a".length(), 1);
    val ggg = GGG( 1 ) \u21a1\u21a1 GGG( 2 );
    check_success("ggg == GGG( 2 )", ggg, GGG( 2 ));
  }
}

//############################################################################
