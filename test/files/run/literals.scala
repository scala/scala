//############################################################################
// Literals
//############################################################################
// $Id$

//############################################################################

object Test {

  def check_success[a](name: String, def closure: a, expected: a): Unit = {
    import java.lang.System;
    System.out.print("test " + name);
    try {
      val actual: a = closure;
      if (actual == expected) {
        System.out.print(" was successful");
      } else {
        System.out.print(" failed: expected "+ expected +", found "+ actual);
      }
    } catch {
      case exception: Throwable => {
        System.out.print(" raised exception " + exception);
      }
    }
    System.out.println();
  }

  def main(args: Array[String]) = {
    // see JLS at address:
    // http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#230798

    check_success("1e1f == 10.0f", 1e1f, 10.0f);
    check_success("2.f == 2.0f", 2.f, 2.0f);
    check_success(".3f == 0.3f", .3f, 0.3f);
    check_success("0f == 0.0f", 0f, 0.0f);
    check_success("3.14f == 3.14f", 3.14f, 3.14f);
    check_success("6.022e23f == 6.022e23f", 6.022e23f, 6.022e23f);
    check_success("09f == 9.0f", 09f, 9.0f);

    check_success("1e1 == 10.0", 1e1, 10.0);
    check_success("2. == 2.0", 2., 2.0);
    check_success("2.d == 2.0", 2.d, 2.0);
    check_success(".3 == 0.3", .3, 0.3);
    check_success("0.0 == 0.0", 0.0, 0.0);
    check_success("0d == 0.0", 0d, 0.0);
    check_success("3.14 == 3.14", 3.14, 3.14);
    check_success("1e-9d == 1.0e-9", 1e-9d, 1.0e-9);
    check_success("1e137 == 1.0e137", 1e137, 1.0e137);
  }
}

//############################################################################
