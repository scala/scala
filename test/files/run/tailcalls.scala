//############################################################################
// Tail Calls
//############################################################################
// $Id$

//############################################################################

object Object {

  def fun1(n: Int, v: Int): Int =
    if (n == 0) v else fun1(n - 1, v - 1);

  def fun2[T](n: Int, v: Int): Int =
    if (n == 0) v else fun2[T](n - 1, v - 1);

  def fun3[T](n: Int, v: Int, ls: List[T]): Int =
    if (n == 0) v else fun3(n - 1, v - 1, ls);

  // !!! return in non-tail-call position
  // !!! local functions
  // !!! functions in objects in classes
  // !!! non-same-instance calls
  // !!! non-same-type calls

}

//############################################################################

final class Final {

  def fun1(n: Int, v: Int): Int =
    if (n == 0) v else fun1(n - 1, v - 1);

}

//############################################################################

class Class {

  final def fun1(n: Int, v: Int): Int =
    if (n == 0) v else fun1(n - 1, v - 1);

  def fun2(n: Int, v: Int): Int = funA(n, v);
  private def funA(n: Int, v: Int): Int =
    if (n == 0) v else funA(n - 1, v - 1);

  def fun3(n: Int, v: Int): Int = fun4(n, v);

  def fun4(n: Int, v: Int): Int =
    if (n == 0) v else fun4(n - 1, v - 1);

}

class SubClass extends Class {

  override def fun4(n: Int, v: Int): Int = v;

}

//############################################################################

sealed class Sealed {

  def fun1(n: Int, v: Int): Int =
    if (n == 0) v else fun1(n - 1, v - 1);

}

//############################################################################

// class SubSealed extends Sealed {

//   override def fun1(n: Int, v: Int): Int = v;

// }

//############################################################################

object Test {

  def check_success(name: String, def closure: Int, expected: Int): Unit = {
    System.out.print("test " + name);
    try {
      val actual: Int = closure;
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
    if (args.length < 1) throw new Error("missing target argument");
    val min = 16;
    val max = args(0) match {
      case "jvm"  => 2 * 1024 * 1024
      case "int"  =>       64 * 1024
      case "msil" => 2 * 1024 * 1024
      case target => throw new Error("unknown target '" + target + "'")
    }
    check_success("Object.fun1", Object.fun1(max, max), 0);
    check_success("Object.fun2", Object.fun2(max, max), 0);
    check_success("Object.fun3", Object.fun3(max, max, List(1)), 0);

    val f: Final = new Final;
    check_success("Class.fun1", f.fun1(max, max), 0);

    val c: Class = new SubClass;
    check_success("Class.fun1", c.fun1(max, max), 0);
    check_success("Class.fun2", c.fun2(max, max), 0);
    check_success("Class.fun3", c.fun3(min, min), min);
    check_success("Class.fun4", c.fun4(min, min), min);

// !!!
//     val s: Sealed = new SubSealed;
//     check_success("Sealed.fun1", s.fun1(min, min), min);

  }
}

//############################################################################
