//############################################################################
// Iterators
//############################################################################
// $Id$

//############################################################################

import java.lang.System; // to avoid name clash with .NET's library

object Test {

  def check_range: Int = {
     val xs1 = Iterator.range(0, 10,  2) toList;
     val xs2 = Iterator.range(0, 10, -2) toList;
     val xs3 = Iterator.range(10, 0, -2) toList;
     val xs4 = Iterator.range(10, 0,  2) toList;
     xs1.length + xs2.length + xs3.length + xs4.length
  }

  def check_take: Int = {
    val it1 = Iterator.from(0);
    val xs1 = it1 take 10 toList;
    xs1.length
  }

  def check_drop: Int = {
    val it1 = Iterator.from(0);
    val it2 = it1 map { x => 2 * x };
    val n1 = it1 drop 2 next;
    val n2 = it2 drop 2 next;
    n1 + n2
  }

  def check_foreach: Int = {
    val it1 = Iterator.from(0) take 20;
    var n = 0;
    it1 foreach { x => n = n + x }
    n
  }

  def check_success[A](name: String, closure: => A, expected: A): Unit = {
    System.out.print("test " + name);
    try {
      val actual: A = closure;
      if (actual == expected)
        System.out.print(" was successful");
      else
        System.out.print(" failed: expected "+ expected +", found "+ actual);
    }
    catch {
      case exception: Throwable => {
        System.out.print(" raised exception " + exception);
      }
    }
    System.out.println();
  }

  def main(args: Array[String]): Unit = {
    check_success("check_range",    check_range,    10);
    check_success("check_take",     check_take,     10);
    check_success("check_drop",     check_drop,     12);
    check_success("check_foreach",  check_foreach, 190);
    System.out.println();
  }
}

//############################################################################
