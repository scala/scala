//############################################################################
// Lists
//############################################################################
// $Id$

//############################################################################

object Test {

  val xs1 = List(1, 2, 3);
  val xs2 = List('a', 'b');
  val xs3 = List(List(1, 2), List(4, 5));
  val xs4 = List(2, 4, 6, 8);
  val xs5 = List(List(3, 4), List(3), List(4, 5));

  def check_count: int = {
    val n1 = xs1 count { e => e % 2 != 0 };
    val n2 = xs4 count { e => e < 5 };
    n1 + n2
  }

  def check_diff: int = {
    val ys1 = xs1 diff xs4;
    val ys2 = xs3 diff xs5;
    ys1.length + ys2.length
  }

  def check_exists: boolean = {
    val b1 = xs1 exists { e => e % 2 == 0 };
    val b2 = xs4 exists { e => e == 5 };
    b1 & b2
  }

  def check_filter: int = {
    val ys1 = xs1 filter { e => e % 2 == 0 };
    val ys2 = xs4 filter { e => e < 5 };
    ys1.length + ys2.length
  }

  def check_foldLeft: int = {
    val n1 = xs1.foldLeft(0)((e1, e2) => e1 + e2);
    val ys1 = xs4.foldLeft(List[Int]())((e1, e2) => e2 :: e1);
    n1 + ys1.length
  }

  def check_forall: boolean = {
    val b1 = xs1 forall { e => e < 10};
    val b2 = xs4 forall { e => e % 2 == 0 };
    b1 & b2
  }

  def check_intersect: int = {
    val ys1 = xs1 intersect xs4;
    val ys2 = xs3 intersect xs5;
    ys1.length + ys2.length
  }

  def check_remove: int = {
    val ys1 = xs1 remove { e => e % 2 != 0 };
    val ys2 = xs4 remove { e => e < 5 };
    ys1.length + ys2.length
  }

  def check_union: int = {
    val ys1 = xs1 union xs4;
    val ys2 = xs3 union xs5;
    ys1.length + ys2.length
  }

  def check_zip: int = {
    val ys1 = xs1 zip xs2;
    val ys2 = xs1 zip xs3;
    ys1.length + ys2.length
  }

  def check_zipAll: int = {
    val ys1 = xs1.zipAll(xs2, 0, '_');
    val ys2 = xs1.zipAll(xs3, 0, List(-1));
    ys1.length + ys2.length
  }

  def check_success[A](name: String, closure: => A, expected: A): Unit = {
    Console.print("test " + name);
    try {
      val actual: A = closure;
      if (actual == expected)
        Console.print(" was successful");
      else
        Console.print(" failed: expected "+ expected +", found "+ actual);
    }
    catch {
      case exception: Throwable => {
        Console.print(" raised exception " + exception);
      }
    }
    Console.println;
  }

  def main(args: Array[String]): Unit = {
    check_success("check_count",     check_count,      4);
    check_success("check_diff",      check_diff,       3);
    check_success("check_exists",    check_exists, false);
    check_success("check_filter",    check_filter,     3);
    check_success("check_foldLeft",  check_foldLeft,  10);
    check_success("check_forall",    check_forall,  true);
    check_success("check_intersect", check_intersect,  2);
    check_success("check_remove",    check_remove,     3);
    check_success("check_union",     check_union,     10);
    check_success("check_zip",       check_zip,        4);
    check_success("check_zipAll",    check_zipAll,     6);
    Console.println;
  }
}

//############################################################################
