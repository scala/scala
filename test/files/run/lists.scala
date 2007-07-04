//############################################################################
// Lists
//############################################################################
// $Id$

//############################################################################

object Test {

  val xs1 = List(1, 2, 3)
  val xs2 = List('a', 'b')
  val xs3 = List(List(1, 2), List(4, 5))
  val xs4 = List(2, 4, 6, 8)
  val xs5 = List(List(3, 4), List(3), List(4, 5))

  def check_count: Int = {
    val n1 = xs1 count { e => e % 2 != 0 }
    val n2 = xs4 count { e => e < 5 }
    n1 + n2
  }

  def check_diff: Int = {
    val ys1 = xs1 diff xs4
    val ys2 = xs3 diff xs5
    ys1.length + ys2.length
  }

  def check_exists: Boolean = {
    val b1 = xs1 exists { e => e % 2 == 0 }
    val b2 = xs4 exists { e => e == 5 }
    b1 & b2
  }

  def check_filter: Int = {
    val ys1 = xs1 filter { e => e % 2 == 0 }
    val ys2 = xs4 filter { e => e < 5 }
    ys1.length + ys2.length
  }

  def check_foldLeft: Int = {
    val n1 = xs1.foldLeft(0)((e1, e2) => e1 + e2)
    val ys1 = xs4.foldLeft(List[Int]())((e1, e2) => e2 :: e1)
    n1 + ys1.length
  }

  def check_forall: Boolean = {
    val b1 = xs1 forall { e => e < 10}
    val b2 = xs4 forall { e => e % 2 == 0 }
    b1 & b2
  }

  def check_intersect: Int = {
    val ys1 = xs1 intersect xs4
    val ys2 = xs3 intersect xs5
    ys1.length + ys2.length
  }

  def check_remove: Int = {
    val ys1 = xs1 remove { e => e % 2 != 0 }
    val ys2 = xs4 remove { e => e < 5 }
    ys1.length + ys2.length
  }

  def check_union: Int = {
    val ys1 = xs1 union xs4;
    val ys2 = xs3 union xs5;
    ys1.length + ys2.length
  }

  def check_zip: Int = {
    val ys1 = xs1 zip xs2;
    val ys2 = xs1 zip xs3;
    ys1.length + ys2.length
  }

  def check_zipAll: Int = {
    val ys1 = xs1.zipAll(xs2, 0, '_')
    val ys2 = xs2.zipAll(xs1, '_', 0)
    val ys3 = xs1.zipAll(xs3, 0, List(-1))
    ys1.length + ys2.length + ys3.length
  }

  def check_success[A](name: String, closure: => A, expected: A) {
    print("test " + name)
    try {
      val actual: A = closure
      if (actual == expected)
        print(" was successful")
      else
        print(" failed: expected "+ expected +", found "+ actual)
    }
    catch {
      case exception: Throwable =>
        print(" raised exception " + exception)
    }
    println
  }

  def main(args: Array[String]) {
    check_success("check_count",     check_count,      4)
    check_success("check_diff",      check_diff,       3)
    check_success("check_exists",    check_exists, false)
    check_success("check_filter",    check_filter,     3)
    check_success("check_foldLeft",  check_foldLeft,  10)
    check_success("check_forall",    check_forall,  true)
    check_success("check_intersect", check_intersect,  2)
    check_success("check_remove",    check_remove,     3)
    check_success("check_union",     check_union,     10)
    check_success("check_zip",       check_zip,        4)
    check_success("check_zipAll",    check_zipAll,     9)
    println
  }
}

//############################################################################
