/** Test the Scala implementation of class <code>scala.List</code>.
 *
 *  @author Stephane Micheloud
 */
import scala.language.postfixOps

object Test {
  def main(args: Array[String]) {
    Test_multiset.run() // multiset operations: union, intersect, diff
    Test1.run() //count, exists, filter, ..
    Test2.run() //#468
    Test3.run() //#1691
    Test4.run()  //#1721
    Test5.run()
  }
}

object Test_multiset {
  def run() {
    def isSubListOf[A](thiz: List[A], that: List[A]): Boolean =
      thiz forall (that contains _)
    val xs = List(1, 1, 2)
    val ys = List(1, 2, 2, 3)
    assert(List(1, 1, 2, 1, 2, 2, 3) == (xs union ys), "xs_union_ys")
    assert(List(1, 2, 2, 3, 1, 1, 2) == (ys union xs), "ys_union_xs")
    assert(List(1, 2) == (xs intersect ys), "xs_intersect_ys")
    assert(List(1, 2) == (ys intersect xs), "ys_intersect_xs")
    assert(List(1) == (xs diff ys), "xs_diff_ys")
    assert(List(2, 3) == (ys diff xs), "ys_diff_xs")
    assert(isSubListOf(xs filterNot (ys contains), xs diff ys), "xs_subset_ys")

    val zs = List(0, 1, 1, 2, 2, 2)
    assert(List(0, 1, 1, 2, 2, 2, 1, 2, 2, 3) == (zs union ys), "zs_union_ys")
    assert(List(1, 2, 2, 3, 0, 1, 1, 2, 2, 2) == (ys union zs), "ys_union_zs")
    assert(List(1, 2, 2) == (zs intersect ys), "zs_intersect_ys")
    assert(List(1, 2, 2) == (ys intersect zs), "ys_intersect_zs")
    assert(List(0, 1, 2) == (zs diff ys), "zs_diff_ys")
    assert(List(3) == (ys diff zs), "ys_diff_zs")
    assert(isSubListOf(zs filterNot (ys contains), zs diff ys), "xs_subset_ys")

    val ws = List(2)
    assert(List(2, 1, 2, 2, 3) == (ws union ys), "ws_union_ys")
    assert(List(1, 2, 2, 3, 2) == (ys union ws), "ys_union_ws")
    assert(List(2) == (ws intersect ys), "ws_intersect_ys")
    assert(List(2) == (ys intersect ws), "ys_intersect_ws")
    assert(List() == (ws diff ys), "ws_diff_ys")
    assert(List(1, 2, 3) == (ys diff ws), "ys_diff_ws")
    assert(isSubListOf(ws filterNot (ys contains), ws diff ys), "ws_subset_ys")

    val vs = List(3, 2, 2, 1)
    assert(List(1, 1, 2, 3, 2, 2, 1) == (xs union vs), "xs_union_vs")
    assert(List(3, 2, 2, 1, 1, 1, 2) == (vs union xs), "vs_union_xs")
    assert(List(1, 2) == (xs intersect vs), "xs_intersect_vs")
    assert(List(2, 1) == (vs intersect xs), "vs_intersect_xs")
    assert(List(1) == (xs diff vs), "xs_diff_vs")
    assert(List(3, 2) == (vs diff xs), "vs_diff_xs")
    assert(isSubListOf(xs filterNot (vs contains), xs diff vs), "xs_subset_vs")

    // tests adapted from Thomas Jung
    assert({
        def sort(zs: List[Int]) = zs sortWith ( _ > _ )
        sort(xs intersect ys) == sort(ys intersect xs)
      }, "be symmetric after sorting")
    assert({
        def cardinality[A](zs: List[A], e: A): Int = zs count (e == _)
        val intersection = xs intersect ys
        xs forall (e => cardinality(intersection, e) == (cardinality(xs, e)
min cardinality(ys, e)))
      }, "obey min cardinality")
    assert({
        val intersection = xs intersect ys
		val unconsumed = xs.foldLeft(intersection){(rest, e) =>
		  if (! rest.isEmpty && e == rest.head) rest.tail else rest
		}
		unconsumed.isEmpty
      }, "maintain order")
    assert(xs == (xs intersect xs),
      "has the list as again intersection")
  }
}

object Test1 {
  def run() {
    val xs1 = List(1, 2, 3)
    val xs2 = List('a', 'b')
    val xs3 = List(List(1, 2), List(4, 5))
    val xs4 = List(2, 4, 6, 8)
    val xs5 = List(List(3, 4), List(3), List(4, 5))

  {
    val n1 = xs1 count { e => e % 2 != 0 }
    val n2 = xs4 count { e => e < 5 }
    assert(4 == (n1 + n2), "check_count")
  }
  {
    val b1 = xs1 exists { e => e % 2 == 0 }
    val b2 = xs4 exists { e => e == 5 }
    assert(!(b1 & b2), "check_exists")
  }
  {
    val ys1 = xs1 filter { e => e % 2 == 0 }
    val ys2 = xs4 filter { e => e < 5 }
    assert(3 == ys1.length + ys2.length, "check_filter")
  }
  {
    val n1 = xs1.foldLeft(0)((e1, e2) => e1 + e2)
    val ys1 = xs4.foldLeft(List[Int]())((e1, e2) => e2 :: e1)
    assert(10 == n1 + ys1.length, "check_foldLeft")
  }
  {
    val b1 = xs1 forall { e => e < 10}
    val b2 = xs4 forall { e => e % 2 == 0 }
    assert(b1 & b2, "check_forall")
  }
  {
    val ys1 = xs1 filterNot { e => e % 2 != 0 }
    val ys2 = xs4 filterNot { e => e < 5 }
    assert(3 == ys1.length + ys2.length, "check_remove")
  }
  {
    val ys1 = xs1 zip xs2
    val ys2 = xs1 zip xs3
    assert(4 == ys1.length + ys2.length, "check_zip")
  }
  {
    val ys1 = xs1.zipAll(xs2, 0, '_')
    val ys2 = xs2.zipAll(xs1, '_', 0)
    val ys3 = xs1.zipAll(xs3, 0, List(-1))
    assert(9 == ys1.length + ys2.length + ys3.length, "check_zipAll")
  }
  }
}

object Test2 {
  def run() {
    val xs1 = List(1, 2, 3)
    val xs2 = List(0)

    val ys1 = xs1 ::: List(4)
    assert(List(1, 2, 3, 4) == ys1, "check_:::")

    val ys2 = ys1 filterNot (_ == 4)
    assert(xs1 == ys2, "check_-")

    val n2 = (xs1 ++ ys1).length
    val n3 = (xs1 ++ Nil).length
    val n4 = (xs1 ++ ((new collection.mutable.ArrayBuffer[Int]) += 0)).length
    assert(14 == n2 + n3 + n4, "check_++")
  }
}

object Test3 {
  def run() {
    try {
      List.range(1, 10, 0)
    } catch {
      case e: IllegalArgumentException => ()
      case _: Throwable => throw new Error("List.range(1, 10, 0)")
    }
    assert(List.range(10, 0, -2) == List(10, 8, 6, 4, 2))
  }
}

object Test4 {
  def run() {
    assert(List(1,2,3).endsWith(List(2,3)))
    assert(!List(1,2,3).endsWith(List(1,3)))
    assert(List(1,2,3).endsWith(List()))
    assert(!List(1,2,3).endsWith(List(0,1,2,3)))
    assert(List(1,2,3).endsWith(List(1,2,3)))
    assert(!List().endsWith(List(1,2,3)))
    assert(List().endsWith(List()))
  }
}

object Test5 {
  def show(xs: List[String]) = xs match {
    case "foo" :: args => args.toString
    case List(x) => x.toString
    case Nil => "Nil"
  }
  def run() {
    assert(show(List()) == "Nil")
    assert(show(List("a")) == "a")
    assert(show(List("foo", "b")) == "List(b)")
  }
}
