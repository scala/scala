//############################################################################
// Lists
//############################################################################

//############################################################################

import testing.SUnit._

/** Test the Scala implementation of class <code>scala.List</code>.
 *
 *  @author Stephane Micheloud
 */
object Test extends TestConsoleMain {
  def suite = new TestSuite(
    Test_multiset, // multiset operations: union, intersect, diff 
    Test1, //count, exists, filter, ..
    Test2, //#468
    Test3, //#1691
    Test4,  //#1721
    Test5
  )
}

object Test_multiset extends TestCase("multiset") with Assert {
  override def enableStackTrace = false
  override def runTest {
    def isSubListOf[A](thiz: List[A], that: List[A]): Boolean =
      thiz forall (that contains _)
    val xs = List(1, 1, 2)
    val ys = List(1, 2, 2, 3)
    assertEquals("xs_union_ys", List(1, 1, 2, 1, 2, 2, 3), xs union ys)
    assertEquals("ys_union_xs", List(1, 2, 2, 3, 1, 1, 2), ys union xs)
    assertEquals("xs_intersect_ys", List(1, 2), xs intersect ys)
    assertEquals("ys_intersect_xs", List(1, 2), ys intersect xs)
    assertEquals("xs_diff_ys", List(1), xs diff ys)
    assertEquals("ys_diff_xs", List(2, 3), ys diff xs)
    assertTrue("xs_subset_ys", isSubListOf(xs -- ys, xs diff ys))

    val zs = List(0, 1, 1, 2, 2, 2)
    assertEquals("zs_union_ys", List(0, 1, 1, 2, 2, 2, 1, 2, 2, 3), zs union ys)
    assertEquals("ys_union_zs", List(1, 2, 2, 3, 0, 1, 1, 2, 2, 2), ys union zs)
    assertEquals("zs_intersect_ys", List(1, 2, 2), zs intersect ys)
    assertEquals("ys_intersect_zs", List(1, 2, 2), ys intersect zs)
    assertEquals("zs_diff_ys", List(0, 1, 2), zs diff ys)
    assertEquals("ys_diff_zs", List(3), ys diff zs)
    assertTrue("xs_subset_ys", isSubListOf(zs -- ys, zs diff ys))

    val ws = List(2)
    assertEquals("ws_union_ys", List(2, 1, 2, 2, 3), ws union ys)
    assertEquals("ys_union_ws", List(1, 2, 2, 3, 2), ys union ws)
    assertEquals("ws_intersect_ys", List(2), ws intersect ys)
    assertEquals("ys_intersect_ws", List(2), ys intersect ws)
    assertEquals("ws_diff_ys", List(), ws diff ys)
    assertEquals("ys_diff_ws", List(1, 2, 3), ys diff ws)
    assertTrue("ws_subset_ys", isSubListOf(ws -- ys, ws diff ys))

    val vs = List(3, 2, 2, 1)
    assertEquals("xs_union_vs", List(1, 1, 2, 3, 2, 2, 1), xs union vs)
    assertEquals("vs_union_xs", List(3, 2, 2, 1, 1, 1, 2), vs union xs)
    assertEquals("xs_intersect_vs", List(1, 2), xs intersect vs)
    assertEquals("vs_intersect_xs", List(2, 1), vs intersect xs)
    assertEquals("xs_diff_vs", List(1), xs diff vs)
    assertEquals("vs_diff_xs", List(3, 2), vs diff xs)
    assertTrue("xs_subset_vs", isSubListOf(xs -- vs, xs diff vs))

    // tests adapted from Thomas Jung 
    assertTrue(
      "be symmetric after sorting", {
        def sort(zs: List[Int]) = zs sort ( _ > _ )
        sort(xs intersect ys) == sort(ys intersect xs)
      })
    assertTrue(
      "obey min cardinality", {
        def cardinality[A](zs: List[A], e: A): Int = zs count (e == _)
        val intersection = xs intersect ys
        xs forall (e => cardinality(intersection, e) == (cardinality(xs, e) 
min cardinality(ys, e)))
      })
    assertTrue(
      "maintain order", {
        val intersection = xs intersect ys
		val unconsumed = xs.foldLeft(intersection){(rest, e) =>
		  if (! rest.isEmpty && e == rest.head) rest.tail else rest
		}
		unconsumed.isEmpty
      })
    assertTrue(
      "has the list as again intersection",
      xs == (xs intersect xs)
    )
  }
}

object Test1 extends TestCase("ctor") with Assert {
  override def enableStackTrace = false
  override def runTest {
    val xs1 = List(1, 2, 3)
    val xs2 = List('a', 'b')
    val xs3 = List(List(1, 2), List(4, 5))
    val xs4 = List(2, 4, 6, 8)
    val xs5 = List(List(3, 4), List(3), List(4, 5))

  {
    val n1 = xs1 count { e => e % 2 != 0 }
    val n2 = xs4 count { e => e < 5 }
    assertEquals("check_count", 4, n1 + n2)
  }
  {
    val b1 = xs1 exists { e => e % 2 == 0 }
    val b2 = xs4 exists { e => e == 5 }
    assertEquals("check_exists", false , b1 & b2)
  }
  {
    val ys1 = xs1 filter { e => e % 2 == 0 }
    val ys2 = xs4 filter { e => e < 5 }
    assertEquals("check_filter", 3, ys1.length + ys2.length)
  }
  {
    val n1 = xs1.foldLeft(0)((e1, e2) => e1 + e2)
    val ys1 = xs4.foldLeft(List[Int]())((e1, e2) => e2 :: e1)
    assertEquals("check_foldLeft", 10, n1 + ys1.length)
  }
  {
    val b1 = xs1 forall { e => e < 10}
    val b2 = xs4 forall { e => e % 2 == 0 }
    assertEquals("check_forall", true, b1 & b2)
  }
  {
    val ys1 = xs1 filterNot { e => e % 2 != 0 }
    val ys2 = xs4 filterNot { e => e < 5 }
    assertEquals("check_remove", 3, ys1.length + ys2.length)
  }
  {
    val ys1 = xs1 zip xs2
    val ys2 = xs1 zip xs3
    assertEquals("check_zip", 4, ys1.length + ys2.length)
  }
  {
    val ys1 = xs1.zipAll(xs2, 0, '_')
    val ys2 = xs2.zipAll(xs1, '_', 0)
    val ys3 = xs1.zipAll(xs3, 0, List(-1))
    assertEquals("check_zipAll", 9, ys1.length + ys2.length + ys3.length)
  }
  }
}

object Test2 extends TestCase("t0468") with Assert {
  override def enableStackTrace = false
  override def runTest {
    val xs1 = List(1, 2, 3)
    val xs2 = List(0)
 
    val ys1 = xs1 ::: List(4)
    assertEquals("check_:::", List(1, 2, 3, 4), ys1)

    val ys2 = ys1 - 4
    assertEquals("check_-", xs1, ys2)

    val n2 = (xs1 ++ ys1).length
    val n3 = (xs1 ++ Nil).length
    val n4 = (xs1 ++ ((new collection.mutable.ArrayBuffer[Int]) + 0)).length
    assertEquals("check_++", 14, n2 + n3 + n4)
  }
}

object Test3 extends TestCase("t1691") with Assert {
  override def enableStackTrace = false
  override def runTest {
    try {
      List.range(1, 10, 0)
    } catch {
      case e: IllegalArgumentException => ()
      case _ => throw new Error("List.range(1, 10, 0)")
    }
    assertEquals(List.range(10, 0, -2),
		 List(10, 8, 6, 4, 2))
  }
}

object Test4 extends TestCase("t1721") with Assert {
  override def enableStackTrace = false
  override def runTest {
    assertTrue(List(1,2,3).endsWith(List(2,3)))
    assertFalse(List(1,2,3).endsWith(List(1,3)))
    assertTrue(List(1,2,3).endsWith(List()))
    assertFalse(List(1,2,3).endsWith(List(0,1,2,3)))
    assertTrue(List(1,2,3).endsWith(List(1,2,3)))
    assertFalse(List().endsWith(List(1,2,3)))
    assertTrue(List().endsWith(List()))
  }
}

object Test5 extends TestCase("list pattern matching") {
  def show(xs: List[String]) = xs match {
    case "foo" :: args => args.toString
    case List(x) => x.toString
    case Nil => "Nil"
  }
  override def runTest {
    assert(show(List()) == "Nil")
    assert(show(List("a")) == "a")
    assert(show(List("foo", "b")) == "List(b)")
  }
}
