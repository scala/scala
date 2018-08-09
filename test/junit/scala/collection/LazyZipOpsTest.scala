package scala.collection

import org.hamcrest.CoreMatchers._
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.immutable._

@RunWith(classOf[JUnit4])
class LazyZipOpsTest {

  private val ws = List(1, 2, 3)
  private val xs = List(1, 2, 3, 4, 5, 6)
  private val ys = List("a", "b", "c", "d", "e", "f")
  private val zs = List(true, false, true, false, true, false)
  private val zipped2 = ws lazyZip xs
  private val zipped3 = ws lazyZip xs lazyZip ys
  private val zipped4 = ws lazyZip xs lazyZip ys lazyZip zs
  private val map = Map(1 -> "foo" , 2 -> "bar")
  private val sortedMap = TreeMap(1 -> "foo" , 2 -> "bar")
  private val sortedSet = TreeSet(1, 2, 3)

  @Test
  def lazyZip2_map(): Unit = {
    val res: List[(Int, Int)] = zipped2.map((a, b) => (a, b))

    assertEquals(List((1, 1), (2, 2), (3, 3)), res)
  }

  @Test
  def lazyZip2_flatMap(): Unit = {
    val res: List[(Int, Int)] = zipped2.flatMap((a, b) => List((a, b)))

    assertEquals(List((1, 1), (2, 2), (3, 3)), res)
  }

  @Test
  def lazyZip2_filter(): Unit = {
    val res: List[(Int, Int)] = zipped2.filter((a, _) => a % 2 == 0)

    assertEquals(List((2, 2)), res)
  }

  @Test
  def lazyZip2_exists(): Unit = {
    assertTrue(zipped2.exists((a, b) => a + b > 5))
    assertFalse(zipped2.exists((a, b) => a + b < 0))
  }

  @Test
  def lazyZip2_forall(): Unit = {
    assertTrue(zipped2.forall((a, b) => a + b > 0))
    assertFalse(zipped2.forall((a, b) => a + b > 2))
  }

  @Test
  def lazyZip2_foreach(): Unit = {
    var res = ""
    zipped2.foreach((a, b) => res += s"[$a,$b]")

    assertEquals("[1,1][2,2][3,3]", res)
  }

  @Test
  def lazyZip2_toIterable(): Unit = {
    val iter: collection.Iterable[(Int, Int)] = zipped2

    assertEquals(List((1, 1), (2, 2), (3, 3)), iter.to(List))
  }

  @Test
  def lazyZip2_empty(): Unit = {
    assertTrue(Nil.lazyZip(xs).isEmpty)
    assertTrue(xs.lazyZip(Nil).isEmpty)
  }

  @Test
  def lazyZip2_withOrdering(): Unit = {
    val res: TreeSet[Int] = sortedSet.lazyZip(ws).map(_ + _)

    assertEquals(TreeSet(2, 4, 6), res)
  }

  @Test
  def lazyZip2_withMap(): Unit = {
    val res: Map[Int, (String, String)] = map.lazyZip(ys).map { case ((k, v), s) => k -> (s, v) }

    assertThat(res, either(
      is(Map(1 -> ("a", "foo"), 2 -> ("b", "bar"))))
      .or(is(Map(1 -> ("b", "foo"), 2 -> ("a", "bar"))))
    )
  }

  @Test
  def lazyZip2_withSortedMap(): Unit = {
    val res: TreeMap[Int, (String, String)] = sortedMap.lazyZip(ys).map { case ((k, v), s) => k -> (s, v) }

    assertEquals(Map(1 -> ("a", "foo"), 2 -> ("b", "bar")), res)
  }

  @Test
  def lazyZip3_map(): Unit = {
    val res: List[(Int, Int, String)] = zipped3.map((a, b, c) => (a, b, c))

    assertEquals(List((1, 1, "a"), (2, 2, "b"), (3, 3, "c")), res)
  }

  @Test
  def lazyZip3_flatMap(): Unit = {
    val res: List[(Int, Int, String)] = zipped3.flatMap((a, b, c) => List((a, b, c)))

    assertEquals(List((1, 1, "a"), (2, 2, "b"), (3, 3, "c")), res)
  }

  @Test
  def lazyZip3_filter(): Unit = {
    val res: List[(Int, Int, String)] = zipped3.filter((a, _, _) => a % 2 != 0)

    assertEquals(List((1, 1, "a"), (3, 3, "c")), res)
  }

  @Test
  def lazyZip3_exists(): Unit = {
    assertTrue(zipped3.exists((a, b, _) => a + b > 5))
    assertFalse(zipped3.exists((a, b, _) => a + b < 0))
  }

  @Test
  def lazyZip3_forall(): Unit = {
    assertTrue(zipped3.forall((a, b, _) => (a + b) % 2 == 0))
    assertFalse(zipped3.forall((a, b, _) => a + b > 5))
  }

  @Test
  def lazyZip3_foreach(): Unit = {
    var res = ""
    zipped3.foreach((a, b, c) => res += s"[$a,$b,$c]")

    assertEquals("[1,1,a][2,2,b][3,3,c]", res)
  }

  @Test
  def lazyZip3_toIterable(): Unit = {
    val iter: collection.Iterable[(Int, Int, String)] = zipped3

    assertEquals(List((1, 1, "a"), (2, 2, "b"), (3, 3, "c")), iter.to(List))
  }

  @Test
  def lazyZip3_empty(): Unit = {
    assertTrue(zipped2.lazyZip(Nil).isEmpty)
    assertTrue(Nil.lazyZip(Nil).lazyZip(xs).isEmpty)
  }

  @Test
  def lazyZip3_withOrdering(): Unit = {
    val res: TreeSet[Int] = sortedSet.lazyZip(xs).lazyZip(ws).map(_ + _ + _)

    assertEquals(TreeSet(3, 6, 9), res)
  }

  @Test
  def lazyZip3_withMap(): Unit = {
    val res: Map[Int, (Int, String, String)] = map.lazyZip(ws).lazyZip(ys).map { case ((k, v), w, y) => k -> (w, y, v) }

    assertThat(res, either(
      is(Map(1 -> (1, "a", "foo"), 2 -> (2, "b", "bar"))))
      .or(is(Map(1 -> (2, "b", "foo"), 2 -> (1, "a", "bar"))))
    )
  }

  @Test
  def lazyZip3_withSortedMap(): Unit = {
    val res: TreeMap[Int, (Int, String, String)] = sortedMap.lazyZip(ws).lazyZip(ys)
      .map { case ((k, v), w, y) => k -> (w, y, v) }

    assertEquals(Map(1 -> (1, "a", "foo"), 2 -> (2, "b", "bar")), res)
  }

  @Test
  def lazyZip4_map(): Unit = {
    val res: List[(Int, Int, String, Boolean)] = zipped4.map((a, b, c, d) => (a, b, c, d))

    assertEquals(List((1, 1, "a", true), (2, 2, "b", false), (3, 3, "c", true)), res)
  }

  @Test
  def lazyZip4_flatMap(): Unit = {
    val res: List[(Int, Int, String, Boolean)] = zipped4.flatMap((a, b, c, d) => List((a, b, c, d)))

    assertEquals(List((1, 1, "a", true), (2, 2, "b", false), (3, 3, "c", true)), res)
  }

  @Test
  def lazyZip4_filter(): Unit = {
    val res: List[(Int, Int, String, Boolean)] = zipped4.filter((_, _, _, d) => d)

    assertEquals(List((1, 1, "a", true), (3, 3, "c", true)), res)
  }

  @Test
  def lazyZip4_exists(): Unit = {
    assertTrue(zipped4.exists((a, b, c, d) => a + b > 5 && !c.isEmpty && d))
    assertFalse(zipped4.exists((a, b, c, d) => a + b > 5 && !c.isEmpty && !d))
  }

  @Test
  def lazyZip4_forall(): Unit = {
    assertTrue(zipped4.forall((a, b, _, _) => (a + b) % 2 == 0))
    assertFalse(zipped4.forall((a, b, _, d) => a + b > 0 && d))
  }

  @Test
  def lazyZip4_foreach(): Unit = {
    var res = ""
    zipped4.foreach((a, b, c, d) => res += s"[$a,$b,$c,$d]")

    assertEquals("[1,1,a,true][2,2,b,false][3,3,c,true]", res)
  }

  @Test
  def lazyZip4_toIterable(): Unit = {
    val iter: collection.Iterable[(Int, Int, String, Boolean)] = zipped4

    assertEquals(List((1, 1, "a", true), (2, 2, "b", false), (3, 3, "c", true)), iter.to(List))
  }

  @Test
  def lazyZip4_empty(): Unit = {
    assertTrue(zipped3.lazyZip(Nil).isEmpty)
    assertTrue(Nil.lazyZip(Nil).lazyZip(Nil).lazyZip(xs).isEmpty)
  }

  @Test
  def lazyZip4_withOrdering(): Unit = {
    val res: TreeSet[Int] = sortedSet.lazyZip(xs).lazyZip(ws).lazyZip(ws).map(_ + _ + _ + _)

    assertEquals(TreeSet(4, 8, 12), res)
  }

  @Test
  def lazyZip4_withMap(): Unit = {
    val res: Map[Int, (Int, Int, String, String)] = map.lazyZip(ws).lazyZip(xs).lazyZip(ys)
      .map { case ((k, v), w, x, y) => k -> (w, x, y, v) }

    assertThat(res, either(
      is(Map(1 -> (1, 1, "a", "foo"), 2 -> (2, 2, "b", "bar"))))
      .or(is(Map(1 -> (2, 2, "b", "foo"), 2 -> (1, 1, "a", "bar"))))
    )
  }

  @Test
  def lazyZip4_withSortedMap(): Unit = {
    val res: TreeMap[Int, (Int, Int, String, String)] = sortedMap.lazyZip(ws).lazyZip(xs).lazyZip(ys)
      .map { case ((k, v), w, x, y) => k -> (w, x, y, v) }

    assertEquals(Map(1 -> (1, 1, "a", "foo"), 2 -> (2, 2, "b", "bar")), res)
  }

  @Test
  def lazyZipArray: Unit = {
    val a = Array(1,2,3).lazyZip(List(4,5,6)).map(_ + _)
    val at: Array[Int] = a
    assertArrayEquals(Array(5, 7, 9), at)
  }

  @Test
  def lazyZipString: Unit = {
    val v = "abc".lazyZip(List(1,2,3)).map((a, b) => a.toInt + b.toInt)
    val vt: IndexedSeq[Int] = v
    assertEquals(Vector(98, 100, 102), vt)
    val s = "abc".lazyZip(List(1,2,3)).map((a, b) => (a.toInt + b.toInt).toChar)
    val st: String = s
    assertEquals("bdf", st)
  }
}