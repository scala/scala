package scala.util

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import scala.math.{Ordered, Ordering}

class SortingTest {
  case class N(i: Int, j: Int) extends Ordered[N] { def compare(n: N) = if (i < n.i) -1 else if (i > n.i) 1 else 0 }

  def mkA(n: Int, max: Int) = Array.tabulate(n)(i => N(util.Random.nextInt(max), i))
  
  def isStable(a: Array[N]): Boolean = { var i = 1; while (i < a.length) { if (a(i).i < a(i-1).i || (a(i).i == a(i-1).i && a(i).j < a(i-1).j)) return false; i += 1 }; true }
  
  def isAntistable(a: Array[N]): Boolean =
    { var i = 1; while (i < a.length) { if (a(i).i > a(i-1).i || (a(i).i == a(i-1).i && a(i).j < a(i-1).j)) return false; i += 1 }; true }
  
  def isSorted(a: Array[N]): Boolean = { var i = 1; while (i < a.length) { if (a(i).i < a(i-1).i) return false; i += 1 }; true }
  
  def isAntisorted(a: Array[N]): Boolean = { var i = 1; while (i < a.length) { if (a(i).i > a(i-1).i) return false; i += 1 }; true }
  
  val sizes = Seq.range(0, 65) ++ Seq(256, 1024, 9121, 65539)
  val variety = Seq(1, 2, 10, 100, 1000, Int.MaxValue)
  val workLimit = 1e6
  val rng = new util.Random(198571)
  
  val backwardsN = Ordering by ((n: N) => -n.i)
  
  def runOneTest(size: Int, variety: Int): Unit = {
    val xs = Array.tabulate(size)(i => N(rng.nextInt(variety), i))
    val ys = Array.range(0, xs.length)
    val zs = { val temp = xs.clone; java.util.Arrays.sort(temp, new java.util.Comparator[N] { def compare(a: N, b: N) = a.compare(b) }); temp }
    val qxs = { val temp = xs.clone; Sorting.quickSort(temp); temp }
    val pxs = { val temp = xs.clone; Sorting.quickSort(temp)(backwardsN); temp }
    val sxs = { val temp = xs.clone; Sorting.stableSort(temp); temp }
    val rxs = { val temp = xs.clone; Sorting.stableSort(temp)(backwardsN); temp }
    val sys = Sorting.stableSort(ys.clone.toIndexedSeq, (i: Int) => xs(i))
    
    assertTrue(isSorted(qxs), "Quicksort should be in order")
    assertTrue(isAntisorted(pxs), "Quicksort should be in reverse order")
    assertTrue(isStable(sxs), "Stable sort should be sorted and stable")
    assertTrue(isAntistable(rxs), "Stable sort should be reverse sorted but stable")
    assertTrue(isStable(sys.map(i => xs(i))), "Stable sorting by proxy should produce sorted stable list")
    assertTrue((qxs zip zs).forall{ case (a,b) => a.i == b.i }, "Quicksort should produce canonical ordering")
    assertTrue((pxs.reverse zip zs).forall{ case (a,b) => a.i == b.i }, "Reverse quicksort should produce canonical ordering")
    assertTrue((sxs zip zs).forall{ case (a,b) => a == b }, "Stable sort should produce exact ordering")
    assertTrue((rxs.reverse zip zs).forall{ case (a,b) => a.i == b.i }, "Reverse stable sort should produce canonical ordering")
    assertTrue((sxs zip sys.map(i => xs(i))).forall{ case (a,b) => a == b }, "Proxy sort and direct sort should produce exactly the same thing")
  }
  
  @Test def testSortConsistency(): Unit = {
    for {
      size <- sizes
      v <- variety
      i <- 0 until math.min(100, math.max(math.min(math.floor(math.pow(v, size)/2), math.ceil(workLimit / (math.log(math.max(2,size))/math.log(2) * size))), 1).toInt)
    } runOneTest(size, v)
    
    for (size <- sizes) {
      val b = Array.fill(size)(rng.nextBoolean())
      val bfwd = Sorting.stableSort(b.clone.toIndexedSeq)
      val bbkw = Sorting.stableSort(b.clone.toIndexedSeq, (x: Boolean, y: Boolean) => x && !y)
      assertTrue(bfwd.dropWhile(_ == false).forall(_ == true), "All falses should be first")
      assertTrue(bbkw.dropWhile(_ == true).forall(_ == false), "All falses should be last when sorted backwards")
      assertTrue(b.count(_ == true) == bfwd.count(_ == true), "Sorting booleans should preserve the number of trues")
      assertTrue(b.count(_ == true) == bbkw.count(_ == true), "Backwards sorting booleans should preserve the number of trues")
      assertTrue(b.length == bfwd.length && b.length == bbkw.length, "Sorting should not change the sizes of arrays")
    }
  }
}
