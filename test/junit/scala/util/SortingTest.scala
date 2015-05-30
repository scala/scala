package scala.util

import org.junit.Test
import org.junit.Assert._
import scala.math.{ Ordered, Ordering }
import scala.reflect.ClassTag

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
    val rxs = { val temp = xs.clone; Sorting.stableSort(temp)(implicitly[ClassTag[N]], backwardsN); temp }
    val sys = Sorting.stableSort(ys.clone: Seq[Int], (i: Int) => xs(i))
    
    assertTrue("Quicksort should be in order", isSorted(qxs))
    assertTrue("Quicksort should be in reverse order", isAntisorted(pxs))
    assertTrue("Stable sort should be sorted and stable", isStable(sxs))
    assertTrue("Stable sort should be reverse sorted but stable", isAntistable(rxs))
    assertTrue("Stable sorting by proxy should produce sorted stable list", isStable(sys.map(i => xs(i))))
    assertTrue("Quicksort should produce canonical ordering", (qxs zip zs).forall{ case (a,b) => a.i == b.i })
    assertTrue("Reverse quicksort should produce canonical ordering", (pxs.reverse zip zs).forall{ case (a,b) => a.i == b.i })
    assertTrue("Stable sort should produce exact ordering", (sxs zip zs).forall{ case (a,b) => a == b })
    assertTrue("Reverse stable sort should produce canonical ordering", (rxs.reverse zip zs).forall{ case (a,b) => a.i == b.i })
    assertTrue("Proxy sort and direct sort should produce exactly the same thing", (sxs zip sys.map(i => xs(i))).forall{ case (a,b) => a == b })
  }
  
  @Test def testSortConsistency: Unit = {
    for {
      size <- sizes
      v <- variety
      i <- 0 until math.min(100, math.max(math.min(math.floor(math.pow(v, size)/2), math.ceil(workLimit / (math.log(math.max(2,size))/math.log(2) * size))), 1).toInt)
    } runOneTest(size, v)
    
    for (size <- sizes) {
      val b = Array.fill(size)(rng.nextBoolean)
      val bfwd = Sorting.stableSort(b.clone: Seq[Boolean])
      val bbkw = Sorting.stableSort(b.clone: Seq[Boolean], (x: Boolean, y: Boolean) => x && !y)
      assertTrue("All falses should be first", bfwd.dropWhile(_ == false).forall(_ == true))
      assertTrue("All falses should be last when sorted backwards", bbkw.dropWhile(_ == true).forall(_ == false))
      assertTrue("Sorting booleans should preserve the number of trues", b.count(_ == true) == bfwd.count(_ == true))
      assertTrue("Backwards sorting booleans should preserve the number of trues", b.count(_ == true) == bbkw.count(_ == true))
      assertTrue("Sorting should not change the sizes of arrays", b.length == bfwd.length && b.length == bbkw.length)
    }
  }
}
