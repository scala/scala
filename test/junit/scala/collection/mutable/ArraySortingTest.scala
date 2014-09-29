package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

/* Tests various maps by making sure they all agree on the same answers. */
@RunWith(classOf[JUnit4])
class ArraySortingTest {
  
  class CantSortMe(val i: Int) {
    override def equals(a: Any) = throw new IllegalArgumentException("I cannot be equalled!")
  }
  
  object CanOrder extends Ordering[CantSortMe] {
    def compare(a: CantSortMe, b: CantSortMe) = a.i compare b.i
  }
  
  // Tests SI-7837
  @Test
  def sortByTest() {
    val test = Array(1,2,3,4,1,3,5,7,1,4,8,1,1,1,1)
    val cant = test.map(i => new CantSortMe(i))
    java.util.Arrays.sort(test)
    scala.util.Sorting.quickSort(cant)(CanOrder)
    assert( test(6) == 1 )
    assert( (test,cant).zipped.forall(_ == _.i) )
  }
}
