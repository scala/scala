package strawman.collection.mutable

import scala.Predef.{genericArrayOps => _, classOf, assert, intWrapper}
import strawman.collection.arrayToArrayOps

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
  
  // Tests scala/bug#7837
  @Test
  def sortByTest(): Unit = {
    val test = Array(1,2,3,4,1,3,5,7,1,4,8,1,1,1,1)
    val cant = test.map(i => new CantSortMe(i))
    java.util.Arrays.sort(test)
    scala.util.Sorting.quickSort(cant)(CanOrder)
    assert( test(6) == 1 )
    assert( (test zip cant).forall { case (i, csm) => i == csm.i } )
  }
}
