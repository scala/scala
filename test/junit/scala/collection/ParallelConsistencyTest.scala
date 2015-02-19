package scala.collection.immutable

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ParallelConsistencyTest {

  private val theSeq = Seq(1,2,3)

  // This collection will throw an exception if you do anything but call .length or .seq
  private val mustCallSeq: collection.GenSeq[Int] = new collection.parallel.ParSeq[Int] {
    def length = 3
    
    // This method is surely sequential & safe -- want all access to go through here
    def seq = theSeq
    
    def notSeq = throw new Exception("Access to parallel collection not via .seq")
    
    // These methods could possibly be used dangerously explicitly or internally
    // (apply could also be used safely; if it is, do test with mustCallSeq)
    def apply(i: Int) = notSeq
    def splitter = notSeq
  }  

   // Test Vector ++ with a small parallel collection concatenation (SI-9072).
  @Test
  def testPlusPlus(): Unit = {
    assert((Vector.empty ++ mustCallSeq) == theSeq, "Vector ++ unsafe with parallel vectors")
  }
  
  // SI-9126, 1 of 2
  @Test
  def testTranspose(): Unit = {
    assert(List(mustCallSeq).transpose.flatten == theSeq, "Transposing inner parallel collection unsafe")
  }
  
  // SI-9126, 2 of 2
  @Test
  def testList_flatMap(): Unit = {
    assert(List(1).flatMap(_ => mustCallSeq) == theSeq, "List#flatMap on inner parallel collection unsafe")
  }
}
