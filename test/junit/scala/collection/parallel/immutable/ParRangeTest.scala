package scala.collection.parallel.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class ParRangeTest {

  @Test
  def buildParRangeString {
    assert(ParRange(1, 5, 1, true).toString == "ParRange 1 to 5")
  }

  @Test
  def zipWithIndex {
    assert((1 to 3).par.zipWithIndex.toList == List((1, 0), (2, 1), (3, 2)))
    assert((1 to 3).par.zipWithIndex(42).toList == List((1, 42), (2, 43), (3, 44)))
  }

}
