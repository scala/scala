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

}
