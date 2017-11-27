package strawman.collection.mutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SortedMultiSetTest {

  @Test
  def sortedMultiSet(): Unit = {
    val sms = SortedMultiSet(2, 1, 3, 2)
    Assert.assertEquals(1, sms.get(1))
    Assert.assertEquals(2, sms.get(2))
    Assert.assertEquals(1, sms.firstKey)
    Assert.assertEquals(3, sms.lastKey)
    sms += 2
    Assert.assertEquals(3, sms.get(2))
    sms -= 3
    Assert.assertFalse(sms.contains(3))
  }

}
