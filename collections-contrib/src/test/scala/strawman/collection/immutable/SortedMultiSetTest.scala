package strawman.collection.immutable

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
    Assert.assertEquals(SortedMultiSet(3, 2, 2), sms.from(2))
    val sms2 = sms + 2
    Assert.assertEquals(3, sms2.get(2))
    val sms3 = sms2 - 3
    Assert.assertFalse(sms3.contains(3))
  }

}
