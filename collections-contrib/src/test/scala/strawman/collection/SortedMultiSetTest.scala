package strawman.collection


import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SortedMultiSetTest {

  def sortedMultiSet(sms: SortedMultiSet[Int]): Unit = {
    Assert.assertEquals(1, sms.get(1))
    Assert.assertEquals(2, sms.get(2))
    Assert.assertEquals(1, sms.firstKey)
    Assert.assertEquals(3, sms.lastKey)
    Assert.assertEquals(SortedMultiSet(3, 2, 2), sms.from(2))
  }

  @Test def run(): Unit = {
    sortedMultiSet(immutable.SortedMultiSet(2, 1, 3, 2))
    sortedMultiSet(mutable.SortedMultiSet(2, 1, 3, 2))
  }

}
