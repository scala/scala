package scala.collection.immutable
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testing.AllocationTest

class MapHashcodeTest extends AllocationTest {

  @Test def nonAllocatingCustom(): Unit = {
    val t0 = Map.empty
    val t1 = new Map.Map1(1,1)
    val t2 = new Map.Map2(1,1, 2,2)
    val t3 = new Map.Map3(1,1, 2,2, 3,3)
    val t4 = new Map.Map4(1,1, 2,2, 3,3, 4,4)

    nonAllocating{ t0.hashCode() }
    nonAllocating{ t0.## }

    nonAllocating{ t1.hashCode() }
    nonAllocating{ t1.## }

    nonAllocating{ t2.hashCode() }
    nonAllocating{ t2.## }

    nonAllocating{ t3.hashCode() }
    nonAllocating{ t3.## }

    nonAllocating{ t4.hashCode() }
    nonAllocating{ t4.## }
  }

  @Test def nonAllocatingListMap(): Unit = {
    val t0 = ListMap.empty[String, String]
    val t1 = t0.updated("1","1")
    val t2 = t1.updated("2","2")
    val t3 = t2.updated("3","3")
    val t4 = t3.updated("4","4")

    val tlarge = (1 to 10000).foldLeft(t0){
      (a,b) => a.updated(b.toString,b.toString)
    }

    nonAllocating{ t0.hashCode() }
    nonAllocating{ t0.## }

    onlyAllocates(32){ t1.hashCode() }
    onlyAllocates(32){ t1.## }

    onlyAllocates(32){ t2.hashCode() }
    onlyAllocates(32){ t2.## }

    onlyAllocates(32){ t3.hashCode() }
    onlyAllocates(32){ t3.## }

    onlyAllocates(32){ t4.hashCode() }
    onlyAllocates(32){ t4.## }

    onlyAllocates(32){ tlarge.hashCode() }
    onlyAllocates(32){ tlarge.## }
  }

  @Test def nonAllocatingSortedMap(): Unit = {
    val t0 = SortedMap.empty[String, String]
    val t1 = t0.updated("1","1")
    val t2 = t1.updated("2","2")
    val t3 = t2.updated("3","3")
    val t4 = t3.updated("4","4")

    val tlarge = (1 to 10000).foldLeft(t0){
      (a,b) => a.updated(b.toString,b.toString)
    }

    onlyAllocates(32){ t0.hashCode() }
    onlyAllocates(32){ t0.## }

    onlyAllocates(32){ t1.hashCode() }
    onlyAllocates(32){ t1.## }

    onlyAllocates(32){ t2.hashCode() }
    onlyAllocates(32){ t2.## }

    onlyAllocates(32){ t3.hashCode() }
    onlyAllocates(32){ t3.## }

    onlyAllocates(32){ t4.hashCode() }
    onlyAllocates(32){ t4.## }

    onlyAllocates(32){ tlarge.hashCode() }
    onlyAllocates(32){ tlarge.## }
  }

  @Test def nonAllocatingHashMap(): Unit = {
    val t0 = HashMap.empty[String, String]
    val t1 = t0.updated("1","1")
    val t2 = t1.updated("2","2")
    val t3 = t2.updated("3","3")
    val t4 = t3.updated("4","4")

    val tlarge = (1 to 10000).foldLeft(t0){
      (a,b) => a.updated(b.toString,b.toString)
    }

    nonAllocating{ t0.hashCode() }
    nonAllocating{ t0.## }

    onlyAllocates(32){ t1.hashCode() }
    onlyAllocates(32){ t1.## }

    onlyAllocates(32){ t2.hashCode() }
    onlyAllocates(32){ t2.## }

    onlyAllocates(32){ t3.hashCode() }
    onlyAllocates(32){ t3.## }

    onlyAllocates(32){ t4.hashCode() }
    onlyAllocates(32){ t4.## }

    onlyAllocates(32){ tlarge.hashCode() }
    onlyAllocates(32){ tlarge.## }
  }
  object MyEmptyMap extends AbstractMap[String, String] {
    override def +[V1 >: String](kv: (String, V1)): Map[String, V1] = ???
    override def get(key: String): Option[String] = None
    override def -(key: String): Map[String, String] = this

    override def iterator: Iterator[(String, String)] = Iterator.empty
  }
  @Test def emptyHashCodes() {
    val expected = MyEmptyMap.hashCode()
    assertEquals(expected, Map.empty.hashCode())
    assertEquals(expected, ListMap.empty.hashCode())
    assertEquals(expected, SortedMap.empty[String, Object].hashCode())
    assertEquals(expected, HashMap.empty.hashCode())
  }
}
