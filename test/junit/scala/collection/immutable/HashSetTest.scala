package scala.collection.immutable

import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class HashSetTest extends AllocationTest {


  def generate(): HashSet[String] = {
    (1 to 1000).map { i => s"key $i" }(scala.collection.breakOut)
  }

  @Test
  def nonAllocatingIdentical(): Unit = {
    val base = generate()
    assertTrue(nonAllocating {
      base == base
    })
  }

  @Test
  def nonAllocatingNotShared(): Unit = {
    val base = generate()
    val notShared = generate()

    assertTrue(nonAllocating {
      base == notShared
    })
    assertTrue(nonAllocating {
      notShared == base
    })
  }

  @Test
  def nonAllocatingShared(): Unit = {
    val base = generate()
    val shared = (base - base.head) + base.head

    assertTrue(nonAllocating {
      base == shared
    })
    assertTrue(nonAllocating {
      shared == base
    })
  }

}
