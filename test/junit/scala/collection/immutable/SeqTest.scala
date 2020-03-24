package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Assert, Test}

import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class SeqTest extends AllocationTest{

  @Test def emptyNonAllocating(): Unit = {
    nonAllocating(Seq.empty)
    nonAllocating(Seq())
  }
}
