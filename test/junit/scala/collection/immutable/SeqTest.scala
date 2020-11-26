package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.collection.Sizes
import scala.tools.testkit.{AllocationTest, CompileTime}

@RunWith(classOf[JUnit4])
class SeqTest extends AllocationTest {

  @Test def emptyNonAllocating(): Unit = {
    nonAllocating(Seq.empty)
    nonAllocating(Seq())
  }

  @Test def smallSeqAllocation(): Unit = {
    if (CompileTime.versionNumberString == "2.13.2") return
    exactAllocates(Sizes.list * 1, "immutable seq  size 1")(Seq("0"))
    exactAllocates(Sizes.list * 2, "immutable seq  size 2")(Seq("0", "1"))
    exactAllocates(Sizes.list * 3, "immutable seq  size 3")(Seq("0", "1", ""))
    exactAllocates(Sizes.list * 4, "immutable seq  size 4")(Seq("0", "1", "2", "3"))
    exactAllocates(Sizes.list * 5, "immutable seq  size 5")(Seq("0", "1", "2", "3", "4"))
    exactAllocates(Sizes.list * 6, "immutable seq  size 6")(Seq("0", "1", "2", "3", "4", "5"))
    exactAllocates(Sizes.list * 7, "immutable seq  size 7")(Seq("0", "1", "2", "3", "4", "5", "6"))
  }
  @Test def largeSeqAllocation(): Unit = {
    def expected(n: Int) = Sizes.list * n + Sizes.wrappedRefArray(n) + Sizes.wrappedRefArrayIterator
    exactAllocates(expected(10), "immutable seq size 10")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    exactAllocates(expected(20), "immutable seq size 20")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  }

}
