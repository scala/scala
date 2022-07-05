
object Sizes {
  def list: Int = 24
  def listBuffer: Int = 32

  def refArray(length:Int): Int = (16 + (length+1) * 4) /8 * 8
  def wrappedRefArray(length:Int): Int = refArray(length) + 16
  def wrappedRefArrayIterator: Int = 24
}

object Test extends scala.tools.testkit.AllocationTest {
  def main(args: Array[String]): Unit = {
    smallSeqAllocation
    //largeSeqAllocation
  }
  def smallSeqAllocation: Unit = {
    exactAllocates(Sizes.list * 1, "collection seq  size 1")(Seq("0"))
    exactAllocates(Sizes.list * 2, "collection seq  size 2")(Seq("0", "1"))
    exactAllocates(Sizes.list * 3, "collection seq  size 3")(Seq("0", "1", ""))
    exactAllocates(Sizes.list * 4, "collection seq  size 4")(Seq("0", "1", "2", "3"))
    exactAllocates(Sizes.list * 5, "collection seq  size 5")(Seq("0", "1", "2", "3", "4"))
    exactAllocates(Sizes.list * 6, "collection seq  size 6")(Seq("0", "1", "2", "3", "4", "5"))
    exactAllocates(Sizes.list * 7, "collection seq  size 7")(Seq("0", "1", "2", "3", "4", "5", "6"))
  }
  def largeSeqAllocation: Unit = {
    exactAllocates(Sizes.list * 10 + Sizes.wrappedRefArray(10) + Sizes.listBuffer + 16, "collection seq size 10")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    exactAllocates(Sizes.list * 20 + Sizes.wrappedRefArray(20) + Sizes.listBuffer + 16, "collection seq size 20")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  }
}

/*
restored partest (cf junit) to test outside scala.collection package

cost of tracking allocations - cost of Object = 0
java.lang.AssertionError: allocating min = 88 allowed = 24 -- collection seq  size 1
 result = List(0) (class scala.collection.immutable.$colon$colon)
 allocation 88 (1000 times)

        at org.junit.Assert.fail(Assert.java:89)
        at scala.tools.testkit.AllocationTest.failTest(AllocationTest.scala:111)
        at scala.tools.testkit.AllocationTest.exactAllocates(AllocationTest.scala:102)
        at scala.tools.testkit.AllocationTest.exactAllocates$(AllocationTest.scala:100)
        at Test$.exactAllocates(small-seq-apply.scala:11)
        at Test$.smallSeqAllocation(small-seq-apply.scala:17)
 */
