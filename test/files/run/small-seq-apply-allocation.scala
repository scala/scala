
object Main extends scala.tools.testkit.AllocationTest {
  def main(args: Array[String]): Unit = {
    smallSeqAllocation
    largeSeqAllocation
  }
  @Test def smallSeqAllocation: Unit = {
    exactAllocates(Sizes.list * 1, "collection seq  size 1")(Seq("0"))
    exactAllocates(Sizes.list * 2, "collection seq  size 2")(Seq("0", "1"))
    exactAllocates(Sizes.list * 3, "collection seq  size 3")(Seq("0", "1", ""))
    exactAllocates(Sizes.list * 4, "collection seq  size 4")(Seq("0", "1", "2", "3"))
    exactAllocates(Sizes.list * 5, "collection seq  size 5")(Seq("0", "1", "2", "3", "4"))
    exactAllocates(Sizes.list * 6, "collection seq  size 6")(Seq("0", "1", "2", "3", "4", "5"))
    exactAllocates(Sizes.list * 7, "collection seq  size 7")(Seq("0", "1", "2", "3", "4", "5", "6"))
  }
  @Test def largeSeqAllocation: Unit = {
    exactAllocates(Sizes.list * 10 + Sizes.wrappedRefArray(10) + Sizes.listBuffer + 16, "collection seq size 10")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    exactAllocates(Sizes.list * 20 + Sizes.wrappedRefArray(20) + Sizes.listBuffer + 16, "collection seq size 20")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  }
}