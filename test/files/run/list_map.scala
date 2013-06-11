import scala.collection.immutable.ListMap

object Test {
  def testHeadTailSync() {
    val r = 1 to 100
    var lm = ListMap(r map (i => (i, i)): _*)
    for (i <- r) {
      val (j, _) = lm.head
      assert (j == i, s"$j != $i")
      lm = lm.tail
    }
  }

  def main(args: Array[String]) {
    testHeadTailSync()
  }
}
