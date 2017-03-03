
import scala.collection.convert.ImplicitConversionsToJava._

object Test {

  def main(args:Array[String]) = {
    val tests = 5000
    val jm: java.util.Map[Int, Int] = scala.collection.mutable.Map((0 until tests) zip (0 until tests).reverse: _*)
    val es = jm.entrySet()
    val it = es.iterator

    // chi square test
    val groups = 10
    val hits = new Array[Int](groups)
    def hit(hc: Int) {
      val bucket = math.abs(hc) / (Int.MaxValue / groups)
      hits(bucket) += 1
    }
    def expected = tests / groups
    def Dstat = {
      val diffs = for (i <- 0 until groups) yield math.abs(hits(i) - expected)
      diffs.sum.toDouble / expected
    }
    def ChiSquare = {
      val diffs = for (i <- 0 until groups) yield (hits(i) - expected) * (hits(i) - expected)
      diffs.sum.toDouble / expected
    }

    while (it.hasNext) {
      val x = it.next()
      hit(x.##)
    }
    // println(hits.toBuffer)
    // println(ChiSquare)
    assert(ChiSquare < 4.0, ChiSquare + " -> " + hits.mkString(", "))
  }

}
