import scala.collection.immutable._

object Test {

  def test(n: Int) = {
    var vb = new VectorBuilder[Int]
    for (i <- 0 until n)
      vb += i
    val v = vb.result
    assert(v == (0 until n), "not same as (0 until " + n + "): " + v)
  }

  def main(args: Array[String]): Unit = {
    for (i <- 0 until 2000)
      test(i)
  }
}
