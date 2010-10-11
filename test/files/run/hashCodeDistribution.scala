// See ticket #2537.
object Test {
  case class C(x: Int, y: Int) { }
  val COUNT = 300
  val totalCodes = COUNT * COUNT

  def main (args: Array[String]) = {
    val hashCodes =
      for (x <- 0 until COUNT; y <- 0 until COUNT) yield C(x,y).hashCode

    val uniques = hashCodes.distinct
    val collisionRate = (totalCodes - uniques.size) * 1000 / totalCodes

    assert(collisionRate < 5, "Collision rate too high: %d / 1000".format(collisionRate))
    // println("collisionRate = %d / 1000".format(collisionRate))
  }
}