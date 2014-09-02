class Tuple2Int(val encoding: Long) extends AnyVal with Product2[Int, Int] {
  def canEqual(that: Any) = false
  def _1: Int = 1
  def _2: Int = 2
}

object Test extends App {
  assert(new Tuple2Int(0)._1 == 1)
  assert(new Tuple2Int(0)._2 == 2)
}
