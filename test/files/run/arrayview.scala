object Test {
  def f = (1 to 100).toArray.view
  
  def main(args: Array[String]): Unit = {
    val xs = (f filter (_ < 50)).reverse.filter(_ % 2 == 0).map(_ / 2).flatMap(x => Array(1, x))
    assert(xs.size == 48)
    val ys = xs.toArray
    assert(ys.size == 48)
    assert(xs.sum == ys.sum)
  }
}
