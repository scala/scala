object Test {
  val source = 0 to 99
  val idxes = (-1 to 2) ++ (97 to 100)
  def str(xs: Seq[Int]) = xs.mkString("(", ", ", ")")
  
  def f(tgt: Seq[Int]) = {
    println("indexOfSlice")
    // the first index `>= from` such that...
    for (x <- idxes) {
      val res = source.indexOfSlice(tgt, x)
      println("  %s with idx >= %d = %d".format(str(tgt), x, res))
    }
    // the last index `<= end` such that...
    println("lastIndexOfSlice")
    for (x <- idxes) {
      val res = source.lastIndexOfSlice(tgt, x)
      println("  %s with idx <= %d = %d".format(str(tgt), x, res))
    }
  }
  
  def g(idx: Int, len: Int) = {
    f(source.slice(idx, idx + len))
  }
  
  def main(args: Array[String]): Unit = {
    g(97, 1)
    g(97, 2)
    g(97, 3)
    g(98, 2)
    g(99, 1)
  }
}
