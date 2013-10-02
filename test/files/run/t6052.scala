






object Test extends App {
  def seqarr(i: Int) = Array[Int]() ++ (0 until i)
  def pararr(i: Int) = seqarr(i).par

  def check[T](i: Int, f: Int => T) {
    val gseq = seqarr(i).toSeq.groupBy(f)
    val gpar = pararr(i).groupBy(f)
    assert(gseq == gpar, (gseq, gpar))
  }

  for (i <- 0 until 20) check(i, _ > 0)
  for (i <- 0 until 20) check(i, _ % 2)
  for (i <- 0 until 20) check(i, _ % 4)
}
