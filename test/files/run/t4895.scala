object Test {
  
  def checkPar(sz: Int) {
    import collection._
    val hs = mutable.HashSet[Int]() ++ (1 to sz)
    assert(hs.par.map(_ + 1).seq.toSeq.sorted == (2 to (sz + 1)))
  }
  
  def main(args: Array[String]) {
    for (i <- 0 until 100) checkPar(i)
    for (i <- 100 until 1000 by 50) checkPar(i)
    for (i <- 1000 until 10000 by 500) checkPar(i)
    for (i <- 10000 until 100000 by 5000) checkPar(i)
  }
  
}
