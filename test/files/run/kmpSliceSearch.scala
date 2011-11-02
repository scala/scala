object Test {
  import scala.collection.SeqLike
  def slowSearch[A](xs: Seq[A], ys: Seq[A], start: Int = 0): Int = {
    if (xs startsWith ys) start
    else if (xs.isEmpty) -1
    else slowSearch(xs.tail, ys, start+1)
  }
  def bkwSlowSearch[A](xs: Seq[A], ys: Seq[A]) = {
    val i = slowSearch(xs.reverse, ys.reverse)
    if (i<0) i
    else xs.length - ys.length - i
  }
  def main(args: Array[String]) {
    val rng = new scala.util.Random(java.lang.Integer.parseInt("kmp",36))
    
    // Make sure we agree with naive implementation
    for (h <- Array(2,5,1000)) {
      for (i <- 0 to 100) {
        for (j <- 0 to 10) {
          val xs = (0 to j).map(_ => (rng.nextInt & 0x7FFFFFFF) % h)
          val xsa = xs.toArray
          val xsv = Vector() ++ xs
          val xsl = xs.toList
          val xss = Vector[Seq[Int]](xs,xsa,xsv,xsl)
          for (k <- 0 to 5) {
            val ys = (0 to k).map(_ => (rng.nextInt & 0x7FFFFFFF) % h)
            val ysa = ys.toArray
            val ysv = Vector() ++ ys
            val ysl = ys.toList
            val yss = Vector[Seq[Int]](ys,ysa,ysv,ysl)
            val fwd_slow = slowSearch(xs,ys)
            val bkw_slow = bkwSlowSearch(xs,ys)
            val fwd_fast = xss.flatMap(xs => yss.map(ys => SeqLike.indexOf(xs,0,xs.length,ys,0,ys.length,0)))
            val bkw_fast = xss.flatMap(xs => yss.map(ys => SeqLike.lastIndexOf(xs,0,xs.length,ys,0,ys.length,xs.length)))
            assert(fwd_fast.forall(_ == fwd_slow))
            assert(bkw_fast.forall(_ == bkw_slow))
          }
        }
      }
    }
    
    // Check performance^Wcorrectness of common small test cases
    val haystacks = List[Seq[Int]](
      Array(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
      Vector(99,2,99,99,2,99,99,99,2,99,99,99,99,2),
      List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
      1 to 15
    )
    val needles = List[Seq[Int]](
      Array(7,8,9,10),
      Vector(99,99,99),
      List(1,1,1,1,1,2),
      5 to 9
    )
    (haystacks zip needles) foreach { 
      case (hay, nee) => 
        println(hay.indexOfSlice(nee,2) + " " + hay.lastIndexOfSlice(nee,13))
    }
  }
}
