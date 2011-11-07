

object Test {
  def main(args: Array[String]) {
    val a = new Array[Int](10)
    val b = new Array[Any](10)
    for (i <- 0 until 10) b(i) = i
    
    Array.copy(b, 3, a, 3, 7)
    assert(a.toSeq == List(0, 0, 0, 3, 4, 5, 6, 7, 8, 9))
  }
}



















