object Test {
  def mkIterator = (1 to 5).iterator map (x => { println(x) ; x })
  def mkInfinite = Iterator continually { println(1) ; 1 }
  
  def main(args: Array[String]): Unit = {
    // Stream is strict in its head so we should see 1 from each of them.
    val s1 = mkIterator.toStream
    val s2 = mkInfinite.toStream
    // back and forth without slipping into nontermination.
    println((Stream from 1).toIterator.drop(10).toStream.drop(10).toIterator.next)
    ()
  }
}
