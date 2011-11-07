object Test {
  // some maximally sized ranges
  val r1 = 0 until Int.MaxValue
  val r2 = 1 to Int.MaxValue
  val r3 = Int.MinValue to -2
  val r4 = Int.MinValue until -1
  
  // some exceptional conditions
  val e1 = () => (0 to Int.MaxValue).length
  val e2 = () => (5 until 5).last
  
  def main(args: Array[String]): Unit = {
    List(r1, r2, r3, r4) foreach (x => assert(x.length == Int.MaxValue))
    
    // exception required
    List(e1, e2) foreach { f =>
      try { f() ; assert(false) }
      catch { case _ => () }
    }
  }
}
