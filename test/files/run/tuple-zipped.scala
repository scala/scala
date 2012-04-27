object Test {
  val xs1 = List.range(1, 100)
  val xs2 = xs1.view
  val xs3 = xs1 take 10
  val ss1 = Stream from 1
  val ss2 = ss1.view
  val ss3 = ss1 take 10
  val as1 = 1 to 100 toArray
  val as2 = as1.view
  val as3 = as1 take 10
  
  def xss1 = List[Seq[Int]](xs1, xs2, xs3, ss1, ss2, ss3, as1, as2, as3)
  def xss2 = List[Seq[Int]](xs1, xs2, xs3, ss3, as1, as2, as3)  // no infinities
  def xss3 = List[Seq[Int]](xs2, xs3, ss3, as1) // representative sampling
  
  def main(args: Array[String]): Unit = {
    for (cc1 <- xss1 ; cc2 <- xss2) {
      val sum1 = (cc1, cc2).zip map { case (x, y) => x + y } sum
      val sum2 = (cc1, cc2).zipped map (_ + _) sum
      
      assert(sum1 == sum2)
    }
    
    for (cc1 <- xss1 ; cc2 <- xss2 ; cc3 <- xss3) {
      val sum1 = (cc1, cc2, cc3).zip map { case (x, y, z) => x + y + z } sum
      val sum2 = (cc1, cc2, cc3).zipped map (_ + _ + _) sum
      
      assert(sum1 == sum2)
    }
    
    assert((ss1, ss1).zipped exists ((x, y) => true))
    assert((ss1, ss1, ss1).zipped exists ((x, y, z) => true))
    
    assert(!(ss1, ss2, 1 to 3).zipped.exists(_ + _ + _ > 100000))
    assert((1 to 3, ss1, ss2).zipped.forall(_ + _ + _ > 0))
    assert((ss1, 1 to 3, ss2).zipped.map(_ + _ + _).size == 3)
  }
}
  
