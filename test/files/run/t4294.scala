object Test {
  def main(args: Array[String]) {
    (Stream.from(1).collect{case x if x > 5000000 => x}: Stream[Int])
    
    assert((Stream from 1 take 10 collect { case x if x <= 3 => x*x }).sum == 14)
  }
}
