




// ticket #3502
object Test {
  
  object GeneratePrimeFactorsLazy extends (Int => List[Int]) {
    override def apply(n:Int) = {
      val s = Stream.range(2, n / 2).filter(n % _ == 0)
      //val s = for (i <- Stream.range(2, n / 2); if n % i == 0) yield i
      s.headOption.map(x => x :: apply(n / x)).getOrElse(List(n))
    }
  }
  
  def main(args:Array[String]) {
    // a prime number
    //val num = 623456789
    val num = 2796203
    assert(GeneratePrimeFactorsLazy(num) == List(num))
  }
  
}
