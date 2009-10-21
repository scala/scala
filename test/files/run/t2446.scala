object Test {
  def main(args : Array[String]) : Unit = {
    val arr = new Array[Int](10000)
    arr(5000) = 1
    arr (9000) = 2
    val sum = arr.reduceRight(_ + _)
    println(sum)
  }
}
