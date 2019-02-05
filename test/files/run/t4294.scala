object Test {
  def main(args: Array[String]): Unit = {
    // Skip test on Avian, see scala/bug#7600 for further information
    if (!scala.tools.partest.utils.Properties.isAvian)
      run()
  }

  def run(): Unit = {
    (LazyList.from(1).collect{case x if x > 5000000 => x}: LazyList[Int])
    assert((LazyList from 1 take 10 collect { case x if x <= 3 => x*x }).sum == 14)
  }
}
