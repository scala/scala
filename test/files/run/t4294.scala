object Test {
  def main(args: Array[String]) {
    // Skip test on Avian, see SI-7600 for further information
    if (!scala.tools.partest.utils.Properties.isAvian)
      run()
  }

  def run(): Unit = {
    (Stream.from(1).collect{case x if x > 5000000 => x}: Stream[Int])
    assert((Stream from 1 take 10 collect { case x if x <= 3 => x*x }).sum == 14)
  }
}
