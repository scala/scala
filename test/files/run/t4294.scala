



object Test {
  def main(args: Array[String]) {
    (Stream.from(1).collect{case x if x > 5000000 => x}: Stream[Int])
  }
}
