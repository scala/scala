object Test {
  val nums = Stream.from(1)
  def isFizz(x: Int) = x % 3 == 0
  def isBuzz(x: Int) = x % 5 == 0
  // next line will run forever if withFilter isn't doing its thing.
  val fizzbuzzes = for (n <- nums ; if isFizz(n) ; if isBuzz(n)) yield n

  def main(args: Array[String]): Unit = {
    fizzbuzzes take 5 foreach println
  }
}
