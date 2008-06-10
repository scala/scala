object Test {
  def abs(x: Int): Int = synchronized {
    if (x > 0)
      return x
    return -x
  }

  def main(args: Array[String]) = {
    Console.println("abs(-5) = " + abs(-5))
  }
}
