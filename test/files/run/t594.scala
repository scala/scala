object Test {
  def main(args: Array[String]): Unit = {
    val array = Array("one", "two", "three")
    val firstTwo: Array[String] = array.slice(0,2)
    for (x <- firstTwo)
      Console.println(x)
  }
}
