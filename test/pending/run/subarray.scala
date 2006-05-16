object Test {
  def main(args: Array[String]): Unit = {
    val array = Array("one", "two", "three")
    val firstTwo: Array[String] = array.subArray(0,2)
    for(val x <- firstTwo)
      Console.println(x)
  }
}
