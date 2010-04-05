object Test {
  def main(args: Array[String]) {
    import collection.mutable._
    val x4 = LinkedList[Int](1)
    println(x4)
    val y4 = LinkedList[Int](1)
    println(y4)
    println(x4 equals y4) // or (y4 equals x4)
  }
}
