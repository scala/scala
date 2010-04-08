object Test {
  def main(args: Array[String]): Unit = {
    val it = List(1).iterator ++ { println("Hello"); Iterator.empty }
    println(it.next)
    it.hasNext
    it.hasNext
    it.hasNext
  }
}
