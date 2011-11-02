object Test {
  def f = {
    val buf = new collection.mutable.ListBuffer[Int]
    buf ++= List(1, 2, 3)
    val l = buf.toList
    buf prependToList List(4, 5, 6)
    l
  }
  
  def main(args: Array[String]): Unit = {
    println(f)
  }
}
