object Test {
  def f() = {
    val lb = scala.collection.mutable.ListBuffer[Int](1, 2)
    val it = lb.iterator
    if (it.hasNext) it.next
    val xs = lb.toList
    lb += 3
    it.mkString
  }

  def main(args: Array[String]) {
    println(f())
  }
}
