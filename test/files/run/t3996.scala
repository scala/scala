




// should not result in a stack overflow
object Test {
  def main(args: Array[String]) {
    import collection.mutable.LinkedList
    val l = new LinkedList[Int]() ++ (0 until 10000)
    assert(l.length == 10000)
  }
}
