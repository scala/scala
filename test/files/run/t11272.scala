
object Test {
  def main(args: Array[String]): Unit = {
    test()
  }
  def test() = {
    val iter = Iterator(128*1024*1024, 128*1024*1024).flatMap(new Array[Byte](_))
    while (iter.hasNext) {
      iter.next()
    }
  }
}
