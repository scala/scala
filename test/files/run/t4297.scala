object Test {
  def main(args: Array[String]): Unit = {
    def f = List(1,2,3).view
    assert(f.toString == "View(?)")
    assert(f.mkString == "123")
  }
}
