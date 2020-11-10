
object Test {
  object Bar
  def main(args: Array[String]): Unit = {
    assert(Bar.getClass == classOf[Bar.type] )
  }
}