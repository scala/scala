object Test {
  def main(args: Array[String]): Unit = {
    one.foo // should not fail...
    one.module
  }
}
