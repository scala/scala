object Test {
  def main(args: Array[String]): Unit = {
    new { val x = "abc" } with AnyRef { }
  }
}
