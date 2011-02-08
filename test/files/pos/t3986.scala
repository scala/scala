object Test {
  def main(args: Array[String]) {
    new { val x = "abc" } with AnyRef { }
  }
}
