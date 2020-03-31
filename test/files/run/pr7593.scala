object Test {
  def main(args: Array[String]): Unit = {
    def foo = synchronized { "bar" }
    val bar = () => foo
    println(bar())
  }
}
