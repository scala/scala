object Test {
  def main(args: Array[String]): Unit = {
    def foo = synchronized { "bar" }
    val eta = foo _
    println(eta())
  }
}
