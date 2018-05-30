object Test {
  def main(args: Array[String]) {
    val x = "a"
    val result = (
      {
        val x = "b"
        "b" == _
      }
    )(x)
    println(result)
  }
}
