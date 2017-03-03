object Test {
  def main(args: Array[String]): Unit = {
    object O
    val x = O
    val y = O
    assert(x eq y)
  }
}
