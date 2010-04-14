object Test {
  def main(args: Array[String]): Unit = {
    val t = new Throwable("DOOM")
    throw t
  }
}
