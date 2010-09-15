object Test {
  def main(args: Array[String]): Unit = {
    val s = (new StringBuilder ++= "hello world") dropRight 1 toString;
    assert(s == "hello worl")
  }
}

