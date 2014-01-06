object Test {
  def main(args: Array[String]): Unit = {
    val x =
      try { ("": Any) match { case List(_*) => true } }
      catch { case _: Throwable => false }

    assert(!x)
  }
}
