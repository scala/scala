object Test {
  def main(args: Array[String]): Unit = {
    val x =
      try { ("": Any) match { case List(_*) => true case x => throw new MatchError(x) } }
      catch { case _: Throwable => false }

    assert(!x)
  }
}
