object Test {
  @annotation.tailrec
  def bar(i : Int) : Int = {
    if (i == 0) 0
    else try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar(i - 1)
    }
  }

  @annotation.tailrec
  def nestedTry1(i : Int) : Int = {
    if (i == 0) 0
    else try {
      throw new RuntimeException
    } catch {
      case _: Throwable =>
        try { ??? } catch { case _: Throwable => nestedTry1(i - 1) }
    }
  }

  def main(args: Array[String]) {
    assert(bar(2) == 0)

    assert(nestedTry1(2) == 0)
  }
}
