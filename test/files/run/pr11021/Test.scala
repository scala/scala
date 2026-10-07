object Test {
  @annotation.varargs def va(s: String*) =
    if (s == null) 0
    else 1

  def main(args: Array[String]) = {
    val arr: Array[String] = null
    val seq: Seq[String] = null

    assert(va(arr: _*) == 0): @annotation.nowarn("cat=deprecation")
    assert(va(seq: _*) == 0)
    assert(J.m() == 0)
  }
}
