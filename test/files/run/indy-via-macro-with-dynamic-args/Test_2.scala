object Test {
  def main(args: Array[String]): Unit = {
    val s = "foo!bar"
    assert(Macro.matcher("foo.bar", s).matches == true)
  }
}
