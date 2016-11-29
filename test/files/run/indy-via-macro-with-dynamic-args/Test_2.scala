object Test {
  def main(args: Array[String]) {
    val s = "foo!bar"
    assert(Macro.matcher("foo.bar", s).matches == true)
  }
}
