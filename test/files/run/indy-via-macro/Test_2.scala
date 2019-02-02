object Test {
  def main(args: Array[String]): Unit = {
    assert(Macro.compilePattern("foo.bar").matcher("foo!bar").matches)
  }
}
