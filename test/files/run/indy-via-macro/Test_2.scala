object Test {
  def main(args: Array[String]) {
    assert(Macro.compilePattern("foo.bar").matcher("foo!bar").matches)
  }
}