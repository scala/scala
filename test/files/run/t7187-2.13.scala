object Test {
  def foo(): () => String = () => ""
  val f: () => Any = foo
  def main(args: Array[String]): Unit = {
    assert(f() == "")
  }
}
