object Test {
  def foo(o: Option[Int]): Int = {
    lazy val i: Int = {
      def local: Int = {if ("".isEmpty) return 42; -42}
      assert(local == 42)
      o.getOrElse(return -1)
    }
    i + 1
  }

  def main(args: Array[String]) {
    assert(foo(None) == -1)
  }
}
