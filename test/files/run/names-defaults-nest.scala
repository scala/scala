object Test {
  def multinest = {
    def baz = bar();
    def bar(x: String = "a"): Any = {
      def bar(x: String = "b") = x
      bar() + x
    };
    assert(baz == "ba", baz)
  }
  def main(args: Array[String]) {
    multinest
  }
}
