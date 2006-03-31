object Test {
  class Foo(x: Foo) {
    def this() = this(this);
  }
  def main(args: Array[String]): Unit = {
    new Foo();
  }
}
