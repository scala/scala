object Test {
  class X(final var x: Int)  { }
  def f = new X(0).x += 1
  def main(args: Array[String]) {
    f
  }
}
