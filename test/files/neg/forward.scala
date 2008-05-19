object Test {
  def f: Int = x;
  val x: Int = f;

  {
    def f: Int = x;
    val x: Int = f;
  }
  {
    def f: Int = g;
    val x: Int = f;
    def g: Int = x;
  }
  {
    def f: Int = g;
    var x: Int = f;
    def g: Int = x;
  }
  {
    def f: Int = g;
    Console.println("foo");
    def g: Int = f;
  }
}
