object Test {
  def f: int = x;
  val x: int = f;

  {
    def f: int = x;
    val x: int = f;
  }
  {
    def f: int = g;
    val x: int = f;
    def g: int = x;
  }
  {
    def f: int = g;
    var x: int = f;
    def g: int = x;
  }
  {
    def f: int = g;
    Console.println("foo");
    def g: int = f;
  }
}
