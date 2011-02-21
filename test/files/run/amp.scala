object Test extends App {

  def foo() = {
    def f: Int = 1
    val x = f _
    x
  }

  def bar(g: => Int) = {
    g _
  }

  Console.println((bar{ Console.println("g called"); 42 })())
  Console.println(foo()())
}
