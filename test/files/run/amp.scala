object Test extends App {

  def foo() = {
    def f: Int = 1
    val x = () => f
    x
  }

  def bar(g: => Int) = {
    () => g
  }

  Console.println((bar{ Console.println("g called"); 42 })())
  Console.println(foo()())
}
