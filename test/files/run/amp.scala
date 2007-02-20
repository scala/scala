object Test extends Application {

  def foo() = {
    def f: int = 1
    val x = &f
    x
  }

  def bar(g: => int) = {
    &g
  }

  Console.println((bar{ Console.println("g called"); 42 })())
  Console.println(foo()())

}



