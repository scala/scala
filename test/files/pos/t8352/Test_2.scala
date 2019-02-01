object Test extends App {
  def expectUnit(): Unit = {
    Macros.foo(true)
  }
}
