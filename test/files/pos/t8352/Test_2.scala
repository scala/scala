object Test extends App {
  def expectUnit() {
    Macros.foo(true)
  }
}