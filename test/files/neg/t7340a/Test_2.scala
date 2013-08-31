object C extends App {
  type Greeter = {
    def hello(): Unit
  }
  val g: Greeter = new B
  g.hello()
}