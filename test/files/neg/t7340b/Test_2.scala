object C extends App {
  type Greeter[T] = {
    def hello: T
  }
  val g1: Greeter[_] = new B
  val g2: Greeter[Unit] = new B
}