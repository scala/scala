trait A;
trait B;
class Foo extends A with B { self: A with B => }
object Test extends App {
  new Foo();
  Console.println("bug211 completed");
}

