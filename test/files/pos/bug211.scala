trait A;
trait B;
class Foo extends A with B { self: A with B => }
object Test extends Application {
  new Foo();
  Console.println("bug211 completed");
}

