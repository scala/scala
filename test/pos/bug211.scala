trait A;
trait B;
class Foo: (A with B) extends A with B;
object Test with Executable {
  new Foo();
  System.out.println("bug211 completed");
}

