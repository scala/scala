trait A;
trait B;
class Foo requires (A with B) extends A with B;
object Test extends Application {
  new Foo();
  System.out.println("bug211 completed");
}

