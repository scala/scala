import scala.tools.nsc._

object Test extends Application {
  class Foo { override def toString = "Foo" };

  val Int = new Interpreter(new Settings());
  Int.bind("foo", "Test.Foo", new Test.Foo());
}
