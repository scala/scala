import scala.tools.nsc._

object Test extends Application { 
  class Foo { override def toString = "Foo" };

  val int = new Interpreter(new Settings());
  int.bind("foo", "Test.Foo", new Test.Foo());
}
