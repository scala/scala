// bug #348
trait Foo {
  type bar <: this.Bar;
  abstract class Bar;
  case class Baz(r:bar) extends this.Bar;
  case object NoBar extends this.Bar;
}
object Test extends Application {
  object ConcreteFooBar extends Foo { // if moved to toplevel, it works
    type bar = Bar;
  }
  def foo = {
    import ConcreteFooBar._ ;
    Baz( NoBar )
  }
}

// bug #367

object Bla {
  def foo(): Unit = (return null).equals(null);
}

