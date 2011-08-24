// bug #348

trait Foo {
  type bar <: Bar;
  abstract class Bar;
  case class Baz(r:bar) extends Bar;
  case object NoBar extends Bar;
}
object Test extends App {
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

