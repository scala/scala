import scala.language.implicitConversions

class Foo
class Bar(a: Int) { def this(a: String) = this(0) }

object Test {
  implicit def ok(f: Foo): Int = 0

  // Resolving the overloaded constructor checks each alternative with `inSilentMode`, which swaps in a buffering
  // reporter on the block's context. The search for a view from `Foo` completes `conv` at that point. The error
  // in its signature must still be reported, not dropped with the buffer.
  def m = {
    new Bar(new Foo)
    implicit def conv(f: Foo): NoSuchType = ???
    ()
  }
}
