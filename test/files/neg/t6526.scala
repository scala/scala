import scala.annotation.tailrec

class TailRec {
  def bar(f: => Any) = ""

  // transform the qualifier of a Select
  bar {
    @tailrec def inner(i: Int): Int = 1 + inner(i)
    inner(0)
  }.length

  // transform the body of a function
  () => {
    @tailrec def inner(i: Int): Int = 1 + inner(i)
    inner(0)
  }

  // transform the qualifier of a Select
  {
    @tailrec def inner(i: Int): Int = 1 + inner(i)
    inner(0)
    ""
  }.length

  // The receiver of a tail recursive call must itself be transformed
  object X {
    @tailrec // okay, all other annotated methods should fail.
    def foo: Any = {
      {
        @tailrec def inner(i: Int): Int = 1 + inner(i)
        inner(0)
        this
      }.foo
    }
  }

  Some(new AnyRef) map { phooie =>
    @tailrec
    def inner(i: Int): Int = 1 + inner(i)
  } getOrElse 42
}
