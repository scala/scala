//> using options -Werror

// variant of pos/t12304
// that does warn as desired

class Foo; class Bar
class Test {
  def t1: Unit = {
    val m = Map(1 -> new Foo)
    m.collect { case (_, bar: Bar) =>}
  }
}
