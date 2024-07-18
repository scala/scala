//> using options -Werror

class Foo
class Test {
  def t1: Unit = {
    val m: Map[String, Map[String, Foo]] = Map("outer" -> Map("inner" -> new Foo))
    m.collect { case (_, foo: Foo) => "This should be type error" }
    // no:
    // class Foo isn't final
    // Map is an unsealed trait
    // so it's possible to define:
    //   class Bar extends Foo with Map[...]
    // so the compiler is right not to warn
  }
}
