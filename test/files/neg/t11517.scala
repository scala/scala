
object X { def f(i: Int): Int = i; def f(s: String): String = s; def g(s: String) = s }

// improve error message
object Test extends App {
  X.f(_ + 1)

  // "abc".indexOf(_ + 1) unstable API across JDK versions
  trait MyString {
    def indexOf(c: Int): Int
    def indexOf(c: Int, begin: Int): Int
    def indexOf(c: Int, begin: Int, end: Int): Int
    def indexOf(s: String): Int
    def indexOf(s: String, begin: Int): Int
    def indexOf(s: String, begin: Int, end: Int): Int
  }
  def `quite overloaded`(s: MyString): Int = s.indexOf(_ + 1)

  X.f("hello, world", i = 42)
  X.f("hello, world", (i: Int) => i)
  X.f((i: Int, _) => i + 1)
  X.g(i => i)
  X.g((i: Int) => i)
}
/*
t11517.scala:6: error: missing parameter type for expanded function ((<x$1: error>) => x$1.$plus(1))
  X.f(_ + 1)
      ^
t11517.scala:8: error: missing parameter type for expanded function ((<x$2: error>) => x$2.$plus(1))
  "abc".indexOf(_ + 1)
                ^
*/
