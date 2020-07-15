package tastytest

class Defaults(val i: Int = 33, val s: String = "foo")(val b: Boolean = false) {
  def foo(a: Int = 0)(b: String = "")(c: Boolean = false) = (a,b,c)
}

object Defaults {
  class OutOfOrder extends Defaults(s = {println(25); "abc"}, i = 12)(true) {
    def bar(d: Long = i.toLong) = (d,s,b)
  }
}
