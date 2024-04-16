//> using options -Werror -Wperformance
object Test {
  var a, b = 0 // ok
  def mkStrangeCounter(): Int => Int = {
    var c = a // nok
    object _d { var d = b }; import _d._ // ok
    e => { c += a; d += b; a *= b; b -= c; c ^ d }
  }

  def f(): Unit = List(42).foreach(i => if (i > 27) return)
}
