// scalac: -uniqid -Werror -Wunused
// hide: #\d+
class C {
  def f(x: Int) = ""
}
class D extends C {
  def f(x: String) = ""
}
final class Test {
  private def this(stuff: Int) = this()

  private class Subtle

  val d = new D
  def f = d.f
  def g(x: Int) = println("error")

  // -Vprint shows two symbols `y`
  def forgone(i: Int) = {
    def m = 17
    object j
    var totally = 27
    var unset: Int = 0
    val z = unset
    for (x <- Option(42); y <- Option(27) if x < i; res = x - y) yield res
  }

  private var misbegotten: Int = 42
  def touch() = misbegotten = 17
  private var forgotten: Int = 42
  def fetch   = forgotten
}
