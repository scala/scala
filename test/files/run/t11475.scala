// scalac: -Yunit-discard
object Test {
  def f(xs: Any*) = xs.size
  def g = Test f ()
  def main(args: Array[String]) = assert(g == 1)
}
