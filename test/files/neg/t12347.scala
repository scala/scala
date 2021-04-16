
object X {
  def f(n: Int, s: String) = s * n
}

object Y {
  def f(n: Int, s: String) = s * n
  def f(s: String) = s * 3
}

object Test extends App {
  def count = 2
  def text  = "hi"
  X.f(n = count, x = text)
  Y.f(n = count, x = text)
}
