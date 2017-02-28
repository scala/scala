
object X {
  @deprecated("just to annoy people", since="forever")
  def x = 42

  def f(i: String) = i
}

trait T {
  import X._

  def a = x

  def b = x

  def c = x

  def d = x

  def e = x

  def F = f("")

  def g = f("")

  def h = f("")

  def i = f("")

  def j = f("")
}

