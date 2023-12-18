//> using options -Werror -Xlint

class C {
  private var (i, j) = init()

  private def init() = (42, 27)
  def f(): Unit = println(i+j)
  def g(): Unit = i += 1
}
class D {
  private var i, j = init()

  private def init() = 42
  def f(): Unit = println(i+j)
  def g(): Unit = i += 1
}
case class K(i: Int, j: Int)
class E {
  private var K(i, j) = init()

  private def init() = K(42, 27)
  def f(): Unit = println(i+j)
  def g(): Unit = i += 1
}
