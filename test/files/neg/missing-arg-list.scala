
trait T {

  def id(i: Int) = i
  def f(i: Int)(j: Int) = i+j
  def g(i: Int, j: Int, k: Int) = i+j+k
  def h(i: Int, j: Int, k: Int)(implicit s: String) = s*(i+j+k)

  val w = id
  val x = f
  val y = g
  val z = h

  def +(i: Int) = i + 42
  val p = +
}
