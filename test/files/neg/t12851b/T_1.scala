
trait T1 {
  def f: Int
  def g(): Int
  def v(): Int
}
trait T2 {
  def f() = 42
  def g = 42
  val v = 42
}
