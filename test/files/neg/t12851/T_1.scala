
trait T1 {
  def f: Int
}
trait T2 extends T1 {
  def f() = 42
}
