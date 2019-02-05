trait T0 { def ap(): Int }
trait T1 { def ap(a: Any): Int }
trait T2 { def ap(a: Any, b: Any): Int }

class Test {
  def f0 = (() => 0): T1
  def f1 = ((x: Any) => 0): T2

  def f2 = ((x: Any) => 0): T0
  def f3 = ((x: Any) => 0): T2

  def f4 = ((x: Any, y: Any) => 0): T0
  def f5 = ((x: Any, y: Any) => 0): T1

  def f6 = ((x) => 0): T2

  def f7 = ((x) => 0): T0
  def f8 = ((x) => 0): T2

  def f9 = ((x, y) => 0): T0
  def g0 = ((x, y) => 0): T1
}
