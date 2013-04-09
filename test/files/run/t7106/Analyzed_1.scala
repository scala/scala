
abstract class Base0 { def p2: Int }
class Base(p1: Int, override val p2: Int) extends Base0

abstract class Sub1(q1: Int, q2: Int, q3: Int) extends Base(q1, q2) {
  def bippy1 = q1
  def bippy2 = q2
  def bippy3 = q3
}
abstract class Sub2(q1: Int, q2: Int, q3: Int) extends Base(q1, q2) {
  def bippy1 = q1
  def bippy2 = p2
  def bippy3 = q3
}
