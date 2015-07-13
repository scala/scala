trait Base {
  def t          = 1
  def t(n: Int)  = n
  def bt         = 2
  def bt(n: Int) = n
}
trait Derived extends Base {
  // was: double definition error
  override def t          = 1 + super.t
  override def t(n: Int)  = 1 + super.t(n)
  override def bt         = 1 + super.bt
  override def bt(n: Int) = 1 + super.bt(n)
}

object Test extends App {
  val d = new Derived {}
  // not the focus of this bug, but let's just check the runtime behaviour while we're here.
  assert(d.t == 2)
  assert(d.t(1) == 2)
  assert(d.bt == 3)
  assert(d.bt(1) == 2)
}
