import a._
 
class C extends B {
  protected override def m() = super.m() + 1
  def emm = m()
}

object Test extends App {
  assert(new C().emm == 2)
}
