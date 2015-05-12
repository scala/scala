class VC(val s: Int) extends AnyVal

class Client {
  def test = {
    val vc: VC = new Test().identity(new VC(42))
    assert(vc.s == 42)
  }
}
