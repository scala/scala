class VC(val s: String) extends AnyVal

class Client {
  def test = new Test().consume(new VC(""))
}
