class Wrap(val x: Int) extends AnyVal {
  def ***(other: Bip): Wrap = new Wrap(x * other.x)
}
class Bip(val x: Int) extends AnyVal

class SameBytecode {
  def a1(x: Int, y: Int): Int = x + y
  def a2(x: Wrap, y: Wrap): Wrap = new Wrap(x.x + y.x)
  def a3(x: Int, y: Wrap): Wrap = new Wrap(x + y.x)
  def a4(x: Int, y: Wrap): Int = x + y.x

  def b1(x: Wrap, y: Int): Int = (x *** new Bip(y)).x
  def b2(x: Wrap, y: Bip): Wrap = x *** y
  def b3(x: Wrap, y: Int): Wrap = x *** new Bip(y)
  def b4(x: Wrap, y: Bip): Bip = new Bip((x *** y).x)
  def b5(x: Wrap, y: Int): Bip = new Bip((x *** new Bip(y)).x)
}
