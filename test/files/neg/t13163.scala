//> using options -Xsource:3 -Xsource-features:case-apply-copy-access

class O {
  case class I1 protected (x: Int)
  case class I2 protected[O] (x: Int)

  def t1 = I1.apply(42) // error
  def t2 = I2.apply(42) // ok
}

class OO extends O {
  class II1 extends I1(42) {
    def t1 = new I1(42)   // error
    def t2 = I1.apply(42) // error
  }

  def t1 = new I1(42) // error
  def t2 = new I2(42) // error
}
