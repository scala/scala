class M(val t: Int) extends AnyVal {
   def lazyString = {
      object X
      lazy val y = 1
      () => X
   }
}

class X1(val s: String) extends AnyVal {
  trait I2 {
   val q: String
   def z = s + q
  }

  def y(x: X1) = {
    val i2 = new I2 { val q = x.s }
    i2.z
  }
}

class X2(val s: String) extends AnyVal {
  private[this] class I2(val q: String)

  def y(i: Int) = {
    val i2 = new I2(i.toString)
    i2.q + s
  }
}
