class M(val t: Int) extends AnyVal {
   def lazyString = {
      object X
      () => X
   }
}

class X1(val s: String) extends AnyVal {
  trait I2 {
   val q: String
   def z = s + q
  }

  def y(x: X1) = {
    val i2 = new I2 { val q = x.s } // allowed as of SI-7571
    i2.z

    { case x => x } : PartialFunction[Int, Int] // allowed
  }
}

class X2(val s: String) extends AnyVal {
  private[this] class I2(val q: String)

  def y(i: Int) = {
    val i2 = new I2(i.toString)
    i2.q + s
  }
}
