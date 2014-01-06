/* Test for SI-7344, where specialized methods inside the bodies of other
 * methods are not specialized, although they might as well be. The name
 * for the specialized method should not be different depending on the
 * outside method/class' specialization. */

class Test[@specialized(Int, Double) X](val x: X) {

  def checkSpecialization[Y](@specialized(Int, Double) y: Y): X = {

    // checking the specialization using the method name, which we can
    // extract from an exception's stack trace. We can match just the
    // prefix, since the compiler will add a suffix to the method name
    // during lambdalift, when it lifts the local methods outside.
    def specMe[@specialized(Int, Double) T, N](t: T, n: N): Unit = checkNameStartsWith(n.toString)

    // expected to specialize:
    specMe("x", "specMe")
    specMe(123, "specMe$mIc$sp")
    specMe(1.3, new { override def toString = "specMe$mDc$sp" })

    x
  }

  // name matching:
  private[this] def checkNameStartsWith(prefix: String): Unit = {
    val method = (new Exception).getStackTrace()(1).getMethodName()
    assert(method.startsWith(prefix), method + ".startsWith(" + prefix + ") should be true")
  }
}

object Test extends App {
  val t1 = new Test("x")
  val t2 = new Test(123)
  val t3 = new Test(1.3)

  // we want specialization to rewire these,
  // that's why they're not in a for loop:
  t1.checkSpecialization("x")

  // Prevented by SI-7579:
  //   The duplicator loses the @specialized annotation,
  //   so our tree transformation doesn't know it needs to
  //   specialize specMe inside the duplicated (and specialized)
  //   variants of the `checkSpecialization` method
  // t1.checkSpecialization(123)
  // t1.checkSpecialization(1.3)
  // t2.checkSpecialization("x")
  // t2.checkSpecialization(123)
  // t2.checkSpecialization(1.3)
  // t3.checkSpecialization("x")
  // t3.checkSpecialization(123)
  // t3.checkSpecialization(1.3)
}
