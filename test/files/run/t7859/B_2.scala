class B private (private val b: Int) extends AnyVal
object B {
  val Const = new B(0)
}

// These tests will require erasure to unbox the value class.
// We need to test under joint and separate compilation to check
// that the 'notPRIVATE' flag on the param accessor is pickled.
//
// See also SI-6601.
object Test {
  def main(args: Array[String]) {
    unboxA
    unboxA1
    unboxA2
    unboxB
  }

  def unboxA {
    val o: Some[A] = Some(A.Const)
    val a = o.get
    def id(a: A): A = a
    id(a)
  }

  def unboxA1 {
    val o: Some[A1] = Some(new A1(0))
    val a = o.get
    def id(a: A1): A1 = a
    id(a)
  }

  def unboxA2 {
    import p.A2
    val o: Some[A2] = Some(new A2(0))
    val a = o.get
    def id(a: A2): A2 = a
    id(a)
  }

  def unboxB {
    val o: Some[B] = Some(B.Const)
    val b = o.get
    def id(b: B): B = b
    id(b)
  }
}
