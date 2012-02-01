// /scala/trac/5429/a.scala
// Wed Feb  1 08:05:27 PST 2012

class A {
  val value = 0
  lazy val lazyvalue = 2
  def nullary = 5
  def emptyArg() = 10
  def oneArg(x: String) = 15
}
class A0 {
  val value: Any = 0
  lazy val lazyvalue: Any = 2
  def nullary: Any = 5
  def emptyArg(): Any = 10
  def oneArg(x: String): Any = 15
}

class B extends A {
  object value      // fail
  object lazyvalue  // fail
  object nullary    // fail
  object emptyArg   // fail
  object oneArg     // overload
}
class B0 extends A0 {
  object value      // fail
  object lazyvalue  // fail
  object nullary    // fail
  object emptyArg   // fail
  object oneArg     // overload
}

class C extends A {
  override object value     // fail
  override object lazyvalue // fail
  override object nullary   // fail
  override object emptyArg  // fail
  override object oneArg    // fail
}
class C0 extends A0 {
  override object value     // !!! this succeeds, but should fail (lazy over strict)
  override object lazyvalue // !!! this fails, but should succeed (lazy over lazy)
  override object nullary   // override
  override object emptyArg  // override
  override object oneArg    // fail
}

class D extends A {
  val value = 0       // fail
  val lazyvalue = 0   // fail
  val nullary = 5     // fail
  val emptyArg = 10   // fail
  val oneArg = 15     // overload
}
class D0 extends A0 {
  override val value = 0      // override
  override val lazyvalue = 0  // fail (non-lazy)
  override val nullary = 5    // override
  override val emptyArg = 10  // override
  override val oneArg = 15    // fail
}

class E extends A {
  def value = 0       // fail
  def lazyvalue = 2   // fail
  def nullary = 5     // fail
  def emptyArg = 10   // fail
  def oneArg = 15     // overload
}
class E0 extends A0 {
  override def value = 0      // fail
  override def lazyvalue = 2  // fail
  override def nullary = 5    // override
  override def emptyArg = 10  // override
  override def oneArg = 15    // fail
}

class F extends A {
  lazy val value = 0       // fail
  lazy val lazyvalue = 2   // fail
  lazy val nullary = 5     // fail
  lazy val emptyArg = 10   // fail
  lazy val oneArg = 15     // overload
}
class F0 extends A0 {
  override lazy val value = 0      // fail (strict over lazy)
  override lazy val lazyvalue = 2  // override (lazy over lazy)
  override lazy val nullary = 5    // override
  override lazy val emptyArg = 10  // override
  override lazy val oneArg = 15    // fail
}

