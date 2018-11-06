trait T

class V1(val n: Long) extends AnyVal { self: T =>
  def foo: V1 = self
  def bar: T  = self
}

class V2(val n: Long) extends AnyVal { self =>
  def foo: V2 = self
}

class V3(val n: Long) extends AnyVal { self: Long =>
  def foo: V3   = self
  def bar: Long = self
}

// non-value classes

class C1(val n: Long) { self: T =>
  def foo: C1 = self
  def bar: T  = self
}

class C2(val n: Long) { self =>
  def foo: C2 = self
}

class C3(val n: Long) { self: Long =>
  def foo: C3   = self
  def bar: Long = self
}

object Test extends App {
  // Rejected: superclass V1 is not a subclass of the superclass Object of the mixin trait T
  // new V1(1l) with T

  assert(new V2(1L).foo.n == 1L)

  // Rejected: V3 does not conform to its self-type V3 with Long
  // new V3(1l)

  val c2 = new C1(2L) with T
  assert(c2.foo.n + c2.bar.asInstanceOf[C1].n == 4L)

  assert(new C2(3L).foo.n == 3L)

  // Rejected: C3 does not conform to its self-type C3 with Long
  // new C3(4l)

  // Rejected: class Long needs to be a trait to be mixed in
  // new C3(4l) with Long
}
