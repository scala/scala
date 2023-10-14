class A {
  val c = new C()

  val t1 = new c.T1()
  c.t(new t1.A())

  val t2 = new c.T2()
  c.t(new t2.A())

  // https://github.com/scala/bug/issues/12606
  // val u = new c.U()
  // c.u(new c.U.A())
}
