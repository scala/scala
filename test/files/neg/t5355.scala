trait Test {
  type A = { def a(b: A): A }
  type B = { def a: B }
  type C = { def a(b: C): AnyRef }
  type D = { val a: D }
  val e: { def a: e.type }
}
