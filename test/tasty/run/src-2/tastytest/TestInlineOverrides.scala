package tastytest

object TestInlineOverrides extends Suite("TestInlineOverrides") {
  import InlineOverrides._

  val b = new B()
  val a: A = b

  test(assert(a.f(22) === "inline 22"))

}
