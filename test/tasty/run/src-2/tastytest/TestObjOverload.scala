package tastytest

object TestObjOverload extends Suite("TestObjOverload") {
  test(assert(ObjOverload.foo(42) === 42))
}
