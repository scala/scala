package tastytest

object TestRuntimeSpecialClasses extends Suite("TestRuntimeSpecialClasses") {
  test("consume standalone top-level object") {
    assert(new tastytest.Null$().res.head === 23)
    assert(new tastytest.Nothing$().res.head === 23)
    assert($.res.head === 23)
    assert(StandaloneObject.res.head === 23)
  }
}
