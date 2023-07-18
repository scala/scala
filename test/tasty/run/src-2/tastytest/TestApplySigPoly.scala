package tastytest

object TestApplySigPoly extends Suite("TestApplySigPoly") {

  test("construct class with APPLYsigpoly in parent") {
    val subbox = new ApplySigPoly.SubBox()
    assert(subbox.value == 23)
  }

  test("call method with APPLYsigpoly in annotation") {
    assert(ApplySigPoly.annotatedMethod == 23)
  }

}
