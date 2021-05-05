package tastytest

object TestInvisibleDefs extends Suite("TestInvisibleDefs") {

  test("invoke '@main def argIsHello'") {
    InvisibleDefs.argIsHello("Hello")
  }

  test("update bean.status") {
    val mybean = new InvisibleDefs.MyBean
    mybean.status = "open"
    assert(mybean.status === "open")
  }

}
