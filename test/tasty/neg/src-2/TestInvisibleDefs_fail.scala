package tastytest

object TestInvisibleDefs {

  def foo: tastytest.argIsHello = ??? // has invisible flag so should not be seen
  def bar: tastytest.argIsHello = ??? // second try on same type

  def testBean = {
    val mybean = new InvisibleDefs.MyBean
    mybean.status = "open"
    mybean.getStatus() // error
    mybean.setStatus("closed") // error
  }

}
