object Test extends scala.tools.partest.StubErrorMessageTest {
  def codeA = """
    package stuberrors
    class A
  """

  def codeB = """
    package stuberrors
    class B[D <: A]
  """

  def userCode = """
    package stuberrors
    object C extends App {
      println(new B)
    }
  """

  def removeFromClasspath(): Unit = {
    removeClasses("stuberrors", List("A"))
  }
}
