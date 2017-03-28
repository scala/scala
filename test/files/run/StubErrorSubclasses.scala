object Test extends scala.tools.partest.StubErrorMessageTest {
  def codeA = """
    package stuberrors
    class A
  """

  def codeB = """
    package stuberrors
    class B extends A
  """

  def userCode = """
    package stuberrors
    class C extends B
  """

  def removeFromClasspath(): Unit = {
    removeClasses("stuberrors", List("A"))
  }
}

