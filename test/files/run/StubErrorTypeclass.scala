object Test extends scala.tools.partest.StubErrorMessageTest {
  def codeA = """
    package stuberrors
    class A[T]
  """

  def codeB = """
    package stuberrors
    class B[T: A](val t: T)
  """

  def userCode = """
    package stuberrors
    // Here we want a stub error not an implicit not found error
    class C { println(new B(1)) }
  """

  def removeFromClasspath(): Unit = {
    removeClasses("stuberrors", List("A"))
  }
}
