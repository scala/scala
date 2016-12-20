object Test extends scala.tools.partest.StubErrorMessageTest {
  def codeA = """
    package stuberrors
    class A
    class NestedB[T]
  """

  def codeB = """
    package stuberrors
    class E extends A
    abstract class B {
      type D <: A
    }
  """

  def userCode = """
    package stuberrors
    class C {
      new B { type D = E }
    }
  """

  def removeFromClasspath(): Unit = {
    removeClasses("stuberrors", List("A"))
  }
}
