object Test extends scala.tools.partest.StubErrorMessageTest {
  def codeA = """
    package stuberrors
    class A
    class AA
  """

  def codeB = """
    package stuberrors

    class B {
      def foo[T <: A]: T = ???
    }

    class D extends A
  """

  def userCode = """
    package stuberrors

    abstract class C extends App {
      val b = new B

      // Use other symbols in the meanwhile
      val aa = new AA
      val dummy = 1
      println(dummy)

      // Should blow up
      b.foo[D]
    }
  """

  def removeFromClasspath(): Unit = {
    removeClasses("stuberrors", List("A"))
  }
}
