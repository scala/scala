object Test extends scala.tools.partest.StubErrorMessageTest {
  def codeA = """
    package stuberrors
    class A
    class AA
  """

  def codeB = """
    package stuberrors

    class B {
      def bar: String = ???
      def foo: A = new A
      def baz: String = ???
    }
  """

  def userCode = """
    package stuberrors

    abstract class C extends App {
      val b = new B {}

      // Use other symbols in the meanwhile
      val aa = new AA
      val dummy = 1
      println(dummy)

      // Should blow up
      b.foo
    }
  """

  def removeFromClasspath(): Unit = {
    removeClasses("stuberrors", List("A"))
  }
}
