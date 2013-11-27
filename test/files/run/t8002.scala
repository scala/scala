object Test extends App {
  val a: Any = {
    class A private () { private def x = 0; A.y };
    object A {
      def a = new A().x
      private def y = 0
    }
    A.a
  }
  def b: Any = {
    object A {
      def a = new A().x
      private def y = 0
    }
    class A private () { private def x = 0; A.y };
    A.a
  }
  b
}
