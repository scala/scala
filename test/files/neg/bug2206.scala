object o {
  class A

  class AX {
      def f() { }
  }

  import Implicits._
  val a = new A
  a.f()

  object Implicits {
      implicit def ax(a: A) = new AX
  }
}