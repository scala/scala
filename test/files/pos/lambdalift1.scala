import scala._;

object test {

  def f[a <: java.lang.Object](x: a) = {
    def print() = java.lang.System.out.println(x);
    class A() {
      def g() = {
	class B() {
	  def h() = print()
	}
	new B().h()
      }
    }
    new A().g()
  }
}
