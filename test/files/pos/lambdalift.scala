import scala._;

module test {

  def f(x: Int) = {
    def g() = h();
    def h() = x;
    g();
    class inner() {
      def g() = h();
      def h() = x;
    }
    g() + new inner().g();
  }
}