import scala._;
module test2 {
  def f(x: Int): Int = 'a';
  def g(x: Int) = f(f(x));
}