import scala._;
object test2 {
  def f(x: Int): Int = 'a';
  def g(x: Int) = f(f(x));
}