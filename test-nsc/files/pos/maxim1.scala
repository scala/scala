object test {
  def f(x: Int)(y: Int) = x + y;
  def y: Int => Int = f(2);
  def main = y(1);
}
