package examples;
import concurrent.ops._;
module futures {
  def someLengthyComputation = 1;
  def anotherLengthyComputation = 2;
  def f(x: Int) = x + x;
  def g(x: Int) = x * x;
  val x = future(someLengthyComputation);
  anotherLengthyComputation;
  val y = f(x()) + g(x());
  System.out.println(y);
}