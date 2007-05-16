package examples

import concurrent.ops._

object futures {
  def someLengthyComputation = 1
  def anotherLengthyComputation = 2
  def f(x: Int) = x + x
  def g(x: Int) = x * x

  def main(args: Array[String]) {
    val x = future(someLengthyComputation)
    anotherLengthyComputation
    val y = f(x()) + g(x())
    println(y)
  }
}
