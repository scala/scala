package tastytest

import Extensions._

/**
 * Suspended until extension methods on anyvals are supported
 */
object TestExtensions {

  def test1 = assert(().hello === "Hello")

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}
