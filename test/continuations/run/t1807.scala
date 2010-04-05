// $Id$

import scala.util.continuations._


object Test {
  def main(args: Array[String]): Unit = {
    val z = reset {
      val f: (() => Int @cps[Int]) = () => 1
      f()
    }
    println(z)
  }
}