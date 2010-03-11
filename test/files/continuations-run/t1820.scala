// $Id$

import scala.util.continuations._


object Test {
  def shifted: Unit @suspendable = shift { (k: Unit => Unit) => () }
  def test1(b: => Boolean) = {
    reset {
      if (b) shifted
    }
  }
  def main(args: Array[String]) = test1(true)
}