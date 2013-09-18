// $Id$

import scala.util.continuations._


object Test {

  def test(x:Int) = if (x <= 7)
    shift { k: (Int=>Int) => k(k(k(x))) }
  else
    shift { k: (Int=>Int) => k(x) }

  def main(args: Array[String]): Unit = {
    println(reset(1 + test(7)))
    println(reset(1 + test(8)))
  }

}
