// $Id$

import scala.util.continuations._


object Test {
 
  def test1(x:Int) = if (x <= 7)
    shift { k: (Int=>Int) => k(k(k(x))) }
  else
    x
  
  def test2(x:Int) = if (x <= 7)
    x
  else
    shift { k: (Int=>Int) => k(k(k(x))) }

  def main(args: Array[String]): Any = {
    println(reset(1 + test1(7)))
    println(reset(1 + test1(8)))
    println(reset(1 + test2(7)))
    println(reset(1 + test2(8)))
  }
  
}