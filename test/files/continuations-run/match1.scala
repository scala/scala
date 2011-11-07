// $Id$

import scala.util.continuations._


object Test {
 
  def test(x:Int) = x match {
    case 7 => shift { k: (Int=>Int) => k(k(k(x))) }
    case _ => x
  }
  
  def main(args: Array[String]): Any = {
    println(reset(1 + test(7)))
    println(reset(1 + test(8)))
  }
  
}