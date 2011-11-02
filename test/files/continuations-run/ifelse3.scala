// $Id$

import scala.util.continuations._


object Test {
 
  def util(x: Boolean) = shift { k: (Boolean=>Int) => k(x) }
 
  def test(x:Int) = if (util(x <= 7))
    x - 1
  else
    x + 1
    
  
  def main(args: Array[String]): Any = {
    println(reset(test(7)))
    println(reset(test(8)))
  }
  
}