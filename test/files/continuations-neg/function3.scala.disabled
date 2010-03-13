// $Id$

import scala.util.continuations._


object Test {
 
  def main(args: Array[String]): Any = {
    
    val g: () => Int = () => shift { k: (Int=>Int) => k(7) }
    
    println(reset(g()))
  }
  
}