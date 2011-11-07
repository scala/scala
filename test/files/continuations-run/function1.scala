// $Id$

import scala.util.continuations._


object Test {
 
  def main(args: Array[String]): Any = {
    
    val f = () => shift { k: (Int=>Int) => k(7) }
    val g: () => Int @cps[Int] = f
    
    println(reset(g()))
  }
  
}