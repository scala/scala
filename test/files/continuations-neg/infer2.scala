// $Id$

import scala.util.continuations._


object Test {
 
  def test(x: => Int @cpsParam[String,Int]) = 7
  
  def sym() = shift { k: (Int => String) => 9 }
  
  
  def main(args: Array[String]): Any = {
    test { sym(); sym() }    
  }
  
}


