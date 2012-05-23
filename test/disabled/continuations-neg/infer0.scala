// $Id$

import scala.util.continuations._


object Test {
 
  def test(x: => Int @cpsParam[String,Int]) = 7
  
  def main(args: Array[String]): Any = {
    test(8)
  }
  
}