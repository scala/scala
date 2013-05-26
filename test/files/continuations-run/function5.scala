// $Id$

import scala.util.continuations._


object Test {
 
  def main(args: Array[String]): Unit = {
    
    val g: () => Int @cps[Int] = () => 7
    
    println(reset(g()))
  }
  
}
