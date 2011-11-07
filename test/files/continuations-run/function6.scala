// $Id$

import scala.util.continuations._


object Test {
 
  def main(args: Array[String]): Any = {
    
    val g: PartialFunction[Int, Int @cps[Int]] = { case x => 7 }
    
    println(reset(g(2)))
    
  }
  
}