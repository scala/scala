// $Id$

import scala.util.continuations._


object Test {
 
  def test(x:Int) = if (x <= 7)
    shift { k: (Unit=>Unit) => println("abort") }
  
  def main(args: Array[String]): Any = {
    println(reset{ test(7); println("alive") })
    println(reset{ test(8); println("alive") })
  }
  
}