// $Id$

import scala.util.continuations._


object Test {
 
  def foo(): Int @cps[Unit] = shift { k => println("up"); k(2); println("down") }
  
  def test(): Unit @cps[Unit] = {
    var x = 0
    while (x < 9) {
      x += foo()
    }
    println(x)
  }

  def main(args: Array[String]): Any = {
    reset(test())
  }
  
}