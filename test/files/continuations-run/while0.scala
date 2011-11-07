// $Id$

import scala.util.continuations._


object Test {
 
  def foo(): Int @cps[Unit] = 2
  
  def test(): Unit @cps[Unit] = {
    var x = 0
    while (x < 9000) { // pick number large enough to require tail-call opt
      x += foo()
    }
    println(x)
  }

  def main(args: Array[String]): Any = {
    reset(test())
  }
  
}