// $Id$

import scala.util.continuations._


object Test {
 
  def foo1(): Int @cps[Unit] = 2
  def foo2(): Int @cps[Unit] = shift { k => println("up"); k(2); println("down") }
  
  def test(): Unit @cps[Unit] = {
    var x = 0
    while (x < 9000) { // pick number large enough to require tail-call opt
      x += (if (x % 1000 != 0) foo1() else foo2())
    }
    println(x)
  }

  def main(args: Array[String]): Any = {
    reset(test())
  }
  
}