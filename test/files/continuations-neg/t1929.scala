// $Id$

import scala.util.continuations._


object Test {
  def main(args : Array[String]) {
    reset {
      println("up")
      val x = shift((k:Int=>String) => k(8) + k(2))
      println("down " + x)
      val y = shift((k:Int=>String) => k(3))
      println("down2 " + y)
      y + x
  	}
  }
}