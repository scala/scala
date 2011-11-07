// $Id$

import scala.util.continuations._


object Test {

  def m0() = {
    shift((k:Int => Int) => k(k(7))) * 2
  }

  def m1() = {
    2 * shift((k:Int => Int) => k(k(7)))
  }

  def main(args: Array[String]) = {
    
    println(reset(m0()))
    println(reset(m1()))
    
  }
  
}
