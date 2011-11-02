// $Id$

import scala.util.continuations._

object Test {
  
  def foo = try {
    shift((k: Int=>Int) => k(7))
  } catch {
    case ex =>
      9
  }

  def bar = try {
    7
  } catch {
    case ex =>
    shiftUnit0[Int,Int](9)
  }
  
  def main(args: Array[String]): Unit = {
    println(reset { foo + 3 })
    println(reset { bar + 3 })
  }
}