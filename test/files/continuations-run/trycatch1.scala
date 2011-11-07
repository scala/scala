// $Id$

import scala.util.continuations._

object Test {

  def fatal: Int = throw new Exception()
  
  def foo1 = try {
    fatal
    shift((k: Int=>Int) => k(7))
  } catch {
    case ex =>
      9
  }

  def foo2 = try {
    shift((k: Int=>Int) => k(7))
    fatal
  } catch {
    case ex =>
      9
  }

  def bar1 = try {
    fatal
    7
  } catch {
    case ex =>
      shiftUnit0[Int,Int](9) // regular shift causes no-symbol doesn't have owner
  }

  def bar2 = try {
    7
    fatal
  } catch {
    case ex =>
      shiftUnit0[Int,Int](9) // regular shift causes no-symbol doesn't have owner
  }

  def main(args: Array[String]): Unit = {
    println(reset { foo1 + 3 })
    println(reset { foo2 + 3 })
    println(reset { bar1 + 3 })
    println(reset { bar2 + 3 })
  }

}