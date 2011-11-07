// $Id$

import scala.util.continuations._

object Test {

  def fatal[T]: T = throw new Exception
  def cpsIntStringInt = shift { k:(Int=>String) => k(3); 7 }
  def cpsIntIntString = shift { k:(Int=>Int) => k(3); "7" }
  
  def foo1 = try {
    fatal[Int]
    cpsIntStringInt
  } catch {
    case ex =>
      cpsIntStringInt
  }

  def foo2 = try {
    fatal[Int]
    cpsIntStringInt
  } catch {
    case ex =>
      cpsIntStringInt
  }


  def main(args: Array[String]): Unit = {
    println(reset { foo1; "3" })
    println(reset { foo2; "3" })
  }

}