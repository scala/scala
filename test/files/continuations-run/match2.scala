// $Id$

import scala.util.continuations._


object Test {
 
  def test1() = {
    val (a, b) = shift { k: (((String,String)) => String) => k("A","B") }
    b
  }

  case class Elem[T,U](a: T, b: U)
  
  def test2() = {
    val Elem(a,b) = shift { k: (Elem[String,String] => String) => k(Elem("A","B")) }
    b
  }

  
  def main(args: Array[String]): Any = {
    println(reset(test1()))
    println(reset(test2()))
  }
  
}