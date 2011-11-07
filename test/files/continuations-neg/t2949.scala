// $Id$

import scala.util.continuations._

object Test {

  def reflect[A,B](xs : List[A]) = shift{ xs.flatMap[B, List[B]] }
  def reify[A, B](x : A @cpsParam[List[A], B]) = reset{ List(x) }

  def main(args: Array[String]): Unit = println(reify {
    val x = reflect[Int, Int](List(1,2,3)) 
    val y = reflect[Int, Int](List(2,4,8))
    x * y
  })
}
