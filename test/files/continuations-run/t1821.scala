// $Id$

import scala.util.continuations._


object Test {
  def suspended[A](x: A): A @suspendable = x
  def test1[A](x: A): A @suspendable = suspended(x) match { case x => x }
  def test2[A](x: List[A]): A @suspendable = suspended(x) match { case List(x) => x }

  def test3[A](x: A): A @suspendable = x match { case x => x }
  def test4[A](x: List[A]): A @suspendable = x match { case List(x) => x }

  def main(args: Array[String]) = {
    println(reset(test1()))
    println(reset(test2(List(()))))
    println(reset(test3()))
    println(reset(test4(List(()))))
  }
}