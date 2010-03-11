// $Id$

import scala.util.continuations._



object Test {

   def double[B](n : Int)(k : Int => B) : B = k(n * 2)

   def main(args : Array[String]) {
      reset {
        val result1 = shift(double[Unit](100))
        val result2 = shift(double[Unit](result1))
        println(result2)
      }
   }
}