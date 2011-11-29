import scala.util.continuations._

object Test {
  def sh(x1:Int) = shift( (k: Int => Int) => k(k(k(x1))))
  
  def testA(x1: Int): Int @cps[Int] = {
      sh(x1)
      if (x1==42) x1 else sh(x1)
  }

  def testB(x1: Int): Int @cps[Int] = {
      if (sh(x1)==43) x1 else x1
  }
  
  def testC(x1: Int): Int @cps[Int] = {
      sh(x1)
      if (sh(x1)==44) x1 else x1
  }

  def testD(x1: Int): Int @cps[Int] = {
      sh(x1)
      if (sh(x1)==45) x1 else sh(x1)
  }

  def main(args: Array[String]): Any = {
    println(reset(1 + testA(7)))
    println(reset(1 + testB(9)))
    println(reset(1 + testC(9)))
    println(reset(1 + testD(7)))
  }
}