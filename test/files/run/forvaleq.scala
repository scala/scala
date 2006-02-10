// test "val foo =" clauses in for comprehensions
// $Id$

import scala.collection.immutable.Queue
import scala.{List=>L}

object Test {
  // redefine some symbols to make it extra hard
  class List
  class Tuple2
  def List[A](as:A*) = 5

  def firstDigit(x:int): int =
    x match {
    case 0 => 0
    case _ if(x<0) => firstDigit(-x)
    case _ if(x<10) => x
    case _ => firstDigit(x / 10)
  }


  {
    // a basic test case

    val input = L.range(0,20)
    val oddFirstTimesTwo =
      for{val x <- input
          val xf = firstDigit(x)
          xf % 2 == 1}
        yield x*2
    Console.println(oddFirstTimesTwo)
  }

  {
    // a test case with patterns

    val input = L.range(0,20)
    val oddFirstTimesTwo =
      for{val x <- input
          val xf = firstDigit(x)
          val yf = x - firstDigit(x) / 10
          val Pair(a, b) = Pair(xf - yf, xf + yf)
          xf % 2 == 1}
        yield a + b
    Console.println(oddFirstTimesTwo)
  }

  {
    // make sure it works on non-Ls

 //   val input: Queue = Queue.Empty[int].incl(L.range(0,20))
    val input = L.range(0,20).elements
    val oddFirstTimesTwo =
      for{val x <- input
          val xf = firstDigit(x)
          xf % 2 == 1}
        yield x*2
    Console.println(oddFirstTimesTwo.toList)
  }

  {
    // yield the computed value

    val input = L.range(0,20)
    val oddFirstTimesTwo =
      for{val x <- input
          val xf = firstDigit(x)
          xf % 2 == 1}
        yield xf*2
    Console.println(oddFirstTimesTwo)
  }

  {
    // make sure the function is only called once
    var count: int = 0

    def fdct(x: int) = {
      count = count + 1
      firstDigit(x)
    }

    val input = L.range(0,20)
    for{val x <- input
        val xf = fdct(x)
        xf % 2 == 1}
      yield xf

    Console.println("called " + count + " times")
  }

  def main(args: Array[String]): Unit = ()
}
