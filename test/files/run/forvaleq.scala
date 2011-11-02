// test "foo = expr" clauses in for comprehensions

import scala.collection.immutable.Queue
import scala.{List=>L}
  
object Test {
  // redefine some symbols to make it extra hard
  class List
  class Tuple2
  def List[A](as: A*) = 5

  def firstDigit(x: Int): Int =
    x match {
    case 0 => 0
    case _ if (x<0) => firstDigit(-x)
    case _ if (x<10) => x
    case _ => firstDigit(x / 10)
  }
  
  
  {
    // a basic test case 
    
    val input = L.range(0,20)
    val oddFirstTimesTwo =
      for {x <- input
           xf = firstDigit(x)
           if xf % 2 == 1}
        yield x*2
    println(oddFirstTimesTwo)
  }

  {
    // a test case with patterns
    
    val input = L.range(0, 20)
    val oddFirstTimesTwo =
      for {x <- input
           xf = firstDigit(x)
           yf = x - firstDigit(x) / 10
           (a, b) = (xf - yf, xf + yf)
           if xf % 2 == 1}
        yield a + b
    println(oddFirstTimesTwo)
  }
  
  {
    // make sure it works on non-Ls
    
 //   val input: Queue = Queue.Empty[int].incl(L.range(0,20))
    val input = L.range(0, 20).iterator
    val oddFirstTimesTwo =
      for {x <- input
          xf = firstDigit(x)
          if xf % 2 == 1}
        yield x*2
    println(oddFirstTimesTwo.toList)    
  }
  
  {
    // yield the computed value
    
    val input = L.range(0,20)
    val oddFirstTimesTwo =
      for {x <- input
          xf = firstDigit(x)
          if xf % 2 == 1}
        yield xf*2
    println(oddFirstTimesTwo)    
  }

  {
    // make sure the function is only called once
    var count: Int = 0
    
    def fdct(x: Int) = {
      count += 1
      firstDigit(x)
    }
    
    val input = L.range(0,20)
    for {x <- input
         xf = fdct(x)
         if xf % 2 == 1}
      yield xf
      
    println("called " + count + " times")
  }

  def main(args: Array[String]) {}
}
