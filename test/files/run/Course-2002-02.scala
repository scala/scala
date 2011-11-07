//############################################################################
// Programmation IV - 2002 - Week 02
//############################################################################

object M0 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

  Console.println(gcd(14,21))
  Console.println(factorial(5))
  Console.println
}

//############################################################################

object M1 {
  def cube(x: Int): Double = x * x * x

  def sumInts(a: Int, b: Int): Double = if (a > b) 0
    else a + sumInts(a + 1, b);

  def sumCubes(a: Int, b: Int): Double = if (a > b) 0
    else cube(a) + sumCubes(a + 1, b);

  def sumReciprocals(a: Int, b: Int): Double = if (a > b) 0
    else 1.0/a + sumReciprocals(a + 1, b);

  def sumPi(n: Int): Double = {
    def element(x: Int): Double = 4.0/(4*x+1) - 4.0/(4*x-1);
    def sumElements(a: Int, b: Int): Double =
      if (a > b) 0
      else element(a) + sumElements(a + 1, b);
    4 + sumElements(1,n)
  }

  Console.println(sumInts(1,4))
  Console.println(sumCubes(1,4))
  Console.println(sumReciprocals(1,4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println
}

//############################################################################

object M2 {
  def id(x: Int): Double = x;
  def cube(x: Int): Double = x * x * x;
  def reciprocal(x: Int): Double = 1.0/x;

  def sum(f: Int => Double, a: Int, b: Int): Double =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b);

  def sumInts(a: Int, b: Int): Double = sum(id, a, b);
  def sumCubes(a: Int, b: Int): Double = sum(cube, a, b);
  def sumReciprocals(a: Int, b: Int): Double = sum(reciprocal, a, b);
  def sumPi(n: Int): Double = {
    def element(x: Int): Double = 4.0/(4*x+1) - 4.0/(4*x-1);
    4 + sum(element, 1, n)
  }

  Console.println(sumInts(1,4))
  Console.println(sumCubes(1,4))
  Console.println(sumReciprocals(1,4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println
}

//############################################################################

object M3 {
  def sum(f: Int => Double, a: Int, b: Int): Double =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b);

  def sumInts(a: Int, b: Int): Double = sum((xXXXXX => xXXXXX), a, b);
  def sumCubes(a: Int, b: Int): Double = sum((x => x * x * x), a, b);
  def sumReciprocals(a: Int, b: Int): Double = sum((x => 1.0/x), a, b);
  def sumPi(n: Int): Double = 4 + sum((x => 4.0/(4*x+1) - 4.0/(4*x-1)), 1, n);

  Console.println(sumInts(1,4))
  Console.println(sumCubes(1,4))
  Console.println(sumReciprocals(1,4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println
}

//############################################################################

object M4 {
  def sum(f: Int => Double): (Int, Int) => Double = {
    def sumF(a: Int, b: Int): Double =
      if (a > b) 0
      else f(a) + sumF(a + 1, b);
    sumF
  }

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  def sumReciprocals = sum(1.0/_)  
  def sumPi = { n: Int => 4 + sum(x => 4.0/(4*x+1) - 4.0/(4*x-1))(1, n) }

  Console.println(sumInts(1,4))
  Console.println(sumCubes(1,4))
  Console.println(sumReciprocals(1,4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println
}

//############################################################################

object M5 {
  def sum(f: Int => Double): (Int, Int) => Double = { (a, b) =>
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  def sumReciprocals = sum(x => 1.0/x)
  def sumPi = { n: Int => 4 + sum(x => 4.0/(4*x+1) - 4.0/(4*x-1))(1, n) }

  Console.println(sumInts(1,4))
  Console.println(sumCubes(1,4))
  Console.println(sumReciprocals(1,4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println
}

//############################################################################

object M6 {
  def sum(f: Int => Double)(a: Int, b: Int): Double =
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b);

  def sumInts = sum(x => x)_
  def sumCubes = sum(x => x * x * x)_
  def sumReciprocals = sum(x => 1.0/x)_
  def sumPi = { n: Int => 4 + sum(x => 4.0/(4*x+1) - 4.0/(4*x-1))(1, n) }

  Console.println(sumInts(1,4))
  Console.println(sumCubes(1,4))
  Console.println(sumReciprocals(1,4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println
}

//############################################################################

object M7 {
  def sum(f: Int => Double)(a: Int, b: Int): Double = {
    def iter(a: Int, result: Double): Double =
      if (a > b) result
      else iter(a + 1, f(a) + result);
    iter(a, 0)
  }

  def sumInts = sum(x => x)_
  def sumCubes = sum(x => x * x * x)_
  def sumReciprocals = sum(x => 1.0/x)_
  def sumPi = { n: Int => 4 + sum(x => 4.0/(4*x+1) - 4.0/(4*x-1))(1, n) }

  Console.println(sumInts(1,4))
  Console.println(sumCubes(1,4))
  Console.println(sumReciprocals(1,4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println
}

//############################################################################

object M8 {
  def product(f: Int => Double)(a: Int, step: Int, b: Int): Double =
    if (a > b) 1
    else f(a) * product(f)(a + step, step, b);

  def productPi = { n: Int => product(x=>4.0*x*x/(2*x-1)/(2*x-1))(1,1,n)/n }

  val pi = 2 * product(x => x * x)(2, 2, 40) / product(x => x * x)(1, 2,40)/40;

  Console.println("pi = " + productPi(20))
  Console.println("pi = " + pi)
  Console.println
}

//############################################################################

object M9 {
  def accumulate[t](combiner: (t, t) => t, nullValue: t, f: Int => t, 
                    next: Int => Int)(a: Int, b: Int): t =
    if (a > b) nullValue
    else combiner(f(a), accumulate(combiner, nullValue, f, next)(next(a), b))

  def inc(x: Int) = x + 1

  def sum(f: Int => Double): (Int, Int) => Double =
    accumulate((x: Double, y: Double) => x + y, 0d, f, inc)

  def product(f: Int => Double): (Int, Int) => Double =
    accumulate((x: Double, y: Double) => x * y, 1d, f, inc)

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  def sumReciprocals = sum(x => 1.0 / x)
  def sumPi = { n: Int => 4 + sum(x => 4.0/(4*x+1) - 4.0/(4*x-1))(1, n) }

  def productPi = { n: Int => product(x=>4.0*x*x/(2*x-1)/(2*x-1))(1,n)/n }

  val pi = 2*product(x => 2*x*2*x)(1,20)/product(x =>(2*x-1)*(2*x-1))(1,20)/40

  Console.println(sumInts(1, 4))
  Console.println(sumCubes(1, 4))
  Console.println(sumReciprocals(1, 4))
  Console.println(sumCubes(1, 10) + sumReciprocals(10, 20))
  Console.println("pi = " + sumPi(20))
  Console.println("pi = " + productPi(20))
  Console.println("pi = " + pi)
  Console.println
}

//############################################################################

object MA {
  val tolerance = 0.0001
  def abs(x: Double) = if (x < 0) -x else x
  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess);
      Console.println(next);
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)

  Console.println("sqrt(2) = " + sqrt(2))
  Console.println
}

//############################################################################

object MB {
  val tolerance = 0.0001;
  def abs(x: Double) = if (x < 0) -x else x;
  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < tolerance;
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess);
      Console.println(next);
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2;
  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0);

  Console.println("sqrt(2) = " + sqrt(2))
  Console.println
}

//############################################################################

object MC {
  def sum(f: Int => Double)(a: Int, b: Int): Double = {
    def iter(a: Int, result: Double): Double = {
      if (a > b) result
      else iter(a + 1, result + f(a))
    }
    iter(a, 0)
  }

  def product(f: Int => Double)(a: Int, b: Int): Double = {
    def iter(a: Int, result: Double): Double = {
      if (a > b) result
      else iter(a + 1, result * f(a))
    }
    iter(a, 1)
  }

  def factorial(n: Int) = product(x => x)(1 , n)

  Console.println(
      "1 + 2 + .. + 5 = " + sum(x => x)(1, 5));
  Console.println(
      "1 * 2 * .. * 5 = " + product(x => x)(1, 5));
  Console.println;

  Console.println(
      "1^2 + 2^2 + .. + 5^2 = " + sum(x => x*x)(1, 5));
  Console.println(
      "1^2 * 2^2 * .. * 5^2 = " + product(x => x*x)(1, 5));
  Console.println;

  Console.println(
      "factorial(0) = " + factorial(0))
  Console.println(
      "factorial(1) = " + factorial(1))
  Console.println(
      "factorial(2) = " + factorial(2))
  Console.println(
      "factorial(3) = " + factorial(3))
  Console.println(
      "factorial(4) = " + factorial(4))
  Console.println(
      "factorial(5) = " + factorial(5))
  Console.println
}

//############################################################################

object MD {
  def reduce(op: (Double,Double) => Double, zero:Double)(f: Int => Double)(a: Int,b: Int): Double = {
    def iter(a: Int, result: Double): Double = {
      if (a > b) result
      else iter(a + 1, op(result, f(a)))
    }
    iter(a, zero)
  }

  def plus (x:Double,y:Double) = x+y; 
  val sum: (Int => Double) => (Int, Int) => Double = reduce(plus , 0);
  def times(x:Double,y:Double) = x*y; 
  val product: (Int => Double) => (Int, Int) => Double = reduce(times, 1);

  def factorial(n: Int) = product(x => x)(1 , n)

  Console.println(
      "1 + 2 + .. + 5 = " + sum(x => x)(1, 5))
  Console.println(
      "1 * 2 * .. * 5 = " + product(x => x)(1, 5))
  Console.println;

  Console.println(
      "1^2 + 2^2 + .. + 5^2 = " + sum(x => x*x)(1, 5))
  Console.println(
      "1^2 * 2^2 * .. * 5^2 = " + product(x => x*x)(1, 5))
  Console.println;

  Console.println(
      "factorial(0) = " + factorial(0))
  Console.println(
      "factorial(1) = " + factorial(1))
  Console.println(
      "factorial(2) = " + factorial(2))
  Console.println(
      "factorial(3) = " + factorial(3))
  Console.println(
      "factorial(4) = " + factorial(4))
  Console.println(
      "factorial(5) = " + factorial(5))
  Console.println
}

//############################################################################

object ME {
  def reduce(op: (Double,Double) => Double, zero:Double)(f: Int => Double)(a: Int,b: Int): Double = {
    def iter(a: Int, result: Double): Double = {
      if (a > b) result
      else iter(a + 1, op(result, f(a)))
    }
    iter(a, zero)
  }

  def sum: (Int => Double) => (Int, Int) => Double     = reduce((x,y) => x + y, 0);
  def product: (Int => Double) => (Int, Int) => Double = reduce((x,y) => x * y, 1);

  def factorial(n: Int) = product(x => x)(1 , n)

  Console.println(
      "1 + 2 + .. + 5 = " + sum(x => x)(1, 5))
  Console.println(
      "1 * 2 * .. * 5 = " + product(x => x)(1, 5))
  Console.println;

  Console.println(
      "1^2 + 2^2 + .. + 5^2 = " + sum(x => x*x)(1, 5))
  Console.println(
      "1^2 * 2^2 * .. * 5^2 = " + product(x => x*x)(1, 5))
  Console.println;

  Console.println(
      "factorial(0) = " + factorial(0))
  Console.println(
      "factorial(1) = " + factorial(1))
  Console.println(
      "factorial(2) = " + factorial(2))
  Console.println(
      "factorial(3) = " + factorial(3))
  Console.println(
      "factorial(4) = " + factorial(4))
  Console.println(
      "factorial(5) = " + factorial(5))
  Console.println
}

//############################################################################

object MF {
  def fib(x: Int): Int =
    if (x <= 1) x
    else fib(x - 2) + fib(x - 1)

  Console.println("fib(0) = " + fib(0))
  Console.println("fib(1) = " + fib(1))
  Console.println("fib(2) = " + fib(2))
  Console.println("fib(3) = " + fib(3))
  Console.println("fib(4) = " + fib(4))
  Console.println("fib(5) = " + fib(5))
  Console.println("fib(6) = " + fib(6))
  Console.println("fib(7) = " + fib(7))
  Console.println("fib(8) = " + fib(8))
  Console.println("fib(9) = " + fib(9))
}

//############################################################################

object MG {
  def fib(x: Int) = {
    def loop(n: Int, prev: Int, fibn: Int): Int =
      if (n == x) fibn
      else loop(n + 1, fibn, fibn + prev)
    if (x == 0) 0 else loop(1, 0, 1)
  }

  Console.println("fib(0) = " + fib(0))
  Console.println("fib(1) = " + fib(1))
  Console.println("fib(2) = " + fib(2))
  Console.println("fib(3) = " + fib(3))
  Console.println("fib(4) = " + fib(4))
  Console.println("fib(5) = " + fib(5))
  Console.println("fib(6) = " + fib(6))
  Console.println("fib(7) = " + fib(7))
  Console.println("fib(8) = " + fib(8))
  Console.println("fib(9) = " + fib(9))
}

//############################################################################

object MH {
  def power(x: Double, y: Int): Double =
    if (y <= 0) 1
    else if (y % 2 == 0) power(x * x, y / 2)
    else x * power(x, y - 1);


  Console.println("power(0,0) = " + power(0,0))
  Console.println("power(0,1) = " + power(0,1))
  Console.println("power(0,2) = " + power(0,2))
  Console.println("power(0,3) = " + power(0,3))
  Console.println("power(0,4) = " + power(0,4))
  Console.println("power(0,5) = " + power(0,5))
  Console.println("power(0,6) = " + power(0,6))
  Console.println("power(0,7) = " + power(0,7))
  Console.println("power(0,8) = " + power(0,8))
  Console.println

  Console.println("power(1,0) = " + power(1,0))
  Console.println("power(1,1) = " + power(1,1))
  Console.println("power(1,2) = " + power(1,2))
  Console.println("power(1,3) = " + power(1,3))
  Console.println("power(1,4) = " + power(1,4))
  Console.println("power(1,5) = " + power(1,5))
  Console.println("power(1,6) = " + power(1,6))
  Console.println("power(1,7) = " + power(1,7))
  Console.println("power(1,8) = " + power(1,8))
  Console.println

  Console.println("power(2,0) = " + power(2,0))
  Console.println("power(2,1) = " + power(2,1))
  Console.println("power(2,2) = " + power(2,2))
  Console.println("power(2,3) = " + power(2,3))
  Console.println("power(2,4) = " + power(2,4))
  Console.println("power(2,5) = " + power(2,5))
  Console.println("power(2,6) = " + power(2,6))
  Console.println("power(2,7) = " + power(2,7))
  Console.println("power(2,8) = " + power(2,8))
  Console.println

  Console.println("power(3,0) = " + power(3,0))
  Console.println("power(3,1) = " + power(3,1))
  Console.println("power(3,2) = " + power(3,2))
  Console.println("power(3,3) = " + power(3,3))
  Console.println("power(3,4) = " + power(3,4))
  Console.println("power(3,5) = " + power(3,5))
  Console.println("power(3,6) = " + power(3,6))
  Console.println("power(3,7) = " + power(3,7))
  Console.println("power(3,8) = " + power(3,8))
  Console.println

  Console.println("power(4,0) = " + power(4,0))
  Console.println("power(4,1) = " + power(4,1))
  Console.println("power(4,2) = " + power(4,2))
  Console.println("power(4,3) = " + power(4,3))
  Console.println("power(4,4) = " + power(4,4))
  Console.println("power(4,5) = " + power(4,5))
  Console.println("power(4,6) = " + power(4,6))
  Console.println("power(4,7) = " + power(4,7))
  Console.println("power(4,8) = " + power(4,8))
  Console.println

  Console.println("power(5,0) = " + power(5,0))
  Console.println("power(5,1) = " + power(5,1))
  Console.println("power(5,2) = " + power(5,2))
  Console.println("power(5,3) = " + power(5,3))
  Console.println("power(5,4) = " + power(5,4))
  Console.println("power(5,5) = " + power(5,5))
  Console.println("power(5,6) = " + power(5,6))
  Console.println("power(5,7) = " + power(5,7))
  Console.println("power(5,8) = " + power(5,8))
  Console.println
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    M0
    M1
    M2
    M3
    M4
    M5
    M6
    M7
    M8
    M9
    MA
    MB
    MC
    MD
    ME
    MF
    MG
    MH
    ()
  }
}

//############################################################################
