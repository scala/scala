//############################################################################
// Programmation IV - 2002 - Week 01
//############################################################################

object M0 {

  //##########################################################################

  Console.println(87 + 145);
  Console.println(1000 - 333);
  Console.println(5 + 2 * 3);

  //##########################################################################

  def size = 2;
  def pi = 3.14159;
  def radius = 10;
  def circumference = 2 * pi * radius;

  Console.println(5 * size);
  Console.println(2 * pi * radius);
  Console.println(circumference);
  Console.println((2 * pi) * radius);

  //##########################################################################

  def square(x: Double) = x * x;

  Console.println(square(2));
  Console.println(square(5 + 4));
  Console.println(square(square(4)));

  //##########################################################################

  def sumOfSquares(x: Double, y: Double) = square(x) + square(y);

  Console.println(sumOfSquares(3, 2+2));

  //##########################################################################

  def loop: Int = loop;
  def first(x: Int, y: Int) = x;
  def constOne(x: Int, y: => Int) = 1;

  Console.println(constOne(1, loop));

  //##########################################################################

  def abs(x: Double) = if (x >= 0) x else -x;

  Console.println(abs(737));
  Console.println(abs(1));
  Console.println(abs(0));
  Console.println(abs(-1));
  Console.println(abs(-76));

  //##########################################################################

  def sqrtIter0(guess: Double, x: Double): Double =
    if (isGoodEnough0(guess, x)) guess
    else sqrtIter0(improve0(guess, x), x);

  def improve0(guess: Double, x: Double) =
    (guess + x / guess) / 2;

  def isGoodEnough0(guess: Double, x: Double) =
    abs(square(guess) - x) < 0.001;

  def sqrt0(x: Double) = sqrtIter0(1.0, x);

  Console.println(sqrt0(2));
  Console.println(sqrt0(3));
  Console.println(sqrt0(4));

  //##########################################################################

  def sqrt1(x: Double) = {
    def sqrtIter1(guess: Double, x: Double): Double =
      if (isGoodEnough1(guess, x)) guess
      else sqrtIter1(improve1(guess, x), x);

    def improve1(guess: Double, x: Double) =
      (guess + x / guess) / 2;

    def isGoodEnough1(guess: Double, x: Double) =
      abs(square(guess) - x) < 0.001;

    sqrtIter1(1.0, x)
  }

  Console.println(sqrt1(2));
  Console.println(sqrt1(3));
  Console.println(sqrt1(4));

  //##########################################################################

  def sqrt2(x: Double) = {
    def sqrtIter2(guess: Double): Double =
      if (isGoodEnough2(guess)) guess
      else sqrtIter2(improve2(guess));

    def improve2(guess: Double) =
      (guess + x / guess) / 2;

    def isGoodEnough2(guess: Double) =
      abs(square(guess) - x) < 0.001;

    sqrtIter2(1.0)
  }

  Console.println(sqrt2(2));
  Console.println(sqrt2(3));
  Console.println(sqrt2(4));

  //##########################################################################
}

//############################################################################

object M1 {
  def abs(x: Double) = if (x >= 0) x else -x;

  def sqrt(x: Double): Double = {
    def sqrtIter(prev: Double, guess: Double): Double =
      if (isGoodEnough(prev, guess)) guess
      else sqrtIter(guess, improve(guess));

    def improve(guess: Double) = (guess + x / guess) / 2;

    def isGoodEnough(prev: Double, guess: Double) =
      abs(prev - guess) / guess < 0.001;

    sqrtIter(1.0, improve(1.0))
  }

  Console.println("sqrt(2) = " + sqrt(2));
}

//############################################################################

object M2 {
  def abs(x: Double) = if (x >= 0) x else -x;

  def sqrt(x:Double):Double = {
    def sqrtIter(guess:Double):Double = {
      val next = improve(guess);
      if (isGoodEnough(guess,next)) next
      else sqrtIter(next)
    }

    def improve(guess:Double) = (guess+x/guess)/2;

    def isGoodEnough(prev:Double,guess:Double) = abs(prev-guess)/guess<0.001;

    sqrtIter(1.0)
  }

  Console.println("sqrt(2) = " + sqrt(2));
}

//############################################################################

object M3 {
  def abs(x: Double) = if (x >= 0) x else -x;

  def cbrt(x:Double):Double = {
    def cbrtIter(guess:Double):Double = {
      val next = improve(guess);
      if (isGoodEnough(guess,next)) next
      else cbrtIter(next)
    }

    def improve(y:Double) = (x/(y*y)+2*y)/3;

    def isGoodEnough(prev:Double,guess:Double) = abs(prev-guess)/guess<0.001;

    cbrtIter(1.0)
  }

  Console.println("cbrt(2) = " + cbrt(2));
}

//############################################################################

object M4 {
  def pascal(c: Int, l: Int): Int =
    if (c <= 0 || c >= l) 1
    else pascal(c - 1, l - 1) + pascal(c, l - 1);

  Console.print(pascal(0,0));
  Console.println;

  Console.print(pascal(0,1));
  Console.print(' ');
  Console.print(pascal(1,1));
  Console.println;

  Console.print(pascal(0,2));
  Console.print(' ');
  Console.print(pascal(1,2));
  Console.print(' ');
  Console.print(pascal(2,2));
  Console.println;

  Console.print(pascal(0,3));
  Console.print(' ');
  Console.print(pascal(1,3));
  Console.print(' ');
  Console.print(pascal(2,3));
  Console.print(' ');
  Console.print(pascal(3,3));
  Console.println;

  Console.print(pascal(0,4));
  Console.print(' ');
  Console.print(pascal(1,4));
  Console.print(' ');
  Console.print(pascal(2,4));
  Console.print(' ');
  Console.print(pascal(3,4));
  Console.print(' ');
  Console.print(pascal(4,4));
  Console.println;
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    M0;
    M1;
    M2;
    M3;
    M4;
    ()
  }
}

//############################################################################
