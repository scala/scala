//############################################################################
// Programmation IV - 2002 - Week 01
//############################################################################
// $Id$

module M0 {

  //##########################################################################

  java.lang.System.out.println(87 + 145);
  java.lang.System.out.println(1000 - 333);
  java.lang.System.out.println(5 + 2 * 3);

  //##########################################################################

  def size = 2;
  def pi = 3.14159;
  def radius = 10;
  def circumference = 2 * pi * radius;

  java.lang.System.out.println(5 * size);
  java.lang.System.out.println(2 * pi * radius);
  java.lang.System.out.println(circumference);
  java.lang.System.out.println((2 * pi) * radius);

  //##########################################################################

  def square(x: Double) = x * x;

  java.lang.System.out.println(square(2));
  java.lang.System.out.println(square(5 + 4));
  java.lang.System.out.println(square(square(4)));

  //##########################################################################

  def sumOfSquares(x: Double, y: Double) = square(x) + square(y);

  java.lang.System.out.println(sumOfSquares(3, 2+2));

  //##########################################################################

  def loop: Int = loop;
  def first(x: Int, y: Int) = x;
  def constOne(x: Int, def y: Int) = 1;

  java.lang.System.out.println(constOne(1, loop));

  //##########################################################################

  def abs(x: Double) = if (x >= 0) x else -x;

  java.lang.System.out.println(abs(737));
  java.lang.System.out.println(abs(1));
  java.lang.System.out.println(abs(0));
  java.lang.System.out.println(abs(-1));
  java.lang.System.out.println(abs(-76));

  //##########################################################################

  def sqrtIter0(guess: Double, x: Double): Double =
    if (isGoodEnough0(guess, x)) guess
    else sqrtIter0(improve0(guess, x), x);

  def improve0(guess: Double, x: Double) =
    (guess + x / guess) / 2;

  def isGoodEnough0(guess: Double, x: Double) =
    abs(square(guess) - x) < 0.001;

  def sqrt0(x: Double) = sqrtIter0(1.0, x);

  java.lang.System.out.println(sqrt0(2));
  java.lang.System.out.println(sqrt0(3));
  java.lang.System.out.println(sqrt0(4));

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

  java.lang.System.out.println(sqrt1(2));
  java.lang.System.out.println(sqrt1(3));
  java.lang.System.out.println(sqrt1(4));

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

  java.lang.System.out.println(sqrt2(2));
  java.lang.System.out.println(sqrt2(3));
  java.lang.System.out.println(sqrt2(4));

  //##########################################################################
}

//############################################################################

module M1 {
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

  java.lang.System.out.println("sqrt(2) = " + sqrt(2));
}

//############################################################################

module M2 {
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

  java.lang.System.out.println("sqrt(2) = " + sqrt(2));
}

//############################################################################

module M3 {
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

  java.lang.System.out.println("cbrt(2) = " + cbrt(2));
}

//############################################################################

module M4 {
  def pascal(c: Int, l: Int): Int =
    if (c <= 0 || c >= l) 1
    else pascal(c - 1, l - 1) + pascal(c, l - 1);

  java.lang.System.out.print(pascal(0,0));
  java.lang.System.out.println();

  java.lang.System.out.print(pascal(0,1));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(1,1));
  java.lang.System.out.println();

  java.lang.System.out.print(pascal(0,2));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(1,2));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(2,2));
  java.lang.System.out.println();

  java.lang.System.out.print(pascal(0,3));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(1,3));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(2,3));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(3,3));
  java.lang.System.out.println();

  java.lang.System.out.print(pascal(0,4));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(1,4));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(2,4));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(3,4));
  java.lang.System.out.print(' ');
  java.lang.System.out.print(pascal(4,4));
  java.lang.System.out.println();
}

//############################################################################

module Test {
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
