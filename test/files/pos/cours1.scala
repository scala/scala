object test {

  def size = 2;
  def pi = 3.14159;
  def radius = 10;
  def circumference = 2 * pi * radius;
  def square(x: Double) = x * x;
  def sumOfSquares(x: Double, y: Double) = square(x) + square(y);
  def loop: Int = loop;
  def first(x: Int, y: Int) = x;
  def constOne(x: Int, def y: Int) = 1;
  def abs(x: Double) = if (x >= 0) x else -x;

  def sqrtIter0(guess: Double, x: Double): Double =
    if (isGoodEnough0(guess, x)) guess
    else sqrtIter0(improve0(guess, x), x);
  def improve0(guess: Double, x: Double) =
    (guess + x / guess) / 2;
  def isGoodEnough0(guess: Double, x: Double) =
    abs(square(guess) - x) < 0.001;
  def sqrt0(x: Double) = sqrtIter0(1.0, x);

  def sqrt1(x: Double) = {
    def sqrtIter1(guess: Double, x: Double): Double =
      if (isGoodEnough1(guess, x)) guess
      else sqrtIter1(improve1(guess, x), x);

    def improve1(guess: Double, x: Double) =
      (guess + x / guess) / 2;

    def isGoodEnough1(guess: Double, x: Double) =
      abs(square(guess) - x) < 0.001;

    sqrtIter1(1.0, x);
  }

  def sqrt2(x: Double) = {
    def sqrtIter2(guess: Double): Double =
      if (isGoodEnough2(guess)) guess
      else sqrtIter2(improve2(guess));

    def improve2(guess: Double) =
      (guess + x / guess) / 2;

    def isGoodEnough2(guess: Double) =
      abs(square(guess) - x) < 0.001;

    sqrtIter2(1.0);
  }

  sqrt0(2);
  sqrt1(2);
  sqrt2(2);
}