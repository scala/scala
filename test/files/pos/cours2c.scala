module m1  {

  def average(x: Double, y: Double) = (x + y)/2;
  def abs(x: Double): Double = if (x < 0) - x else x;
  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < 0.001;

  def search(f: Double => Double, negPoint: Double, posPoint: Double): Double = {
    val midPoint = average(negPoint, posPoint);
    if (isCloseEnough(negPoint, posPoint))
      midPoint
    else {
      val testVal = f(midPoint);
      if (testVal > 0) search (f, negPoint, midPoint)
      else if (testVal < 0) search (f, midPoint, posPoint)
      else midPoint
    }
  }

  def halfIntervalMethod(f: Double => Double, a: Double, b: Double): Double = {
    val aval = f(a);
    val bval = f(b);
    if (aval < 0 && bval > 0) search(f, a, b)
    else if (bval < 0 && aval > 0) search(f, b, a)
    else error("Values are not of opposite sign")
  }
}

module m2 {

  def abs(x: Double): Double = if (x < 0) - x else x;
  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < 0.001;
  def average(x: Double, y: Double) = (x + y)/2;

  def fixedPoint(f: Double => Double, firstGuess: Double) = {
    def try(guess: Double): Double = {
      val next = f(guess);
      if (isCloseEnough(guess, next)) next
      else try(next)
    }
    try(firstGuess);
  }

  def sin(x: Double): Double = x;
  def cos(x: Double): Double = x;

  val result = fixedPoint((y => sin(y) + cos(y)), 1.0);

  def averageDamp(f: Double => Double)(x: Double) =
    average(x, f(x));

  def sqrt(x: Double) =
    fixedPoint(averageDamp(y => x/y), 1.0);

  def cubeRoot(x: Double) =
    fixedPoint(averageDamp(y => x/(y*y)), 1.0);
}

