module m1 {

    def id(x: Int): Double = x;
    def cube(x: Int): Double = x * x * x;
    def reciprocal(x: Int): Double = 1.0/x;

    def sumInts(a: Int, b: Int): Double =
      if (a > b) 0
      else a + sumInts(a + 1, b);

    def sumCubes(a: Int, b: Int): Double =
      if (a > b) 0
      else cube(a) + sumCubes(a + 1, b);

    def sumReciprocals(a: Int, b: Int): Double =
      if (a > b) 0
      else 1.0/a + sumReciprocals(a, b);

}

module m2 {

    def id(x: Int): Double = x;
    def cube(x: Int): Double = x * x * x;
    def reciprocal(x: Int): Double = 1.0/x;

    def sum(f: Int => Double, a: Int, b: Int): Double =
      if (a > b) 0
      else f(a) + sum(f, a + 1, b);

    def sumInts(a: Int, b: Int): Double = sum(id, a, b);
    def sumCubes(a: Int, b: Int): Double = sum(cube, a, b);
    def sumReciprocals(a: Int, b: Int): Double = sum(reciprocal, a, b);
}

module m3 {

    def sum(f: Int => Double, a: Int, b: Int): Double =
      if (a > b) 0
      else f(a) + sum(f, a + 1, b);

    def sumInts(a: Int, b: Int): Double = sum((x => x), a, b);
    def sumCubes(a: Int, b: Int): Double = sum((x => x * x * x), a, b);
    def sumReciprocals(a: Int, b: Int): Double = sum((x => 1.0/x), a, b);
}

module m4 {

    def sum(f: Int => Double) = {
      def sumF(a: Int, b: Int): Double =
        if (a > b) 0
        else f(a) + sumF(a + 1, b);
      sumF
    }

    def sumInts = sum(x => x);
    def sumCubes = sum(x => x * x * x);
    def sumReciprocals = sum(x => 1.0/x);

    sumCubes(1, 10) + sumReciprocals(10, 20);
}

module m5 {

    def sum(f: Int => Double): (Int, Int) => Double = (a, b) =>
      if (a > b) 0
      else f(a) + sum(f)(a + 1, b);

    def sumInts = sum(x => x);
    def sumCubes = sum(x => x * x * x);
    def sumReciprocals = sum(x => 1.0/x);

    sumCubes(1, 10) + sumReciprocals(10, 20);
}

module m6 {

    def sum(f: Int => Double)(a: Int, b: Int): Double =
      if (a > b) 0
      else f(a) + sum(f)(a + 1, b);

    def sumInts = sum(x => x);
    def sumCubes = sum(x => x * x * x);
    def sumReciprocals = sum(x => 1.0/x);

    sumCubes(1, 10) + sumReciprocals(10, 20);
}

module m7 {

    def sum(f: Int => Double)(a: Int, b: Int): Double = {
      def iter(a: Int, result: Double): Double =
        if (a > b) result
	else iter(a + 1, f(a) + result);
      iter(a, 0);
    }

    def sumInts = sum(x => x);
    def sumCubes = sum(x => x * x * x);
    def sumReciprocals = sum(x => 1.0/x);

    sumCubes(1, 10) + sumReciprocals(10, 20);
}

module m8 {

  def product(f: Int => Double)(a: Int, step: Int, b: Int): Double =
    if (a > b) 1
    else f(a) * product(f)(a + step, step, b);

  val pi = 8 * product(x => x * x)(4, 2, 40) / product(x => x * x)(3, 2, 40);
}

module m9 {

  def accumulate[t](combiner: (t, t) => t, nullValue: t, f: Int => t, next: Int => Int)
		   (a: Int, b: Int): t =
    if (a > b) nullValue
    else combiner(f(a), accumulate(combiner, nullValue, f, next)(next(a), b));

  def inc(x: Int) = x + 1;

  def sum(f: Int => Double) =
    accumulate((x: Double, y: Double) => x + y, 0, f, inc);
  def product(f: Int => Double) =
    accumulate((x: Double, y: Double) => x * y, 1, f, inc);
}

