//############################################################################
// Programmation IV - 2002 - Week 10
//############################################################################

import math.{Pi, log}

object M0 {

  def addLazyList (s1: LazyList[Int], s2: LazyList[Int]): LazyList[Int] =
    LazyList.cons(s1.head + s2.head, addLazyList(s1.tail, s2.tail));

  val fib: LazyList[Int] =
    LazyList.cons(0, LazyList.cons(1, addLazyList(this.fib, this.fib.tail)));

  def test() = {
    var i = 0;
    fib.take(20).foreach(n => {Console.println("fib("+i+") = "+n); i=i+1});
    Console.println()
  }
}

//############################################################################

object M1 {

  def scale(x: Double, s: LazyList[Double]): LazyList[Double] =
    s map { e: Double => e*x }

  def partialSums(s: LazyList[Double]): LazyList[Double] =
    LazyList.cons(s.head, partialSums(s.tail) map (x => x + s.head));

  def euler(s: LazyList[Double]): LazyList[Double] = {
    val nm1 = s apply 0;
    val n   = s apply 1;
    val np1 = s apply 2;
    LazyList.cons(np1 - ((np1 - n)*(np1 - n) / (nm1 - 2*n + np1)),euler(s.tail))
  };

  def better(s: LazyList[Double], transform: LazyList[Double] => LazyList[Double])
    : LazyList[LazyList[Double]] =
    LazyList.cons(s, better(transform(s), transform));

  def veryGood(s: LazyList[Double], transform: LazyList[Double] => LazyList[Double])
    : LazyList[Double] =
    better(s, transform) map (x => x.head);

  def lnSummands(n: Double): LazyList[Double] =
    LazyList.cons(1.0 / n, lnSummands(n + 1.0) map { x: Double => -x })

  var ln0 = partialSums(lnSummands(1.0));
  var ln1 = euler(ln0);
  var ln2 = veryGood(ln0, euler);

  def piSummands(n: Double): LazyList[Double] =
    LazyList.cons(1.0 / n, piSummands(n + 2.0) map { x: Double => -x })

  var pi0 = scale(4.0, partialSums(piSummands(1.0)));
  var pi1 = euler(pi0);
  var pi2 = veryGood(pi0, euler);

  def pad(s: String, n: Int): String =
    if (n <= 0) s.substring(0, s.length() + n)
    else pad(s + " ", n - 1);
  def str(d: Double) = { val s = d.toString(); pad(s, 18 - s.length()) };

  def test() = {
    var i = 0;
    while (i < 10) {
      Console.print("pi("+i+") = ");
      Console.print(str(pi0.apply(i)) + ", ");
      Console.print(str(pi1.apply(i)) + ", ");
      Console.print(str(pi2.apply(i)) + "\n");
      i = i + 1;
    }
    Console.print("pi    = ");
    Console.print(str(Pi) + ", ");
    Console.print(str(Pi) + ", ");
    Console.print(str(Pi) + "\n");
    Console.println()
    i = 0;
    while (i < 10) {
      Console.print("ln("+i+") = ");
      Console.print(str(ln0.apply(i)) + ", ");
      Console.print(str(ln1.apply(i)) + ", ");
      Console.print(str(ln2.apply(i)) + "\n");
      i = i + 1;
    }
    Console.print("ln    = ");
    Console.print(str(log(2)) + ", ");
    Console.print(str(log(2)) + ", ");
    Console.print(str(log(2)) + "\n");
    Console.println()
  }
}

//############################################################################

object M2 {

  class IntIterator(start: Int) extends Iterator[Int] {
    var current: Int = start;
    def hasNext = true;
    def next() = { current = current + 1; current - 1 };
  }

  class PrimeIterator() extends Iterator[Int] {
    var current: Iterator[Int] = new IntIterator(2);
    def hasNext = true;
    def next() = {
      val p = current.next();
      current = current filter { x => !((x % p) == 0) };
      p
    }
  }

  def test() = {
    val i = (new PrimeIterator()).take(30);
    Console.print("prime numbers:");
    while (i.hasNext) { Console.print(" " + i.next()); }
    Console.println()
  }
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    M0.test()
    M1.test()
    M2.test()
    ()
  }
}

//############################################################################
