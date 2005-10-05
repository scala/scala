//############################################################################
// Programmation IV - 2002 - Week 10
//############################################################################
// $Id$

import java.lang.System; // to avoid name clash with .NET's library

object M0 {

  def addStream (s1: Stream[int], s2: Stream[int]): Stream[int] =
    Stream.cons(s1.head + s2.head, addStream(s1.tail, s2.tail));

  val fib: Stream[int] =
    Stream.cons(0, Stream.cons(1, addStream(this.fib, this.fib.tail)));

  def test = {
    var i = 0;
    fib.take(20).foreach(n => {System.out.println("fib("+i+") = "+n); i=i+1});
    System.out.println();
  }
}

//############################################################################

object M1 {

  def scale(x: double, s: Stream[double]): Stream[double] =
    s map (e: double => e*x);

  def partialSums(s: Stream[double]): Stream[double] =
    Stream.cons(s.head, partialSums(s.tail) map (x => x + s.head));

  def euler(s: Stream[double]): Stream[double] = {
    val nm1 = s at 0;
    val n   = s at 1;
    val np1 = s at 2;
    Stream.cons(np1 - ((np1 - n)*(np1 - n) / (nm1 - 2*n + np1)),euler(s.tail))
  };

  def better(s: Stream[double], transform: Stream[double] => Stream[double])
    : Stream[Stream[double]] =
    Stream.cons(s, better(transform(s), transform));

  def veryGood(s: Stream[double], transform: Stream[double] => Stream[double])
    : Stream[double] =
    better(s, transform) map (x => x.head);

  def lnSummands(n: double): Stream[double] =
    Stream.cons(1.0 / n, lnSummands(n + 1.0) map (x: double => -x));

  var ln0 = partialSums(lnSummands(1.0));
  var ln1 = euler(ln0);
  var ln2 = veryGood(ln0, euler);

  def piSummands(n: double): Stream[double] =
    Stream.cons(1.0 / n, piSummands(n + 2.0) map (x: double => -x));

  var pi0 = scale(4.0, partialSums(piSummands(1.0)));
  var pi1 = euler(pi0);
  var pi2 = veryGood(pi0, euler);

  def pad(s: String, n: int): String =
    if (n <= 0) s.substring(0, s.length() + n)
    else pad(s + " ", n - 1);
  def str(d: double) = { val s = d.toString(); pad(s, 18 - s.length()) };

  def test = {
    var i = 0;
    while (i < 10) {
      System.out.print("pi("+i+") = ");
      System.out.print(str(pi0.at(i)) + ", ");
      System.out.print(str(pi1.at(i)) + ", ");
      System.out.print(str(pi2.at(i)) + "\n");
      i = i + 1;
    }
    System.out.print("pi    = ");
    System.out.print(str(Math.PI) + ", ");
    System.out.print(str(Math.PI) + ", ");
    System.out.print(str(Math.PI) + "\n");
    System.out.println();
    i = 0;
    while (i < 10) {
      System.out.print("ln("+i+") = ");
      System.out.print(str(ln0.at(i)) + ", ");
      System.out.print(str(ln1.at(i)) + ", ");
      System.out.print(str(ln2.at(i)) + "\n");
      i = i + 1;
    }
    System.out.print("ln    = ");
    System.out.print(str(Math.log(2)) + ", ");
    System.out.print(str(Math.log(2)) + ", ");
    System.out.print(str(Math.log(2)) + "\n");
    System.out.println();
  }
}

//############################################################################

object M2 {

  class IntIterator(start: int) extends Iterator[int] {
    var current: int = start;
    def hasNext = true;
    def next = { current = current + 1; current - 1 };
  }

  class PrimeIterator() extends Iterator[int] {
    var current: Iterator[int] = new IntIterator(2);
    def hasNext = true;
    def next = {
      val p = current.next;
      current = current filter { x => !((x % p) == 0) };
      p
    }
  }

  def test = {
    val i = (new PrimeIterator()).take(30);
    System.out.print("prime numbers:");
    while (i.hasNext) { System.out.print(" " + i.next); }
    System.out.println();
  }
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    M0.test;
    M1.test;
    M2.test;
    ()
  }
}

//############################################################################
