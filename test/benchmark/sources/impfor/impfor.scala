/*                     __                                               *\
**     ________ ___   / /  ___     Scala benchmark suite                **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */


package benchmarks;

/** Imperative for construct. Translated from MLton benchmarking suite */

class For {
  def impFor[A](start: Int, stop: Int, f: Int => A) = {
    var i = start;
    def loop: Unit = if (i >= stop) ()
		     else { f(i); i = i + 1; loop }

    loop;
  }
}


object impfor extends For with scala.testing.Benchmark {

  def doit: Unit = {
    var x = 0;

    impFor(0, 10, t =>
      impFor(0, 10, t =>
	impFor(0, 10, t =>
	  impFor(0, 10, t =>
	    impFor(0, 10, t =>
	      impFor(0, 10, t =>
		impFor(0, 10, t => x = x + 1))))))); // 7 inner loops

    if (x != 10000000) {
      Console.println("Error");
      System.exit(1);
    }
  }

  def run: Unit = doit;
}
