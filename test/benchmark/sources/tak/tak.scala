/*                     __                                               *\
**     ________ ___   / /  ___     Scala benchmark suite                **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package benchmarks;

/** The Tak recursive function. Translated from the MLton benchmarking
    suite.  */
class Tak {
  def tak(x: Int, y: Int, z: Int): Int =
    if (! (y < x))
      z
    else
      tak (tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, z, y));

  def f(x: Int): Unit = x match {
    case 0 => ();
    case n => {tak (36, 42, 12); f (n - 1) }
  }
}

object tak extends Tak with scala.testing.Benchmark {
  def run = f(10000);
}
