/*                     __                                               *\
**     ________ ___   / /  ___     Scala benchmark suite                **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */


package benchmarks;

/** Quick sort with a functional taste. */
object sort1 with scala.testing.Benchmark {

  def sort(a: List[Int]): List[Int] = {
    if (a.length < 2)
      a
    else {
      val pivot = a(a.length / 2);
      sort(a.filter(x => x < pivot))
        ::: a.filter(x => x == pivot)
        ::: sort(a.filter(x => x > pivot))
    }
  }

  def run: Unit = sort(List.range(1, 10000));
}
