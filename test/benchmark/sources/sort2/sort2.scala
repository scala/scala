/*                     __                                               *\
**     ________ ___   / /  ___     Scala benchmark suite                **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package benchmarks;

/** Quick-sort a list of integers, version 2. Taken from the
    Scala distribution examples.  */
class Sorter {

  def sort(a: List[Int]): List[Int] = {
    if (a.length < 2)
      a
    else {
      val pivot = a(a.length / 2);
      def lePivot(x: Int) = x < pivot;
      def gtPivot(x: Int) = x > pivot;
      def eqPivot(x: Int) = x == pivot;
      sort(a filter lePivot)
        ::: sort(a filter eqPivot)
        ::: sort(a filter gtPivot)
    }
  }

}

object sort2 extends Sorter with scala.testing.Benchmark {
  def run: Unit = sort(List.range(1,10000));

}
