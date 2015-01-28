/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2015, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.control

object BreakableGenerators {
  import scala.language.implicitConversions

  type Generator[+A] = Iterator[A]
  type BreakableGenerator[+A] = BreakableIterator[A]

  /**
   * Generates a new breakable generator from any traversable object.
   * Example usage:
   * import scala.util.control.BreakableGenerators._
   * {{{
   * for {
   *   loop <- breakable(1 to 1000);
   *   j <- loop;
   *   ...
   * } yield { ... }
   * }}}
   * Traversable object include sequences, iterators, ranges, etc.
   */
  def breakable[A](t1: TraversableOnce[A]): Generator[BreakableGenerator[A]] =
    List(new BreakableIterator(t1.toIterator)).iterator

  /**
   * Generates a breakable generator for infinite looping
   * Example usage:
   * import scala.util.control.BreakableGenerators._
   * {{{
   * for {
   *   loop <- infinite;
   *   _ <- loop;
   *   ...
   * } yield { ... }
   * }}}
   */
  def infinite: Generator[BreakableGenerator[Unit]] = breakable(Iterator.continually(()))


  // Mediates boolean expression with 'break' and 'continue' invocations
  private[control] case class BreakableGuardCondition(cond: Boolean) {
    /**
     * Break the looping over one or more breakable generators, if 'cond' evaluates to true.
     *  Example usage:
     *  {{{
     *  import scala.util.control.BreakableGenerators._
     *  for {
     *    loop <- breakable(1 to 1000);
     *    j <- loop;
     *    if { j > 10 } break(loop);  // break when 'j' exceeds 10
     *    ...
     *  } yield { ... }
     * }}}
     */
    def break(b: BreakableGenerator[_], bRest: BreakableGenerator[_]*): Boolean = {
      if (cond) {
        b.break
        for (x <- bRest) { x.break }
      }
      !cond
    }

    /**
     * Continue to next iteration of enclosing generator if 'cond' evaluates to true.
     * Example usage:
     * {{{
     * import scala.language.postfixOps
     * import scala.util.control.BreakableGenerators._
     * for {
     *   loop <- breakable(1 to 1000);
     *   j <- loop;
     *   if { j % 2 == 1 } continue;  // continue when 'j' is odd
     *   ...
     * } yield { ... }
     * }}}
     */
    def continue: Boolean = !cond
  }

  // implicit conversion of boolean values to breakable guard condition mediary
  implicit def toBreakableGuardCondition(cond: Boolean) = BreakableGuardCondition(cond)

  // An iterator that can be halted via its 'break' method.  Not invoked directly
  private[control] sealed class BreakableIterator[+A](itr: Iterator[A]) extends Iterator[A] {
    private var broken = false
    private[BreakableGenerators] def break { broken = true }

    def hasNext = !broken && itr.hasNext
    def next = itr.next
  }
}
