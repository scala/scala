/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sequence.scala 16092 2008-09-12 10:37:06Z nielsen $


package scalax.collection

import generic._
import immutable.Nil

/** Class <code>Sequence[A]</code> represents finite sequences of elements
 *  of type <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Sequence[+A] extends OrderedIterable[A] with SizedIterable[A] with covariant.SequenceTemplate[Sequence, A]

object Sequence extends covariant.SequenceFactory[Sequence] {

  /** The empty sequence */
  val empty : Sequence[Nothing] = immutable.Nil

  type View[+UC[+B] <: Sequence[B], +A] = covariant.SequenceView[UC, A]

  /** @deprecated use View instead
   */
  @deprecated type Projection[A] = View[C, A] forSome { type C[+B] <: Sequence[B] }

  /** @deprecated use Sequence(value) instead */
  @deprecated def singleton[A](value: A) = Sequence(value)

  /** Builds a singleton sequence.
   *
   * @deprecated use <code>Sequence(x)</code> instead.
   */
  @deprecated def single[A](x: A) = singleton(x)
}

