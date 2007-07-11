/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.input

/** An interface for streams of values that have positions.
 *
 * @author Martin Odersky, Adriaan Moors
 */
abstract class Reader[+T] {

   /** Returns the first element of the stream
    */
  def first: T

  /** Returns an abstract reader consisting of all elements except the first
   *
   * @return If <code>atEnd</code> is <code>true</code>, the result will be
   *         <code>this'; otherwise, it's a <code>Reader</code> containing
   *         more elements.
   */
  def rest: Reader[T]

  /** The position of the first element in the stream
   */
  def pos: Position

  /** Whether there are any more elements in this reader besides the first.
   * (i.e., whether calling `rest' will yield a `Reader' with more elements)
   */
  def atEnd: Boolean
}
