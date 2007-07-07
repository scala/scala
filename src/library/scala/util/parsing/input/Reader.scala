/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.input

/** An interface for streams of values that have positions
 *
 * @author Martin Odersky, Adriaan Moors
 */
abstract class Reader[+T] {

   /** Returns the first element of the stream
    */
  def first: T

  /** Returns an abstract reader consisting of all elements except the first
   *
   * @return If `atEnd' is true, the result will be `this'; otherwise, it's a `Reader' containing
   *         more elements.
   */
  def rest: Reader[T]

  /** The position of the first element in the stream
   */
  def pos: Position

  /** Whether there are any more elements in this reader besides the first.
   * (i.e., whether calling `rest' will yield a `Reader' with more elements)
   */
  def atEnd: boolean
}
