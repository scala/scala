/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.parsing.input

/** A trait for objects that have a source position.
 *
 * @author Martin Odersky, Adriaan Moors
 */
trait Positional {

  /** The source position of this object, initially set to undefined. */
  var pos: Position = NoPosition

  /** If current source position is undefined, update it with given position `newpos`
   *  @return  the object itself
   */
  def setPos(newpos: Position): this.type = {
    if (pos eq NoPosition) pos = newpos
    this
  }
}


