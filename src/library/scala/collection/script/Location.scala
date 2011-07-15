/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package script

/** Class `Location` describes locations in messages implemented by
 *  class [[scala.collection.script.Message]].
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/05/2004
 *  @since   2.8
 */

sealed abstract class Location
case object Start extends Location
case object End extends Location
case object NoLo extends Location
case class Index(n: Int) extends Location
