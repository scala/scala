/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package script

/** Class `Location` describes locations in messages implemented by
 *  class [[scala.collection.script.Message]].
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/05/2004
 *  @since   2.8
 */

@deprecated("scripting is deprecated", "2.11.0")
sealed abstract class Location

@deprecated("scripting is deprecated", "2.11.0")
case object Start extends Location

@deprecated("scripting is deprecated", "2.11.0")
case object End extends Location

@deprecated("scripting is deprecated", "2.11.0")
case object NoLo extends Location

@deprecated("scripting is deprecated", "2.11.0")
case class Index(n: Int) extends Location
