/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

/**
 * The message sent to a message box when the period specified in
 * `receiveWithin` expires.
 *
 * @author  Martin Odersky
 * @version 1.0, 10/03/2003
 */
@deprecated("use actors instead", "2.8.0")
case object TIMEOUT
