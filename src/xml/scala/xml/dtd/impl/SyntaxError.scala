/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package xml.dtd.impl

/** This runtime exception is thrown if an attempt to instantiate a
 *  syntactically incorrect expression is detected.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
@deprecated("This class will be removed", "2.10.0")
private[dtd] class SyntaxError(e: String) extends RuntimeException(e)
