/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.regexp

/** This runtime exception is thrown if an attempt to instantiate a
 *  syntactically incorrect expression is detected.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
class SyntaxError(e: String) extends RuntimeException(e)
