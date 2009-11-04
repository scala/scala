/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** This class implements errors which are thrown whenever an
 *  object doesn't match any pattern of a pattern matching
 *  expression.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 1.1, 05/03/2004
 *  @since   2.0
 */
final class MatchError(msg: String) extends RuntimeException(msg) {
  def this(obj: Any) =
    this(if (null != obj) obj.toString() else "null")
}
