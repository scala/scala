/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;

import Predef._

/** This class implements errors which are thrown whenever an
 *  object doesn't match any pattern of a pattern matching
 *  expression.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 1.1, 05/03/2004
 */
object MatchError {

  // todo: change pattern matcher so that dummy type parameter T can be removed.
  def fail[T](source: String, line: Int): Bottom = throw new MatchError(source, line);

  def report(source: String, line: Int, obj: Any) =
    try {
      throw new MatchError(source, line, obj.toString())
    } catch {
      case e: MatchError => throw e
      case e: Throwable => throw new MatchError(source, line)
    }
}

final class MatchError(msg: String) extends Error(msg) {
    def this(source: String, line: Int) =
      this(" in '" + source + "' at line " + line);
    def this(source: String, line: Int, obj: String) =
      this("for object " + obj + " in '" + source + "' at line " + line);

    def this(ob:Any) =
      this(ob.toString());
}
