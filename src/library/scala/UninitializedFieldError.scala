/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $


package scala

import Predef._

/** This class implements errors which are thrown whenever a
 *  field is used before it has been initialized.
 *
 *  Such runtime checks are not emitted by default. See the
 *  compiler documentation for knowing how to turn them on.
 *
 *  Note: This check requires the new initialization order,
 *    planned for 2.8.0 and available under -Xexperimental.
 *
 *  @since 2.7
 */
final case class UninitializedFieldError(msg: String)
           extends RuntimeException(msg) {
  def this(obj: Any) =
    this(if (null != obj) obj.toString() else "null")
}
