/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** Until scala 2.10.0 this marker trait was added to
 *  scala-compiled classes.  Now it only exists for backward
 *  compatibility.
 */
@deprecated("ScalaObject will be removed", "2.10.0")
trait ScalaObject
