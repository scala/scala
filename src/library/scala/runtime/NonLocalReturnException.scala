/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime

import scala.util.control.ControlThrowable

/** !!! This class has been replaced by NonLocalReturnControl and should be deleted.
 *  But, it can't be deleted until starr is updated to use the new name.
 */
class NonLocalReturnException[T](val key: AnyRef, val value: T) extends ControlThrowable
