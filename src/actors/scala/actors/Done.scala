/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * @author Philipp Haller
 */
class Done extends Throwable {
  override def fillInStackTrace(): Throwable =
    this;
}

class ContinueException extends Throwable {
  override def fillInStackTrace(): Throwable =
    this;
}

class AbortException extends Throwable {
  override def fillInStackTrace(): Throwable =
    this;
}
