/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-04, LAMP/EPFL               **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.concurrent;

class Lock {
  var available = true;
  def acquire = synchronized {
    if (!available) wait();
    available = false
  }
  def release = synchronized {
    available = true;
    notify()
  }
}
