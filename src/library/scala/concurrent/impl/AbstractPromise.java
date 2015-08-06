/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2015, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl;

import java.util.concurrent.atomic.AtomicReference;

@Deprecated // Since 2.11.8. Extend java.util.concurrent.atomic.AtomicReference instead.
abstract class AbstractPromise extends AtomicReference<Object> {
  protected final boolean updateState(Object oldState, Object newState) { return compareAndSet(oldState, newState); }
  protected final Object getState() { return get(); }
}
