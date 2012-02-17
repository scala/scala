/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



abstract class AbstractPromise {
    private volatile Object _ref = null;
    protected final static AtomicReferenceFieldUpdater<AbstractPromise, Object> updater =
	AtomicReferenceFieldUpdater.newUpdater(AbstractPromise.class, Object.class, "_ref");
}
