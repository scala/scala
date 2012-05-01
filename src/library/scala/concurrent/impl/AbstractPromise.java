/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl;


import scala.concurrent.util.Unsafe;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



abstract class AbstractPromise {
    private volatile Object _ref;
    
    final static long _refoffset;
    
    static {
	try {
	    _refoffset = Unsafe.instance.objectFieldOffset(AbstractPromise.class.getDeclaredField("_ref"));
	} catch (Throwable t) {
	    throw new ExceptionInInitializerError(t);
	}
    }
    
    protected final boolean updateState(Object oldState, Object newState) {
	return Unsafe.instance.compareAndSwapObject(this, _refoffset, oldState, newState);
    }
    
    protected final Object getState() {
	return _ref;
    }
    
    protected final static AtomicReferenceFieldUpdater<AbstractPromise, Object> updater =
	AtomicReferenceFieldUpdater.newUpdater(AbstractPromise.class, Object.class, "_ref");
}