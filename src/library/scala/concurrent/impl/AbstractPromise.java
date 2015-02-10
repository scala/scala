/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
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
    /*
     * In a runtime with a security manager installed, or a non-Oracle runtime,
     * the Unsafe class may be unavailable. In this case, _refoffset will be
     * set to -1.
     *
     * We assume that a valid field offset will be non-negative, and likely a
     * multiple of 4.
     */
    static {
    	long refoffset = -1;
	try {
	    refoffset = Unsafe.instance.objectFieldOffset(AbstractPromise.class.getDeclaredField("_ref"));
	} catch (Throwable t) {
	    /* Unsafe object instance access failed */
	}
	_refoffset = refoffset;
    }
    
    /*
     * The Hotspot compiler can identify that the value of _refoffset never
     * changes, and compile only the branch below that is used. When the Unsafe
     * object is available, the compare-and-swap operation will be inlined
     * directly into *this* method's caller.
     */
    protected final boolean updateState(Object oldState, Object newState) {
    	if (_refoffset == -1) {
	    return updater.compareAndSet(this, oldState, newState);
	} else {
	    return Unsafe.instance.compareAndSwapObject(this, _refoffset, oldState, newState);
	}
    }

    protected final Object getState() {
	return _ref;
    }

    protected final static AtomicReferenceFieldUpdater<AbstractPromise, Object> updater =
	AtomicReferenceFieldUpdater.newUpdater(AbstractPromise.class, Object.class, "_ref");
}
