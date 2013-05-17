/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.concurrent;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

abstract class MainNode<K, V> extends BasicNode {

    @SuppressWarnings("rawtypes")
    public static final AtomicReferenceFieldUpdater<MainNode, MainNode> updater =
            AtomicReferenceFieldUpdater.newUpdater(MainNode.class, MainNode.class, "prev");

    public volatile MainNode<K, V> prev = null;

    public abstract int cachedSize(Object ct);

    public boolean CAS_PREV(MainNode<K, V> oldval, MainNode<K, V> nval) {
	return updater.compareAndSet(this, oldval, nval);
    }

    public void WRITE_PREV(MainNode<K, V> nval) {
	updater.set(this, nval);
    }

    // do we need this? unclear in the javadocs...
    // apparently not - volatile reads are supposed to be safe
    // irregardless of whether there are concurrent ARFU updates
    @Deprecated @SuppressWarnings("unchecked")
    public MainNode<K, V> READ_PREV() {
	return updater.get(this);
    }

}