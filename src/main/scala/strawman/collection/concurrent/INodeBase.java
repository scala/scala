/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman.collection.concurrent;

import java.lang.Object;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

abstract class INodeBase<K, V> extends BasicNode {

    @SuppressWarnings("unchecked")
    public static final AtomicReferenceFieldUpdater<INodeBase<?, ?>, MainNode<?, ?>> updater =
            AtomicReferenceFieldUpdater.newUpdater((Class<INodeBase<?, ?>>) (Class<?>) INodeBase.class, (Class<MainNode<?, ?>>) (Class<?>) MainNode.class, "mainnode");

    public static final Object RESTART = new Object();

    public volatile MainNode<K, V> mainnode = null;

    public final Gen gen;

    public INodeBase(Gen generation) {
        gen = generation;
    }

    public BasicNode prev() {
	return null;
    }

}