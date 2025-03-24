/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.concurrent;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

abstract class MainNode<K, V> extends BasicNode {

    @SuppressWarnings("unchecked")
    public static final AtomicReferenceFieldUpdater<MainNode<?, ?>, MainNode<?, ?>> updater =
            AtomicReferenceFieldUpdater.newUpdater((Class<MainNode<?, ?>>) (Class<?>) MainNode.class, (Class<MainNode<?, ?>>) (Class<?>) MainNode.class, "prev");

    public volatile MainNode<K, V> prev = null;

    public abstract int cachedSize(Object ct);

    // standard contract
    public abstract int knownSize();

    public boolean CAS_PREV(MainNode<K, V> oldval, MainNode<K, V> nval) {
        return updater.compareAndSet(this, oldval, nval);
    }

    public void WRITE_PREV(MainNode<K, V> nval) {
        updater.set(this, nval);
    }

    // do we need this? unclear in the javadocs...
    // apparently not - volatile reads are supposed to be safe
    // regardless of whether there are concurrent ARFU updates
    @Deprecated @SuppressWarnings("unchecked")
    public MainNode<K, V> READ_PREV() {
        return (MainNode<K, V>) updater.get(this);
    }

}
