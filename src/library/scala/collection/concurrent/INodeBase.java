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

abstract class INodeBase<K, V> extends BasicNode {

    @SuppressWarnings("unchecked")
    public static final AtomicReferenceFieldUpdater<INodeBase<?, ?>, MainNode<?, ?>> updater =
            AtomicReferenceFieldUpdater.newUpdater((Class<INodeBase<?, ?>>) (Class<?>) INodeBase.class, (Class<MainNode<?, ?>>) (Class<?>) MainNode.class, "mainnode");

    static final Object RESTART = new Object();

    static final Object NO_SUCH_ELEMENT_SENTINEL = new Object();

    public volatile MainNode<K, V> mainnode = null;

    public final Gen gen;

    public INodeBase(Gen generation) {
        gen = generation;
    }

    public BasicNode prev() {
        return null;
    }

}
