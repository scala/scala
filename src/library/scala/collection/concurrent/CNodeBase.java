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

import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

abstract class CNodeBase<K, V> extends MainNode<K, V> {

    @SuppressWarnings("unchecked")
    public static final AtomicIntegerFieldUpdater<CNodeBase<?, ?>> updater =
            AtomicIntegerFieldUpdater.newUpdater((Class<CNodeBase<?, ?>>) (Class<?>) CNodeBase.class, "csize");

    public volatile int csize = -1;

    public boolean CAS_SIZE(int oldval, int nval) {
        return updater.compareAndSet(this, oldval, nval);
    }

    public void WRITE_SIZE(int nval) {
        updater.set(this, nval);
    }

    public int READ_SIZE() {
        return updater.get(this);
    }

}
