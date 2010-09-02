/*
  File: ConditionVariable.java
  Originally written by Doug Lea and released into the public domain.
  This may be used for any purposes whatsoever without acknowledgment.
  Thanks for the assistance and support of Sun Microsystems Labs,
  and everyone contributing, testing, and using this code.
  History:
  Date       Who                What
  11Jun1998  dl               Create public version
 */

package scala.actors.threadpool.locks;

import java.util.Collection;
import java.util.Date;
import scala.actors.threadpool.*;
import scala.actors.threadpool.helpers.*;

class FIFOCondVar extends CondVar implements Condition, java.io.Serializable {
    private static final long serialVersionUID = -497497271881010475L;

    private static final WaitQueue.QueuedSync sync = new WaitQueue.QueuedSync() {
        public boolean recheck(WaitQueue.WaitNode node) { return false; }
        public void takeOver(WaitQueue.WaitNode node) {}
    };

    // wait queue; only accessed when holding the lock
    private final WaitQueue wq = new FIFOWaitQueue();

    /**
     * Create a new CondVar that relies on the given mutual exclusion lock.
     * @param lock A non-reentrant mutual exclusion lock.
     */
    FIFOCondVar(ExclusiveLock lock) {
        super(lock);
    }

    public void awaitUninterruptibly() {
        int holdCount = lock.getHoldCount();
        if (holdCount == 0) {
            throw new IllegalMonitorStateException();
        }
        WaitQueue.WaitNode n = new WaitQueue.WaitNode();
        wq.insert(n);
        for (int i=holdCount; i>0; i--) lock.unlock();
        try {
            n.doWaitUninterruptibly(sync);
        }
        finally {
            for (int i=holdCount; i>0; i--) lock.lock();
        }
    }

    public void await() throws InterruptedException {
        int holdCount = lock.getHoldCount();
        if (holdCount == 0) {
            throw new IllegalMonitorStateException();
        }
        if (Thread.interrupted()) throw new InterruptedException();
        WaitQueue.WaitNode n = new WaitQueue.WaitNode();
        wq.insert(n);
        for (int i=holdCount; i>0; i--) lock.unlock();
        try {
            n.doWait(sync);
        }
        finally {
            for (int i=holdCount; i>0; i--) lock.lock();
        }
    }

    public boolean await(long timeout, TimeUnit unit) throws InterruptedException {
        int holdCount = lock.getHoldCount();
        if (holdCount == 0) {
            throw new IllegalMonitorStateException();
        }
        if (Thread.interrupted()) throw new InterruptedException();
        long nanos = unit.toNanos(timeout);
        WaitQueue.WaitNode n = new WaitQueue.WaitNode();
        wq.insert(n);
        boolean success = false;
        for (int i=holdCount; i>0; i--) lock.unlock();
        try {
            success = n.doTimedWait(sync, nanos);
        }
        finally {
            for (int i=holdCount; i>0; i--) lock.lock();
        }
        return success;
    }

//    public long awaitNanos(long timeout) throws InterruptedException {
//        throw new UnsupportedOperationException();
//    }
//
    public boolean awaitUntil(Date deadline) throws InterruptedException {
        if (deadline == null) throw new NullPointerException();
        long abstime = deadline.getTime();
        long start = System.currentTimeMillis();
        long msecs = abstime - start;
        return await(msecs, TimeUnit.MILLISECONDS);
    }

    public void signal() {
        if (!lock.isHeldByCurrentThread()) {
            throw new IllegalMonitorStateException();
        }
        for (;;) {
            WaitQueue.WaitNode w = wq.extract();
            if (w == null) return;  // no one to signal
            if (w.signal(sync)) return; // notify if still waiting, else skip
        }
    }

    public void signalAll() {
        if (!lock.isHeldByCurrentThread()) {
            throw new IllegalMonitorStateException();
        }
        for (;;) {
            WaitQueue.WaitNode w = wq.extract();
            if (w == null) return;  // no more to signal
            w.signal(sync);
        }
    }

    protected boolean hasWaiters() {
        if (!lock.isHeldByCurrentThread()) {
            throw new IllegalMonitorStateException();
        }
        return wq.hasNodes();
    }

    protected int getWaitQueueLength() {
        if (!lock.isHeldByCurrentThread()) {
            throw new IllegalMonitorStateException();
        }
        return wq.getLength();
    }

    protected Collection getWaitingThreads() {
        if (!lock.isHeldByCurrentThread()) {
            throw new IllegalMonitorStateException();
        }
        return wq.getWaitingThreads();
    }


}
