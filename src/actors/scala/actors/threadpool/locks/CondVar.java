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

class CondVar implements Condition, java.io.Serializable {
    private static final long serialVersionUID = -5009898475638427940L;

    /** The lock **/
    protected final ExclusiveLock lock;

    /**
     * Create a new CondVar that relies on the given mutual
     * exclusion lock.
     * @param lock A non-reentrant mutual exclusion lock.
     **/

    CondVar(ExclusiveLock lock) {
        this.lock = lock;
    }

    public void awaitUninterruptibly() {
        int holdCount = lock.getHoldCount();
        if (holdCount == 0) {
            throw new IllegalMonitorStateException();
        }
        // avoid instant spurious wakeup if thread already interrupted
        boolean wasInterrupted = Thread.interrupted();
        try {
            synchronized (this) {
                for (int i=holdCount; i>0; i--) lock.unlock();
                try {
                    wait();
                }
                catch (InterruptedException ex) {
                    wasInterrupted = true;
                    // may have masked the signal and there is no way
                    // to tell; we must wake up spuriously
                }
            }
        }
        finally {
            for (int i=holdCount; i>0; i--) lock.lock();
            if (wasInterrupted) {
                Thread.currentThread().interrupt();
            }
        }
    }

    public void await() throws InterruptedException {
        int holdCount = lock.getHoldCount();
        if (holdCount == 0) {
            throw new IllegalMonitorStateException();
        }
        if (Thread.interrupted()) throw new InterruptedException();
        try {
            synchronized (this) {
                for (int i=holdCount; i>0; i--) lock.unlock();
                try {
                    wait();
                }
                catch (InterruptedException ex) {
                    notify();
                    throw ex;
                }
            }
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
        boolean success = false;
        try {
            synchronized (this) {
                for (int i=holdCount; i>0; i--) lock.unlock();
                try {
                    if (nanos > 0) {
                        long start = Utils.nanoTime();
                        TimeUnit.NANOSECONDS.timedWait(this, nanos);
                        // DK: due to coarse-grained (millis) clock, it seems
                        // preferable to acknowledge timeout (success == false)
                        // when the equality holds (timing is exact)
                        success = Utils.nanoTime() - start < nanos;
                    }
                }
                catch (InterruptedException ex) {
                    notify();
                    throw ex;
                }
            }
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
        int holdCount = lock.getHoldCount();
        if (holdCount == 0) {
            throw new IllegalMonitorStateException();
        }
        long abstime = deadline.getTime();
        if (Thread.interrupted()) throw new InterruptedException();

        boolean success = false;
        try {
            synchronized (this) {
                for (int i=holdCount; i>0; i--) lock.unlock();
                try {
                    long start = System.currentTimeMillis();
                    long msecs = abstime - start;
                    if (msecs > 0) {
                        wait(msecs);
                        // DK: due to coarse-grained (millis) clock, it seems
                        // preferable to acknowledge timeout (success == false)
                        // when the equality holds (timing is exact)
                        success = System.currentTimeMillis() - start < msecs;
                    }
                }
                catch (InterruptedException ex) {
                    notify();
                    throw ex;
                }
            }
        }
        finally {
            for (int i=holdCount; i>0; i--) lock.lock();
        }
        return success;
    }

    public synchronized void signal() {
        if (!lock.isHeldByCurrentThread()) {
            throw new IllegalMonitorStateException();
        }
        notify();
    }

    public synchronized void signalAll() {
        if (!lock.isHeldByCurrentThread()) {
            throw new IllegalMonitorStateException();
        }
        notifyAll();
    }

    protected ExclusiveLock getLock() { return lock; }

    protected boolean hasWaiters() {
        throw new UnsupportedOperationException("Use FAIR version");
    }

    protected int getWaitQueueLength() {
        throw new UnsupportedOperationException("Use FAIR version");
    }

    protected Collection getWaitingThreads() {
        throw new UnsupportedOperationException("Use FAIR version");
    }

    static interface ExclusiveLock extends Lock {
        boolean isHeldByCurrentThread();
        int getHoldCount();
    }
}
