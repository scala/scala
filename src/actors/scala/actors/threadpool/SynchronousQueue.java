/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.threadpool;
import scala.actors.threadpool.locks.*;
//import edu.emory.mathcs.backport.java.util.*;
import java.util.Collection;
import java.util.Iterator;
import scala.actors.threadpool.helpers.Utils;
import java.util.NoSuchElementException;

/**
 * A {@linkplain BlockingQueue blocking queue} in which each insert
 * operation must wait for a corresponding remove operation by another
 * thread, and vice versa.  A synchronous queue does not have any
 * internal capacity, not even a capacity of one.  You cannot
 * <tt>peek</tt> at a synchronous queue because an element is only
 * present when you try to remove it; you cannot insert an element
 * (using any method) unless another thread is trying to remove it;
 * you cannot iterate as there is nothing to iterate.  The
 * <em>head</em> of the queue is the element that the first queued
 * inserting thread is trying to add to the queue; if there is no such
 * queued thread then no element is available for removal and
 * <tt>poll()</tt> will return <tt>null</tt>.  For purposes of other
 * <tt>Collection</tt> methods (for example <tt>contains</tt>), a
 * <tt>SynchronousQueue</tt> acts as an empty collection.  This queue
 * does not permit <tt>null</tt> elements.
 *
 * <p>Synchronous queues are similar to rendezvous channels used in
 * CSP and Ada. They are well suited for handoff designs, in which an
 * object running in one thread must sync up with an object running
 * in another thread in order to hand it some information, event, or
 * task.
 *
 * <p> This class supports an optional fairness policy for ordering
 * waiting producer and consumer threads.  By default, this ordering
 * is not guaranteed. However, a queue constructed with fairness set
 * to <tt>true</tt> grants threads access in FIFO order. Fairness
 * generally decreases throughput but reduces variability and avoids
 * starvation.
 *
 * <p>This class and its iterator implement all of the
 * <em>optional</em> methods of the {@link Collection} and {@link
 * Iterator} interfaces.
 *
 * <p>This class is a member of the
 * <a href="{@docRoot}/../technotes/guides/collections/index.html">
 * Java Collections Framework</a>.
 *
 * @since 1.5
 * @author Doug Lea
 */
public class SynchronousQueue extends AbstractQueue
        implements BlockingQueue, java.io.Serializable {
    private static final long serialVersionUID = -3223113410248163686L;

    /*
      This implementation divides actions into two cases for puts:

      * An arriving producer that does not already have a waiting consumer
        creates a node holding item, and then waits for a consumer to take it.
      * An arriving producer that does already have a waiting consumer fills
        the slot node created by the consumer, and notifies it to continue.

      And symmetrically, two for takes:

      * An arriving consumer that does not already have a waiting producer
        creates an empty slot node, and then waits for a producer to fill it.
      * An arriving consumer that does already have a waiting producer takes
        item from the node created by the producer, and notifies it to continue.

      When a put or take waiting for the actions of its counterpart
      aborts due to interruption or timeout, it marks the node
      it created as "CANCELLED", which causes its counterpart to retry
      the entire put or take sequence.

      This requires keeping two simple queues, waitingProducers and
      waitingConsumers. Each of these can be FIFO (preserves fairness)
      or LIFO (improves throughput).
    */

    /** Lock protecting both wait queues */
    private final ReentrantLock qlock;
    /** Queue holding waiting puts */
    private final WaitQueue waitingProducers;
    /** Queue holding waiting takes */
    private final WaitQueue waitingConsumers;

    /**
     * Creates a <tt>SynchronousQueue</tt> with nonfair access policy.
     */
    public SynchronousQueue() {
        this(false);
    }

    /**
     * Creates a <tt>SynchronousQueue</tt> with specified fairness policy.
     * @param fair if true, threads contend in FIFO order for access;
     * otherwise the order is unspecified.
     */
    public SynchronousQueue(boolean fair) {
        if (fair) {
            qlock = new ReentrantLock(true);
            waitingProducers = new FifoWaitQueue();
            waitingConsumers = new FifoWaitQueue();
        }
        else {
            qlock = new ReentrantLock();
            waitingProducers = new LifoWaitQueue();
            waitingConsumers = new LifoWaitQueue();
        }
    }

    /**
     * Queue to hold waiting puts/takes; specialized to Fifo/Lifo below.
     * These queues have all transient fields, but are serializable
     * in order to recover fairness settings when deserialized.
     */
    static abstract class WaitQueue implements java.io.Serializable {
        /** Creates, adds, and returns node for x. */
        abstract Node enq(Object x);
        /** Removes and returns node, or null if empty. */
        abstract Node deq();
        /** Removes a cancelled node to avoid garbage retention. */
        abstract void unlink(Node node);
        /** Returns true if a cancelled node might be on queue. */
        abstract boolean shouldUnlink(Node node);
    }

    /**
     * FIFO queue to hold waiting puts/takes.
     */
    static final class FifoWaitQueue extends WaitQueue implements java.io.Serializable {
        private static final long serialVersionUID = -3623113410248163686L;
        private transient Node head;
        private transient Node last;

        Node enq(Object x) {
            Node p = new Node(x);
            if (last == null)
                last = head = p;
            else
                last = last.next = p;
            return p;
        }

        Node deq() {
            Node p = head;
            if (p != null) {
                if ((head = p.next) == null)
                    last = null;
                p.next = null;
            }
            return p;
        }

        boolean shouldUnlink(Node node) {
            return (node == last || node.next != null);
        }

        void unlink(Node node) {
            Node p = head;
            Node trail = null;
            while (p != null) {
                if (p == node) {
                    Node next = p.next;
                    if (trail == null)
                        head = next;
                    else
                        trail.next = next;
                    if (last == node)
                        last = trail;
                    break;
                }
                trail = p;
                p = p.next;
            }
        }
    }

    /**
     * LIFO queue to hold waiting puts/takes.
     */
    static final class LifoWaitQueue extends WaitQueue implements java.io.Serializable {
        private static final long serialVersionUID = -3633113410248163686L;
        private transient Node head;

        Node enq(Object x) {
            return head = new Node(x, head);
        }

        Node deq() {
            Node p = head;
            if (p != null) {
                head = p.next;
                p.next = null;
            }
            return p;
        }

        boolean shouldUnlink(Node node) {
            // Return false if already dequeued or is bottom node (in which
            // case we might retain at most one garbage node)
            return (node == head || node.next != null);
        }

        void unlink(Node node) {
            Node p = head;
            Node trail = null;
            while (p != null) {
                if (p == node) {
                    Node next = p.next;
                    if (trail == null)
                        head = next;
                    else
                        trail.next = next;
                    break;
                }
                trail = p;
                p = p.next;
            }
        }
    }

    /**
     * Unlinks the given node from consumer queue.  Called by cancelled
     * (timeout, interrupt) waiters to avoid garbage retention in the
     * absence of producers.
     */
    private void unlinkCancelledConsumer(Node node) {
        // Use a form of double-check to avoid unnecessary locking and
        // traversal. The first check outside lock might
        // conservatively report true.
        if (waitingConsumers.shouldUnlink(node)) {
            qlock.lock();
            try {
                if (waitingConsumers.shouldUnlink(node))
                    waitingConsumers.unlink(node);
            } finally {
                qlock.unlock();
            }
        }
    }

    /**
     * Unlinks the given node from producer queue.  Symmetric
     * to unlinkCancelledConsumer.
     */
    private void unlinkCancelledProducer(Node node) {
        if (waitingProducers.shouldUnlink(node)) {
            qlock.lock();
            try {
                if (waitingProducers.shouldUnlink(node))
                    waitingProducers.unlink(node);
            } finally {
                qlock.unlock();
            }
        }
    }

    /**
     * Nodes each maintain an item and handle waits and signals for
     * getting and setting it. The class extends
     * AbstractQueuedSynchronizer to manage blocking, using AQS state
     *  0 for waiting, 1 for ack, -1 for cancelled.
     */
    static final class Node implements java.io.Serializable {
        private static final long serialVersionUID = -3223113410248163686L;

        /** Synchronization state value representing that node acked */
        private static final int ACK    =  1;
        /** Synchronization state value representing that node cancelled */
        private static final int CANCEL = -1;

        int state = 0;

        /** The item being transferred */
        Object item;
        /** Next node in wait queue */
        Node next;

        /** Creates a node with initial item */
        Node(Object x) { item = x; }

        /** Creates a node with initial item and next */
        Node(Object x, Node n) { item = x; next = n; }

        /**
         * Takes item and nulls out field (for sake of GC)
         *
         * PRE: lock owned
         */
        private Object extract() {
            Object x = item;
            item = null;
            return x;
        }

        /**
         * Tries to cancel on interrupt; if so rethrowing,
         * else setting interrupt state
         *
         * PRE: lock owned
         */
        private void checkCancellationOnInterrupt(InterruptedException ie)
            throws InterruptedException
        {
            if (state == 0) {
                state = CANCEL;
                notify();
                throw ie;
            }
            Thread.currentThread().interrupt();
        }

        /**
         * Fills in the slot created by the consumer and signal consumer to
         * continue.
         */
        synchronized boolean setItem(Object x) {
            if (state != 0) return false;
            item = x;
            state = ACK;
            notify();
            return true;
        }

        /**
         * Removes item from slot created by producer and signal producer
         * to continue.
         */
        synchronized Object getItem() {
            if (state != 0) return null;
            state = ACK;
            notify();
            return extract();
        }

        /**
         * Waits for a consumer to take item placed by producer.
         */
        synchronized void waitForTake() throws InterruptedException {
            try {
                while (state == 0) wait();
            } catch (InterruptedException ie) {
                checkCancellationOnInterrupt(ie);
            }
        }

        /**
         * Waits for a producer to put item placed by consumer.
         */
        synchronized Object waitForPut() throws InterruptedException {
            try {
                while (state == 0) wait();
            } catch (InterruptedException ie) {
                checkCancellationOnInterrupt(ie);
            }
            return extract();
        }

        private boolean attempt(long nanos) throws InterruptedException {
            if (state != 0) return true;
            if (nanos <= 0) {
                state = CANCEL;
                notify();
                return false;
            }
            long deadline = Utils.nanoTime() + nanos;
            while (true) {
                TimeUnit.NANOSECONDS.timedWait(this, nanos);
                if (state != 0) return true;
                nanos = deadline - Utils.nanoTime();
                if (nanos <= 0) {
                    state = CANCEL;
                    notify();
                    return false;
                }
            }
        }

        /**
         * Waits for a consumer to take item placed by producer or time out.
         */
        synchronized boolean waitForTake(long nanos) throws InterruptedException {
            try {
                if (!attempt(nanos)) return false;
            } catch (InterruptedException ie) {
                checkCancellationOnInterrupt(ie);
            }
            return true;
        }

        /**
         * Waits for a producer to put item placed by consumer, or time out.
         */
        synchronized Object waitForPut(long nanos) throws InterruptedException {
            try {
                if (!attempt(nanos)) return null;
            } catch (InterruptedException ie) {
                checkCancellationOnInterrupt(ie);
            }
            return extract();
        }
    }

    /**
     * Adds the specified element to this queue, waiting if necessary for
     * another thread to receive it.
     *
     * @throws InterruptedException {@inheritDoc}
     * @throws NullPointerException {@inheritDoc}
     */
    public void put(Object e) throws InterruptedException {
        if (e == null) throw new NullPointerException();
        final ReentrantLock qlock = this.qlock;

        for (;;) {
            Node node;
            boolean mustWait;
            if (Thread.interrupted()) throw new InterruptedException();
            qlock.lock();
            try {
                node = waitingConsumers.deq();
                if ( (mustWait = (node == null)) )
                    node = waitingProducers.enq(e);
            } finally {
                qlock.unlock();
            }

            if (mustWait) {
                try {
                    node.waitForTake();
                    return;
                } catch (InterruptedException ex) {
                    unlinkCancelledProducer(node);
                    throw ex;
                }
            }

            else if (node.setItem(e))
                return;

            // else consumer cancelled, so retry
        }
    }

    /**
     * Inserts the specified element into this queue, waiting if necessary
     * up to the specified wait time for another thread to receive it.
     *
     * @return <tt>true</tt> if successful, or <tt>false</tt> if the
     *         specified waiting time elapses before a consumer appears.
     * @throws InterruptedException {@inheritDoc}
     * @throws NullPointerException {@inheritDoc}
     */
    public boolean offer(Object e, long timeout, TimeUnit unit) throws InterruptedException {
        if (e == null) throw new NullPointerException();
        long nanos = unit.toNanos(timeout);
        final ReentrantLock qlock = this.qlock;
        for (;;) {
            Node node;
            boolean mustWait;
            if (Thread.interrupted()) throw new InterruptedException();
            qlock.lock();
            try {
                node = waitingConsumers.deq();
                if ( (mustWait = (node == null)) )
                    node = waitingProducers.enq(e);
            } finally {
                qlock.unlock();
            }

            if (mustWait) {
                try {
                    boolean x = node.waitForTake(nanos);
                    if (!x)
                        unlinkCancelledProducer(node);
                    return x;
                } catch (InterruptedException ex) {
                    unlinkCancelledProducer(node);
                    throw ex;
                }
            }

            else if (node.setItem(e))
                return true;

            // else consumer cancelled, so retry
        }
    }

    /**
     * Retrieves and removes the head of this queue, waiting if necessary
     * for another thread to insert it.
     *
     * @return the head of this queue
     * @throws InterruptedException {@inheritDoc}
     */
    public Object take() throws InterruptedException {
        final ReentrantLock qlock = this.qlock;
        for (;;) {
            Node node;
            boolean mustWait;

            if (Thread.interrupted()) throw new InterruptedException();
            qlock.lock();
            try {
                node = waitingProducers.deq();
                if ( (mustWait = (node == null)) )
                    node = waitingConsumers.enq(null);
            } finally {
                qlock.unlock();
            }

            if (mustWait) {
                try {
                    Object x = node.waitForPut();
                    return (Object)x;
                } catch (InterruptedException ex) {
                    unlinkCancelledConsumer(node);
                    throw ex;
                }
            }
            else {
                Object x = node.getItem();
                if (x != null)
                    return (Object)x;
                // else cancelled, so retry
            }
        }
    }

    /**
     * Retrieves and removes the head of this queue, waiting
     * if necessary up to the specified wait time, for another thread
     * to insert it.
     *
     * @return the head of this queue, or <tt>null</tt> if the
     *         specified waiting time elapses before an element is present.
     * @throws InterruptedException {@inheritDoc}
     */
    public Object poll(long timeout, TimeUnit unit) throws InterruptedException {
        long nanos = unit.toNanos(timeout);
        final ReentrantLock qlock = this.qlock;

        for (;;) {
            Node node;
            boolean mustWait;

            if (Thread.interrupted()) throw new InterruptedException();
            qlock.lock();
            try {
                node = waitingProducers.deq();
                if ( (mustWait = (node == null)) )
                    node = waitingConsumers.enq(null);
            } finally {
                qlock.unlock();
            }

            if (mustWait) {
                try {
                    Object x = node.waitForPut(nanos);
                    if (x == null)
                        unlinkCancelledConsumer(node);
                    return (Object)x;
                } catch (InterruptedException ex) {
                    unlinkCancelledConsumer(node);
                    throw ex;
                }
            }
            else {
                Object x = node.getItem();
                if (x != null)
                    return (Object)x;
                // else cancelled, so retry
            }
        }
    }

    // Untimed nonblocking versions

    /**
     * Inserts the specified element into this queue, if another thread is
     * waiting to receive it.
     *
     * @param e the element to add
     * @return <tt>true</tt> if the element was added to this queue, else
     *         <tt>false</tt>
     * @throws NullPointerException if the specified element is null
     */
    public boolean offer(Object e) {
        if (e == null) throw new NullPointerException();
        final ReentrantLock qlock = this.qlock;

        for (;;) {
            Node node;
            qlock.lock();
            try {
                node = waitingConsumers.deq();
            } finally {
                qlock.unlock();
            }
            if (node == null)
                return false;

            else if (node.setItem(e))
                return true;
            // else retry
        }
    }

    /**
     * Retrieves and removes the head of this queue, if another thread
     * is currently making an element available.
     *
     * @return the head of this queue, or <tt>null</tt> if no
     *         element is available.
     */
    public Object poll() {
        final ReentrantLock qlock = this.qlock;
        for (;;) {
            Node node;
            qlock.lock();
            try {
                node = waitingProducers.deq();
            } finally {
                qlock.unlock();
            }
            if (node == null)
                return null;

            else {
                Object x = node.getItem();
                if (x != null)
                    return (Object)x;
                // else retry
            }
        }
    }

    /**
     * Always returns <tt>true</tt>.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @return <tt>true</tt>
     */
    public boolean isEmpty() {
        return true;
    }

    /**
     * Always returns zero.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @return zero
     */
    public int size() {
        return 0;
    }

    /**
     * Always returns zero.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @return zero
     */
    public int remainingCapacity() {
        return 0;
    }

    /**
     * Does nothing.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     */
    public void clear() {}

    /**
     * Always returns <tt>false</tt>.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @param o object to be checked for containment in this queue
     * @return <tt>false</tt>
     */
    public boolean contains(Object o) {
        return false;
    }

    /**
     * Always returns <tt>false</tt>.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @param o the element to remove
     * @return <tt>false</tt>
     */
    public boolean remove(Object o) {
        return false;
    }

    /**
     * Returns <tt>false</tt> unless the given collection is empty.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @param c the collection
     * @return <tt>false</tt> unless the given collection is empty
     * @throws NullPointerException if the specified collection is null
     */
    public boolean containsAll(Collection c) {
        return c.isEmpty();
    }

    /**
     * Always returns <tt>false</tt>.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @param c the collection
     * @return <tt>false</tt>
     */
    public boolean removeAll(Collection c) {
        return false;
    }

    /**
     * Always returns <tt>false</tt>.
     * A <tt>SynchronousQueue</tt> has no internal capacity.
     *
     * @param c the collection
     * @return <tt>false</tt>
     */
    public boolean retainAll(Collection c) {
        return false;
    }

    /**
     * Always returns <tt>null</tt>.
     * A <tt>SynchronousQueue</tt> does not return elements
     * unless actively waited on.
     *
     * @return <tt>null</tt>
     */
    public Object peek() {
        return null;
    }


    static class EmptyIterator implements Iterator {
        public boolean hasNext() {
            return false;
        }
        public Object next() {
            throw new NoSuchElementException();
        }
        public void remove() {
            throw new IllegalStateException();
        }
    }

    /**
     * Returns an empty iterator in which <tt>hasNext</tt> always returns
     * <tt>false</tt>.
     *
     * @return an empty iterator
     */
    public Iterator iterator() {
        return new EmptyIterator();
    }


    /**
     * Returns a zero-length array.
     * @return a zero-length array
     */
    public Object[] toArray() {
        return new Object[0];
    }

    /**
     * Sets the zeroeth element of the specified array to <tt>null</tt>
     * (if the array has non-zero length) and returns it.
     *
     * @param a the array
     * @return the specified array
     * @throws NullPointerException if the specified array is null
     */
    public Object[] toArray(Object[] a) {
        if (a.length > 0)
            a[0] = null;
        return a;
    }

    /**
     * @throws UnsupportedOperationException {@inheritDoc}
     * @throws ClassCastException            {@inheritDoc}
     * @throws NullPointerException          {@inheritDoc}
     * @throws IllegalArgumentException      {@inheritDoc}
     */
    public int drainTo(Collection c) {
        if (c == null)
            throw new NullPointerException();
        if (c == this)
            throw new IllegalArgumentException();
        int n = 0;
        Object e;
        while ( (e = poll()) != null) {
            c.add(e);
            ++n;
        }
        return n;
    }

    /**
     * @throws UnsupportedOperationException {@inheritDoc}
     * @throws ClassCastException            {@inheritDoc}
     * @throws NullPointerException          {@inheritDoc}
     * @throws IllegalArgumentException      {@inheritDoc}
     */
    public int drainTo(Collection c, int maxElements) {
        if (c == null)
            throw new NullPointerException();
        if (c == this)
            throw new IllegalArgumentException();
        int n = 0;
        Object e;
        while (n < maxElements && (e = poll()) != null) {
            c.add(e);
            ++n;
        }
        return n;
    }
}
