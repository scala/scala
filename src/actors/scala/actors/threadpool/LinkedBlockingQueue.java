/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.threadpool;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
//import edu.emory.mathcs.backport.java.util.*;
import scala.actors.threadpool.helpers.*;

/**
 * An optionally-bounded {@linkplain BlockingQueue blocking queue} based on
 * linked nodes.
 * This queue orders elements FIFO (first-in-first-out).
 * The <em>head</em> of the queue is that element that has been on the
 * queue the longest time.
 * The <em>tail</em> of the queue is that element that has been on the
 * queue the shortest time. New elements
 * are inserted at the tail of the queue, and the queue retrieval
 * operations obtain elements at the head of the queue.
 * Linked queues typically have higher throughput than array-based queues but
 * less predictable performance in most concurrent applications.
 *
 * <p> The optional capacity bound constructor argument serves as a
 * way to prevent excessive queue expansion. The capacity, if unspecified,
 * is equal to {@link Integer#MAX_VALUE}.  Linked nodes are
 * dynamically created upon each insertion unless this would bring the
 * queue above capacity.
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
 *
 */
public class LinkedBlockingQueue extends AbstractQueue
        implements BlockingQueue, java.io.Serializable {
    private static final long serialVersionUID = -6903933977591709194L;

    /*
     * A variant of the "two lock queue" algorithm.  The putLock gates
     * entry to put (and offer), and has an associated condition for
     * waiting puts.  Similarly for the takeLock.  The "count" field
     * that they both rely on is maintained as an atomic to avoid
     * needing to get both locks in most cases. Also, to minimize need
     * for puts to get takeLock and vice-versa, cascading notifies are
     * used. When a put notices that it has enabled at least one take,
     * it signals taker. That taker in turn signals others if more
     * items have been entered since the signal. And symmetrically for
     * takes signalling puts. Operations such as remove(Object) and
     * iterators acquire both locks.
     */

    /**
     * Linked list node class
     */
    static class Node {
        /** The item, volatile to ensure barrier separating write and read */
        volatile Object item;
        Node next;
        Node(Object x) { item = x; }
    }

    /** The capacity bound, or Integer.MAX_VALUE if none */
    private final int capacity;

    /** Current number of elements */
    private volatile int count = 0;

    /** Head of linked list */
    private transient Node head;

    /** Tail of linked list */
    private transient Node last;

    /** Lock held by take, poll, etc */
    private final Object takeLock = new SerializableLock();

    /** Lock held by put, offer, etc */
    private final Object putLock = new SerializableLock();

    /**
     * Signals a waiting take. Called only from put/offer (which do not
     * otherwise ordinarily lock takeLock.)
     */
    private void signalNotEmpty() {
        synchronized (takeLock) {
            takeLock.notify();
        }
    }

    /**
     * Signals a waiting put. Called only from take/poll.
     */
    private void signalNotFull() {
        synchronized (putLock) {
            putLock.notify();
        }
    }

    /**
     * Creates a node and links it at end of queue.
     * @param x the item
     */
    private void insert(Object x) {
        last = last.next = new Node(x);
    }

    /**
     * Removes a node from head of queue,
     * @return the node
     */
    private Object extract() {
        Node first = head.next;
        head = first;
        Object x = first.item;
        first.item = null;
        return x;
    }


    /**
     * Creates a <tt>LinkedBlockingQueue</tt> with a capacity of
     * {@link Integer#MAX_VALUE}.
     */
    public LinkedBlockingQueue() {
        this(Integer.MAX_VALUE);
    }

    /**
     * Creates a <tt>LinkedBlockingQueue</tt> with the given (fixed) capacity.
     *
     * @param capacity the capacity of this queue
     * @throws IllegalArgumentException if <tt>capacity</tt> is not greater
     *         than zero
     */
    public LinkedBlockingQueue(int capacity) {
        if (capacity <= 0) throw new IllegalArgumentException();
        this.capacity = capacity;
        last = head = new Node(null);
    }

    /**
     * Creates a <tt>LinkedBlockingQueue</tt> with a capacity of
     * {@link Integer#MAX_VALUE}, initially containing the elements of the
     * given collection,
     * added in traversal order of the collection's iterator.
     *
     * @param c the collection of elements to initially contain
     * @throws NullPointerException if the specified collection or any
     *         of its elements are null
     */
    public LinkedBlockingQueue(Collection c) {
        this(Integer.MAX_VALUE);
        for (Iterator itr = c.iterator(); itr.hasNext();) {
            Object e = itr.next();
            add(e);
        }
    }


    // this doc comment is overridden to remove the reference to collections
    // greater in size than Integer.MAX_VALUE
    /**
     * Returns the number of elements in this queue.
     *
     * @return the number of elements in this queue
     */
    public int size() {
        return count;
    }

    // this doc comment is a modified copy of the inherited doc comment,
    // without the reference to unlimited queues.
    /**
     * Returns the number of additional elements that this queue can ideally
     * (in the absence of memory or resource constraints) accept without
     * blocking. This is always equal to the initial capacity of this queue
     * less the current <tt>size</tt> of this queue.
     *
     * <p>Note that you <em>cannot</em> always tell if an attempt to insert
     * an element will succeed by inspecting <tt>remainingCapacity</tt>
     * because it may be the case that another thread is about to
     * insert or remove an element.
     */
    public int remainingCapacity() {
        return capacity - count;
    }

    /**
     * Inserts the specified element at the tail of this queue, waiting if
     * necessary for space to become available.
     *
     * @throws InterruptedException {@inheritDoc}
     * @throws NullPointerException {@inheritDoc}
     */
    public void put(Object e) throws InterruptedException {
        if (e == null) throw new NullPointerException();
        // Note: convention in all put/take/etc is to preset
        // local var holding count  negative to indicate failure unless set.
        int c = -1;
        synchronized (putLock) {
            /*
             * Note that count is used in wait guard even though it is
             * not protected by lock. This works because count can
             * only decrease at this point (all other puts are shut
             * out by lock), and we (or some other waiting put) are
             * signalled if it ever changes from
             * capacity. Similarly for all other uses of count in
             * other wait guards.
             */
            try {
                while (count == capacity)
                    putLock.wait();
            } catch (InterruptedException ie) {
                putLock.notify(); // propagate to a non-interrupted thread
                throw ie;
            }
            insert(e);
            synchronized (this) { c = count++; }
            if (c + 1 < capacity)
                putLock.notify();
        }

        if (c == 0)
            signalNotEmpty();
    }

    /**
     * Inserts the specified element at the tail of this queue, waiting if
     * necessary up to the specified wait time for space to become available.
     *
     * @return <tt>true</tt> if successful, or <tt>false</tt> if
     *         the specified waiting time elapses before space is available.
     * @throws InterruptedException {@inheritDoc}
     * @throws NullPointerException {@inheritDoc}
     */
    public boolean offer(Object e, long timeout, TimeUnit unit)
        throws InterruptedException {

        if (e == null) throw new NullPointerException();
        long nanos = unit.toNanos(timeout);
        int c = -1;
        synchronized (putLock) {
            long deadline = Utils.nanoTime() + nanos;
            for (;;) {
                if (count < capacity) {
                    insert(e);
                    synchronized (this) { c = count++; }
                    if (c + 1 < capacity)
                        putLock.notify();
                    break;
                }
                if (nanos <= 0)
                    return false;
                try {
                    TimeUnit.NANOSECONDS.timedWait(putLock, nanos);
                    nanos = deadline - Utils.nanoTime();
                } catch (InterruptedException ie) {
                    putLock.notify(); // propagate to a non-interrupted thread
                    throw ie;
                }
            }
        }
        if (c == 0)
            signalNotEmpty();
        return true;
    }

    /**
     * Inserts the specified element at the tail of this queue if it is
     * possible to do so immediately without exceeding the queue's capacity,
     * returning <tt>true</tt> upon success and <tt>false</tt> if this queue
     * is full.
     * When using a capacity-restricted queue, this method is generally
     * preferable to method {@link BlockingQueue#add add}, which can fail to
     * insert an element only by throwing an exception.
     *
     * @throws NullPointerException if the specified element is null
     */
    public boolean offer(Object e) {
        if (e == null) throw new NullPointerException();
        if (count == capacity)
            return false;
        int c = -1;
        synchronized (putLock) {
            if (count < capacity) {
                insert(e);
                synchronized (this) { c = count++; }
                if (c + 1 < capacity)
                    putLock.notify();
            }
        }
        if (c == 0)
            signalNotEmpty();
        return c >= 0;
    }


    public Object take() throws InterruptedException {
        Object x;
        int c = -1;
        synchronized (takeLock) {
            try {
                while (count == 0)
                    takeLock.wait();
            } catch (InterruptedException ie) {
                takeLock.notify(); // propagate to a non-interrupted thread
                throw ie;
            }

            x = extract();
            synchronized (this) { c = count--; }
            if (c > 1)
                takeLock.notify();
        }
        if (c == capacity)
            signalNotFull();
        return x;
    }

    public Object poll(long timeout, TimeUnit unit) throws InterruptedException {
        Object x = null;
        int c = -1;
        long nanos = unit.toNanos(timeout);
        synchronized (takeLock) {
            long deadline = Utils.nanoTime() + nanos;
            for (;;) {
                if (count > 0) {
                    x = extract();
                    synchronized (this) { c = count--; }
                    if (c > 1)
                        takeLock.notify();
                    break;
                }
                if (nanos <= 0)
                    return null;
                try {
                    TimeUnit.NANOSECONDS.timedWait(takeLock, nanos);
                    nanos = deadline - Utils.nanoTime();
                } catch (InterruptedException ie) {
                    takeLock.notify(); // propagate to a non-interrupted thread
                    throw ie;
                }
            }
        }
        if (c == capacity)
            signalNotFull();
        return x;
    }

    public Object poll() {
        if (count == 0)
            return null;
        Object x = null;
        int c = -1;
        synchronized (takeLock) {
            if (count > 0) {
                x = extract();
                synchronized (this) { c = count--; }
                if (c > 1)
                    takeLock.notify();
            }
        }
        if (c == capacity)
            signalNotFull();
        return x;
    }


    public Object peek() {
        if (count == 0)
            return null;
        synchronized (takeLock) {
            Node first = head.next;
            if (first == null)
                return null;
            else
                return first.item;
        }
    }

    /**
     * Removes a single instance of the specified element from this queue,
     * if it is present.  More formally, removes an element <tt>e</tt> such
     * that <tt>o.equals(e)</tt>, if this queue contains one or more such
     * elements.
     * Returns <tt>true</tt> if this queue contained the specified element
     * (or equivalently, if this queue changed as a result of the call).
     *
     * @param o element to be removed from this queue, if present
     * @return <tt>true</tt> if this queue changed as a result of the call
     */
    public boolean remove(Object o) {
        if (o == null) return false;
        boolean removed = false;
        synchronized (putLock) {
            synchronized (takeLock) {
                Node trail = head;
                Node p = head.next;
                while (p != null) {
                    if (o.equals(p.item)) {
                        removed = true;
                        break;
                    }
                    trail = p;
                    p = p.next;
                }
                if (removed) {
                    p.item = null;
                    trail.next = p.next;
                    if (last == p)
                        last = trail;
                    synchronized (this) {
                        if (count-- == capacity)
                            putLock.notifyAll();
                    }
                }
            }
        }
        return removed;
    }

    /**
     * Returns an array containing all of the elements in this queue, in
     * proper sequence.
     *
     * <p>The returned array will be "safe" in that no references to it are
     * maintained by this queue.  (In other words, this method must allocate
     * a new array).  The caller is thus free to modify the returned array.
     *
     * <p>This method acts as bridge between array-based and collection-based
     * APIs.
     *
     * @return an array containing all of the elements in this queue
     */
    public Object[] toArray() {
        synchronized (putLock) {
            synchronized (takeLock) {
                int size = count;
                Object[] a = new Object[size];
                int k = 0;
                for (Node p = head.next; p != null; p = p.next)
                    a[k++] = p.item;
                return a;
            }
        }
    }

    /**
     * Returns an array containing all of the elements in this queue, in
     * proper sequence; the runtime type of the returned array is that of
     * the specified array.  If the queue fits in the specified array, it
     * is returned therein.  Otherwise, a new array is allocated with the
     * runtime type of the specified array and the size of this queue.
     *
     * <p>If this queue fits in the specified array with room to spare
     * (i.e., the array has more elements than this queue), the element in
     * the array immediately following the end of the queue is set to
     * <tt>null</tt>.
     *
     * <p>Like the {@link #toArray()} method, this method acts as bridge between
     * array-based and collection-based APIs.  Further, this method allows
     * precise control over the runtime type of the output array, and may,
     * under certain circumstances, be used to save allocation costs.
     *
     * <p>Suppose <tt>x</tt> is a queue known to contain only strings.
     * The following code can be used to dump the queue into a newly
     * allocated array of <tt>String</tt>:
     *
     * <pre>
     *     String[] y = x.toArray(new String[0]);</pre>
     *
     * Note that <tt>toArray(new Object[0])</tt> is identical in function to
     * <tt>toArray()</tt>.
     *
     * @param a the array into which the elements of the queue are to
     *          be stored, if it is big enough; otherwise, a new array of the
     *          same runtime type is allocated for this purpose
     * @return an array containing all of the elements in this queue
     * @throws ArrayStoreException if the runtime type of the specified array
     *         is not a supertype of the runtime type of every element in
     *         this queue
     * @throws NullPointerException if the specified array is null
     */
    public Object[] toArray(Object[] a) {
        synchronized (putLock) {
            synchronized (takeLock) {
                int size = count;
                if (a.length < size)
                    a = (Object[])java.lang.reflect.Array.newInstance
                        (a.getClass().getComponentType(), size);

                int k = 0;
                for (Node p = head.next; p != null; p = p.next)
                    a[k++] = (Object)p.item;
                if (a.length > k)
                    a[k] = null;
                return a;
            }
        }
    }

    public String toString() {
        synchronized (putLock) {
            synchronized (takeLock) {
                return super.toString();
            }
        }
    }

    /**
     * Atomically removes all of the elements from this queue.
     * The queue will be empty after this call returns.
     */
    public void clear() {
        synchronized (putLock) {
            synchronized (takeLock) {
                head.next = null;
                assert head.item == null;
                last = head;
                int c;
                synchronized (this) {
                    c = count;
                    count = 0;
                }
                if (c == capacity)
                    putLock.notifyAll();
            }
        }
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
        Node first;
        synchronized (putLock) {
            synchronized (takeLock) {
                first = head.next;
                head.next = null;
                assert head.item == null;
                last = head;
                int cold;
                synchronized (this) {
                    cold = count;
                    count = 0;
                }
                if (cold == capacity)
                    putLock.notifyAll();
            }
        }
        // Transfer the elements outside of locks
        int n = 0;
        for (Node p = first; p != null; p = p.next) {
            c.add(p.item);
            p.item = null;
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
        synchronized (putLock) {
            synchronized (takeLock) {
                int n = 0;
                Node p = head.next;
                while (p != null && n < maxElements) {
                    c.add(p.item);
                    p.item = null;
                    p = p.next;
                    ++n;
                }
                if (n != 0) {
                    head.next = p;
                    assert head.item == null;
                    if (p == null)
                        last = head;
                    int cold;
                    synchronized (this) {
                        cold = count;
                        count -= n;
                    }
                    if (cold == capacity)
                        putLock.notifyAll();
                }
                return n;
            }
        }
    }

    /**
     * Returns an iterator over the elements in this queue in proper sequence.
     * The returned <tt>Iterator</tt> is a "weakly consistent" iterator that
     * will never throw {@link java.util.ConcurrentModificationException},
     * and guarantees to traverse elements as they existed upon
     * construction of the iterator, and may (but is not guaranteed to)
     * reflect any modifications subsequent to construction.
     *
     * @return an iterator over the elements in this queue in proper sequence
     */
    public Iterator iterator() {
      return new Itr();
    }

    private class Itr implements Iterator {
        /*
         * Basic weak-consistent iterator.  At all times hold the next
         * item to hand out so that if hasNext() reports true, we will
         * still have it to return even if lost race with a take etc.
         */
        private Node current;
        private Node lastRet;
        private Object currentElement;

        Itr() {
            synchronized (putLock) {
                synchronized (takeLock) {
                    current = head.next;
                    if (current != null)
                        currentElement = current.item;
                }
            }
        }

        public boolean hasNext() {
            return current != null;
        }

        public Object next() {
            synchronized (putLock) {
                synchronized (takeLock) {
                    if (current == null)
                        throw new NoSuchElementException();
                    Object x = currentElement;
                    lastRet = current;
                    current = current.next;
                    if (current != null)
                        currentElement = current.item;
                    return x;
                }
            }
        }

        public void remove() {
            if (lastRet == null)
                throw new IllegalStateException();
            synchronized (putLock) {
                synchronized (takeLock) {
                    Node node = lastRet;
                    lastRet = null;
                    Node trail = head;
                    Node p = head.next;
                    while (p != null && p != node) {
                        trail = p;
                        p = p.next;
                    }
                    if (p == node) {
                        p.item = null;
                        trail.next = p.next;
                        if (last == p)
                            last = trail;
                        int c;
                        synchronized (this) { c = count--; }
                        if (c == capacity)
                            putLock.notifyAll();
                    }
                }
            }
        }
    }

    /**
     * Save the state to a stream (that is, serialize it).
     *
     * @serialData The capacity is emitted (int), followed by all of
     * its elements (each an <tt>Object</tt>) in the proper order,
     * followed by a null
     * @param s the stream
     */
    private void writeObject(java.io.ObjectOutputStream s)
        throws java.io.IOException {

        synchronized (putLock) {
            synchronized (takeLock) {
                // Write out any hidden stuff, plus capacity
                s.defaultWriteObject();

                // Write out all elements in the proper order.
                for (Node p = head.next; p != null; p = p.next)
                    s.writeObject(p.item);

                // Use trailing null as sentinel
                s.writeObject(null);
            }
        }
    }

    /**
     * Reconstitute this queue instance from a stream (that is,
     * deserialize it).
     * @param s the stream
     */
    private void readObject(java.io.ObjectInputStream s)
        throws java.io.IOException, ClassNotFoundException {
        // Read in capacity, and any hidden stuff
        s.defaultReadObject();

        synchronized (this) { count = 0; }
        last = head = new Node(null);

        // Read in all elements and place in queue
        for (;;) {
            Object item = (Object)s.readObject();
            if (item == null)
                break;
            add(item);
        }
    }

    private static class SerializableLock implements java.io.Serializable {
        private final static long serialVersionUID = -8856990691138858668L;
    }
}
