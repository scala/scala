/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package scala.concurrent.forkjoin;
import java.util.concurrent.*;

/**
 * A {@link BlockingQueue} in which producers may wait for consumers
 * to receive elements.  A {@code TransferQueue} may be useful for
 * example in message passing applications in which producers
 * sometimes (using method {@code transfer}) await receipt of
 * elements by consumers invoking {@code take} or {@code poll},
 * while at other times enqueue elements (via method {@code put})
 * without waiting for receipt. Non-blocking and time-out versions of
 * {@code tryTransfer} are also available.  A TransferQueue may also
 * be queried via {@code hasWaitingConsumer} whether there are any
 * threads waiting for items, which is a converse analogy to a
 * {@code peek} operation.
 *
 * <p>Like any {@code BlockingQueue}, a {@code TransferQueue} may be
 * capacity bounded. If so, an attempted {@code transfer} operation
 * may initially block waiting for available space, and/or
 * subsequently block waiting for reception by a consumer.  Note that
 * in a queue with zero capacity, such as {@link SynchronousQueue},
 * {@code put} and {@code transfer} are effectively synonymous.
 *
 * <p>This interface is a member of the
 * <a href="{@docRoot}/../technotes/guides/collections/index.html">
 * Java Collections Framework</a>.
 *
 * @since 1.7
 * @author Doug Lea
 * @param <E> the type of elements held in this collection
 */
public interface TransferQueue<E> extends BlockingQueue<E> {
    /**
     * Transfers the specified element if there exists a consumer
     * already waiting to receive it, otherwise returning {@code false}
     * without enqueuing the element.
     *
     * @param e the element to transfer
     * @return {@code true} if the element was transferred, else
     *         {@code false}
     * @throws ClassCastException if the class of the specified element
     *         prevents it from being added to this queue
     * @throws NullPointerException if the specified element is null
     * @throws IllegalArgumentException if some property of the specified
     *         element prevents it from being added to this queue
     */
    boolean tryTransfer(E e);

    /**
     * Inserts the specified element into this queue, waiting if
     * necessary for space to become available and the element to be
     * dequeued by a consumer invoking {@code take} or {@code poll}.
     *
     * @param e the element to transfer
     * @throws InterruptedException if interrupted while waiting,
     *         in which case the element is not enqueued.
     * @throws ClassCastException if the class of the specified element
     *         prevents it from being added to this queue
     * @throws NullPointerException if the specified element is null
     * @throws IllegalArgumentException if some property of the specified
     *         element prevents it from being added to this queue
     */
    void transfer(E e) throws InterruptedException;

    /**
     * Inserts the specified element into this queue, waiting up to
     * the specified wait time if necessary for space to become
     * available and the element to be dequeued by a consumer invoking
     * {@code take} or {@code poll}.
     *
     * @param e the element to transfer
     * @param timeout how long to wait before giving up, in units of
     *        {@code unit}
     * @param unit a {@code TimeUnit} determining how to interpret the
     *        {@code timeout} parameter
     * @return {@code true} if successful, or {@code false} if
     *         the specified waiting time elapses before completion,
     *         in which case the element is not enqueued.
     * @throws InterruptedException if interrupted while waiting,
     *         in which case the element is not enqueued.
     * @throws ClassCastException if the class of the specified element
     *         prevents it from being added to this queue
     * @throws NullPointerException if the specified element is null
     * @throws IllegalArgumentException if some property of the specified
     *         element prevents it from being added to this queue
     */
    boolean tryTransfer(E e, long timeout, TimeUnit unit)
        throws InterruptedException;

    /**
     * Returns {@code true} if there is at least one consumer waiting
     * to dequeue an element via {@code take} or {@code poll}.
     * The return value represents a momentary state of affairs.
     *
     * @return {@code true} if there is at least one waiting consumer
     */
    boolean hasWaitingConsumer();

    /**
     * Returns an estimate of the number of consumers waiting to
     * dequeue elements via {@code take} or {@code poll}. The return
     * value is an approximation of a momentary state of affairs, that
     * may be inaccurate if consumers have completed or given up
     * waiting. The value may be useful for monitoring and heuristics,
     * but not for synchronization control. Implementations of this
     * method are likely to be noticeably slower than those for
     * {@link #hasWaitingConsumer}.
     *
     * @return the number of consumers waiting to dequeue elements
     */
    int getWaitingConsumerCount();
}
