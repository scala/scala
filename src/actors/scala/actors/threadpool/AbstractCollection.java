/*
 * Written by Dawid Kurzyniec, based on public domain code written by Doug Lea
 * and publicly available documentation, and released to the public domain, as
 * explained at http://creativecommons.org/licenses/publicdomain
 */

package scala.actors.threadpool;
import scala.actors.threadpool.helpers.Utils;

/**
 * Overrides toArray() and toArray(Object[]) in AbstractCollection to provide
 * implementations valid for concurrent collections.
 *
 * @author Doug Lea
 * @author Dawid Kurzyniec
 */
public abstract class AbstractCollection extends java.util.AbstractCollection {

    /**
     * Sole constructor. (For invocation by subclass constructors, typically
     * implicit.)
     */
    protected AbstractCollection() { super(); }

    public Object[] toArray() {
        return Utils.collectionToArray(this);
    }

    public Object[] toArray(Object[] a) {
        return Utils.collectionToArray(this, a);
    }
}
