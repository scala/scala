/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This class is typically used as a mixin. It adds a subscription
 *  mechanism to the <code>Buffer</code> class into which this abstract
 *  class is mixed in. Class <code>ObservableBuffer</code> publishes
 *  events of the type <code>Message</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
abstract class ObservableBuffer[A, This <: ObservableBuffer[A, This]]: This
                    extends Buffer[A]
                    with Publisher[Message[Pair[Location, A]] with Undoable, This] {

    abstract override def +(elem: A): Buffer[A] = {
        super.+(elem);
        publish(new Include(Pair(End, elem)) with Undoable {
            def undo: Unit = trimEnd(1);
        });
        this
    }

    abstract override def +:(elem: A): Buffer[A] = {
        super.+:(elem);
        publish(new Include(Pair(Start, elem)) with Undoable {
            def undo: Unit = trimStart(1);
        });
        this
    }

    abstract override def insertAll(n: Int, iter: Iterable[A]): Unit = {
        super.insertAll(n, iter);
        var i = n;
        val it = iter.elements;
        while (it.hasNext) {
            publish(new Include(Pair(Index(i), it.next)) with Undoable {
                def undo: Unit = remove(i);
            });
            i = i + 1;
        }
    }

    abstract override def update(n: Int, newelem: A): Unit = {
        val oldelem = apply(n);
        super.update(n, newelem);
        publish(new Update(Pair(Index(n), newelem)) with Undoable {
            def undo: Unit = update(n, oldelem);
        });
    }

    abstract override def remove(n: Int): A = {
        val oldelem = apply(n);
        super.remove(n);
        publish(new Remove(Pair(Index(n), oldelem)) with Undoable {
            def undo: Unit = insert(n, oldelem);
        });
        oldelem
    }

    abstract override def clear: Unit = {
        super.clear;
        publish(new Reset with Undoable { def undo: Unit = error("cannot undo"); });
    }
}
