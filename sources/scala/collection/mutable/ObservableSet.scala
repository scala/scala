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
 *  mechanism to the <code>Set</code> class into which this abstract
 *  class is mixed in. Class <code>ObservableSet</code> publishes
 *  events of the type <code>ObservableUpdate</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
abstract class ObservableSet[A, This <: ObservableSet[A, This]]: This
                    extends scala.collection.mutable.Set[A]
                    with Publisher[ObservableUpdate[A] with Undo, This] {

    abstract override def +=(elem: A): Unit = if (!contains(elem)) {
        super.+=(elem);
        publish(new Inclusion(elem) with Undo { def undo = -=(elem); });
    }

    abstract override def -=(elem: A): Unit = if (contains(elem)) {
        super.-=(elem);
        publish(new Removal(elem) with Undo { def undo = +=(elem); });
    }

    abstract override def clear: Unit = {
        super.clear;
        publish(new Reset() with Undo { def undo = error("cannot undo"); });
    }
}
