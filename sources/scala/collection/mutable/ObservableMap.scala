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
 *  mechanism to the <code>Map</code> class into which this abstract
 *  class is mixed in. Class <code>ObservableMap</code> publishes
 *  events of the type <code>ObservableUpdate</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
abstract class ObservableMap[A, B, This <: ObservableMap[A, B, This]]: This
                    extends scala.collection.mutable.Map[A, B]
                    with Publisher[ObservableUpdate[Pair[A, B]] with Undo, This] {

    abstract override def update(key: A, value: B): Unit = get(key) match {
        case None => super.update(key, value);
                     publish(new Inclusion(Pair(key, value)) with Undo {
                                 def undo = -=(key);
                             });
        case Some(old) => super.update(key, value);
                          publish(new Modification(Pair(key, old), Pair(key, value)) with Undo {
                                      def undo = update(key, old._2);
                                  });
    }

    abstract override def -=(key: A): Unit = get(key) match {
        case None =>
        case Some(old) => super.-=(key);
                          publish(new Removal(Pair(key, old)) with Undo {
                                      def undo = update(key, old);
                                  });
    }

    abstract override def clear: Unit = {
        super.clear;
        publish(new Reset() with Undo { def undo = error("cannot undo"); });
    }
}
