/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


abstract class ObservableSet[A, This <: ObservableSet[A, This]]: This
                    extends MutableSet[A]
                    with Publisher[ObservableUpdate[A] with Undo, This] {

    override def add(elem: A): Unit = if (!contains(elem)) {
   		super.add(elem);
   		publish(new Inclusion(elem) with Undo {
   		            def undo = remove(elem);
   		        });
   	}

	override def remove(elem: A): Unit = if (contains(elem)) {
		super.remove(elem);
		publish(new Removal(elem) with Undo {
   		            def undo = add(elem);
   		        });
	}

    override def clear: Unit = {
        super.clear;
        publish(new Reset() with Undo { def undo = error("cannot undo"); });
    }
}
