/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

import scala.collection.mutable._;


abstract class Enumeration(initial: Int, names: String*) {

    def this() = this(0, "0");

    def this(names: String*) = this(0, names: _*);

    /** A mapping between the enumeration value id and the enumeration
     *  object.
     */
    private var values: Map[Int, Value] = new HashMap;

    /** A cache listing all values of this enumeration.
     */
    private var vcache: List[Value] = null;

    private def updateCache: List[Value] =
    	if (vcache == null) {
    		vcache = values.values.toList.sort((p1, p2) => p1.id < p2.id);
    		vcache
    	} else
    		vcache;

    protected var nextId = initial;

    protected var nextName = names.elements;

    private var topId = initial;

    final def maxId = topId;

    /** Returns the enumeration value for the given id.
     */
    final def apply(x: Int): Value = values(x);

    /** Returns all values of this enumeration.
     */
    final def elements: Iterator[Value] = updateCache.elements;

    def foreach(f: Value => Unit): Unit = elements foreach f;

    def forall(p: Value => Boolean): Boolean = elements forall p;

    def exists(p: Value => Boolean): Boolean = elements exists p;

    def map[b](f: Value => b): Iterator[b] = elements map f;

    def flatMap[b](f: Value => Iterator[b]): Iterator[b] = elements flatMap f;

    def filter(p: Value => Boolean): Iterator[Value] = elements filter p;

    override def toString(): String = updateCache.mkString("{", ", ", "}");

    protected final def Value: Value =
    	new Val(nextId, if (nextName.hasNext) nextName.next else nextId.toString());

    protected final def Value(i: Int): Value =
    	new Val(i, if (nextName.hasNext) nextName.next else i.toString());

    protected final def Value(name: String): Value = new Val(nextId, name);

    protected final def Value(i: Int, name: String): Value = new Val(nextId, nextId.toString());

    trait Value extends Ord[Value] {
        def id: Int;
        def < (that: Value): Boolean = id < that.id;
    }

    protected class Val(i: Int, name: String) extends Value {
        def this(i: Int) =
        	this(i, if (nextName.hasNext) nextName.next else i.toString());
        def this(name: String) = this(nextId, name);
        def this() =
        	this(nextId, if (nextName.hasNext) nextName.next else nextId.toString());
        assert(!values.isDefinedAt(i));
        values(i) = this;
        nextId = i + 1;
        if (nextId > topId)
        	topId = nextId;
        def id = i;
        override def toString() = name;
    }
}
