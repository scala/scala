/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


object ListMap {
  def Empty[A, B] = new ListMap[A, B];

  /** bug: crashes analzyer with NullPointerException */
  /*
  def fromList[A,B]( list:List[Pair[A,B]] ):ListMap[A,B] =
    list.foldLeft (Empty[A,B]) { ( x:ListMap[A,B],y:Pair[A,B] ) =>
                                  x.update( y._1, y._2 ) };
  */
}

/** This class implements immutable maps using a list-based data
 *  structure. Instances of <code>ListMap</code> represent
 *  empty maps; they can be either created by calling the constructor
 *  directly, or by applying the function <code>ListMap.Empty</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/07/2003
 */
class ListMap[A, B] with Map[A, B, ListMap[A, B]] {

    def size: Int = 0;

    def get(key: A): Option[B] = None;

    def update(key: A, value: B): ListMap[A, B] = new Node(key, value);

    def -(key: A): ListMap[A, B] = this;

    def elements: Iterator[Pair[A, B]] = toList.elements;

    override def toList: List[Pair[A, B]] = Nil;

    override def equals(obj: Any): Boolean =
        if (obj.isInstanceOf[scala.collection.Map[A, B]]) {
            val that = obj.asInstanceOf[scala.collection.Map[A, B]];
            if (size != that.size) false else toList.forall {
                case Pair(key, value) => that.get(key) match {
                    case None => false;
                    case Some(v) => v == value;
                }
            };
        } else
            false;

    override def hashCode(): Int = 0;

    protected class Node(key: A, value: B) extends ListMap[A, B] {
        override def size: Int = ListMap.this.size + 1;
        override def isEmpty: Boolean = false;
        override def apply(k: A): B = if (k == key) value else ListMap.this(k);
        override def get(k: A): Option[B] =
            if (k == key) Some(value) else ListMap.this.get(k);
        override def update(k: A, v: B): ListMap[A, B] =
            if (k == key) {
                new ListMap.this.Node(k, v);
            } else {
                val tail = ListMap.this.update(k,v); new tail.Node(key, value)
            }
        override def -(k: A): ListMap[A, B] =
            if (k == key)
                ListMap.this
            else {
                val tail = ListMap.this - k; new tail.Node(key, value)
            }
        override def toList: List[Pair[A, B]] = Pair(key, value) :: ListMap.this.toList;
        override def hashCode(): Int =
            (key.hashCode() ^ value.hashCode()) + ListMap.this.hashCode();
    }
}


