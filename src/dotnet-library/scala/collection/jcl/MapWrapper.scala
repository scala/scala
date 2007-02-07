/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A wrapper around a Java map.
 *
 *  @author Sean McDirmid
 */
trait MapWrapper[K,E] extends jcl.Map[K,E] {
  protected def underlying : java.util.Map;
  override def size = underlying.size;
  override def isEmpty = underlying.isEmpty;
  override def clear() = underlying.clear;
  override def put(key : K, elem : E) = {
    if (elem == null) throw new IllegalArgumentException;
    val ret = underlying.put(key,elem);
    if (ret == null) None else Some(ret.asInstanceOf[E]);
  }
  override def get(key : K) : Option[E] = {
    val ret = underlying.get(key);
    if (ret == null) None else Some(ret.asInstanceOf[E]);
  }

  override def putAll(that : Iterable[Tuple2[K,E]]) : Unit = that match {
  case that : MapWrapper[_,_] => underlying.putAll(that.underlying);
  case _ => super.putAll(that);
  }
  override def remove(key : K) = {
    val ret = underlying.remove(key);
    if (ret == null) None else Some(ret.asInstanceOf[E]);
  }
  override def contains(key : K) = underlying.containsKey(key);
  override def keySet : Set[K] = new KeySet;
  override def valueSet : MutableIterable[E] = new ValueSet;
  override def elements : MutableIterator[Tuple2[K,E]] = new IteratorWrapper;
  class IteratorWrapper extends MutableIterator[Tuple2[K,E]] {
    val underlying = MapWrapper.this.underlying.entrySet.iterator;
    def hasNext = underlying.hasNext;
    def remove = underlying.remove;
    def next = {
      val next = underlying.next.asInstanceOf[java.util.Map.Entry];
      Tuple2(next.getKey.asInstanceOf[K],next.getValue.asInstanceOf[E]);
    }
  }
  class KeySet extends super.KeySet with SetWrapper[K] {
    protected val underlying = MapWrapper.this.underlying.keySet;
  }
  class ValueSet extends IterableWrapper[E] {
    val underlying = MapWrapper.this.underlying.values;
    override def has(e : E) = MapWrapper.this.underlying.containsValue(e);
  }
  override def toString = underlying.toString;
  override def hashCode = underlying.hashCode;
  override def equals(that : Any) = that match {
    case that: MapWrapper[_,_] => underlying == that.underlying;
    case _ => super.equals(that);
  }
}
