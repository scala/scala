/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl

/** A wrapper around a Java map.
 *
 *  @author Sean McDirmid
 */
trait MapWrapper[K, E] extends jcl.Map[K, E] {
  def underlying: java.util.Map[K, E]
  override def size = underlying.size
  override def isEmpty = underlying.isEmpty
  override def clear() = underlying.clear

  override def put(key: K, elem: E) = {
    //if (elem == null) throw new IllegalArgumentException;
    val ret = underlying.put(key, elem)
    if (ret == null) None else Some(ret.asInstanceOf[E])
  }

  override def get(key : K) : Option[E] = {
    val ret = underlying.get(key);
    if (ret == null) None else Some(ret.asInstanceOf[E]);
  }

  override def ++=(that : Iterable[Tuple2[K,E]]) : Unit = that match {
    case that : MapWrapper[_,_] => underlying.putAll(that.underlying);
    case _ => super.++=(that)
  }

  override def removeKey(key: K) = {
    val ret = underlying.remove(key)
    if (ret == null) None else Some(ret.asInstanceOf[E])
  }

  override def contains(key: K) = underlying.containsKey(key)
  override def keySet: Set.Projection[K] = new KeySet
  override def valueSet: MutableIterable.Projection[E] = new ValueSet
  override def elements: MutableIterator[Tuple2[K,E]] = new IteratorWrapper

  class IteratorWrapper extends MutableIterator[Tuple2[K,E]] {
    val underlying = MapWrapper.this.underlying.entrySet.iterator
    def hasNext = underlying.hasNext
    def remove = underlying.remove
    def next = {
      val next = underlying.next.asInstanceOf[java.util.Map.Entry[K,E]]
      Tuple2(next.getKey.asInstanceOf[K],next.getValue.asInstanceOf[E])
    }
  }

  class KeySet extends super.KeySet with SetWrapper[K] with Set.Projection[K] {
    val underlying = MapWrapper.this.underlying.keySet
  }

  class ValueSet extends IterableWrapper[E] with MutableIterable.Projection[E] {
    override def size = MapWrapper.this.size
    val underlying = MapWrapper.this.underlying.values
    override def has(e : E) = MapWrapper.this.underlying.containsValue(e)
  }

  override def toString = underlying.toString
  override def hashCode = underlying.hashCode

  override def equals(that : Any) = that match {
    case that: MapWrapper[_,_] => underlying == that.underlying
    case _ => super.equals(that)
  }
}
