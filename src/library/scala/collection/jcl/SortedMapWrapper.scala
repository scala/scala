/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A sorted map that wraps an underlying Java sorted map.
 *
 *  @author Sean McDirmid
 */
trait SortedMapWrapper[K,E] extends SortedMap[K,E] with MapWrapper[K,E] {
  protected override def underlying : java.util.SortedMap;
  /** the comparator function of this sorted map is defined in terms
    * of the underlying sorted map's comparator.
    */
  def compare(k0 : K, k1 : K) = underlying.comparator.compare(k0,k1);
  override def first = underlying.firstKey.asInstanceOf[K];
  override def last = underlying.lastKey.asInstanceOf[K];
  override def keySet : SortedSet[K] = new KeySet;
  override protected def Range(from : Option[K], until : Option[K]) = new Range(from,until);
  protected class Range(from : Option[K], until : Option[K]) extends super.Range(from,until) with SortedMapWrapper[K,E] {
    val underlying = Tuple2(from,until) match {
    case Tuple2(None,None) => throw new IllegalArgumentException;
    case Tuple2(Some(from),None) => SortedMapWrapper.this.underlying.tailMap(from);
    case Tuple2(None,Some(until)) => SortedMapWrapper.this.underlying.headMap(until);
    case Tuple2(Some(from),Some(until)) => SortedMapWrapper.this.underlying.subMap(from,until);
    }
    override def compare(k0 : K, k1 : K) = super[SortedMapWrapper].compare(k0, k1);
  }
  protected class KeySet extends super[SortedMap].KeySet with SetWrapper[K] {
    protected val underlying = SortedMapWrapper.this.underlying.keySet;
  }
}
