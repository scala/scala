/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

object SortedMap {
  trait Projection[K,E] extends Map.Projection[K,E] with SortedMap[K,E] {
    override def projection = this
  }
  def apply[T,E](map0 : java.util.SortedMap[T,E]) = new SortedMapWrapper[T,E] {
    val underlying = map0
  }
}
/** A map whose keys are sorted.
 *
 *  @author Sean McDirmid
 */
trait SortedMap[K,E] extends scala.collection.SortedMap[K,E] with Map[K,E] with Sorted[K,Tuple2[K,E]] {
  final protected type SortedSelf = SortedMap[K,E];
  override def compare(k0 : K, k1 : K) : Int;
  override def firstKey : K = elements.next._1;
  override def lastKey : K = {
    val i = elements;
    var last : K = i.next._1;
    while (i.hasNext) last = i.next._1;
    last;
  }
  override def rangeImpl(from : Option[K], until : Option[K]) : SortedMap[K,E] = Range(from, until);
  override def keySet : SortedSet.Projection[K] = new KeySet;

  override def projection : SortedMap.Projection[K,E] = new SortedMap.Projection[K,E] {
    override def elements = SortedMap.this.elements
    override def size = SortedMap.this.size
    override def get(k : K) = SortedMap.this.get(k)
    override def put(k : K, e : E) = SortedMap.this.put(k, e)
    override def compare(k0 : K, k1 : K) = SortedMap.this.compare(k0, k1)
  }

  override def lense[F](f : E => F, g : F => E) : jcl.SortedMap.Projection[K,F] = new Lense[F](f,g);

  protected class Lense[F](f : E => F, g : F => E) extends super.Lense[F](f,g) with SortedMap.Projection[K,F] {
    def compare(k0 : K, k1 : K) = SortedMap.this.compare(k0, k1);
    override def filterKeys(p : K => Boolean) : SortedMap.Projection[K,F] =
      SortedMap.this.projection.filterKeys(p).lense(f,g);
    override def lense[G](f0 : F => G, g0 : G => F) : jcl.SortedMap.Projection[K,G] =
      SortedMap.this.lense[G]({x:E => f0(f(x))}, {y:G => g(g0(y))});
    override def rangeImpl(from : Option[K], until : Option[K]) =
      SortedMap.this.rangeImpl(from,until).lense(f,g);
  }
  protected class KeySet extends super.KeySet with SortedSet.Projection[K] {
    def compare(k0 : K, k1 : K) = SortedMap.this.compare(k0,k1);
    override def firstKey = SortedMap.this.firstKey;
    override def lastKey = SortedMap.this.lastKey;
    override def rangeImpl(from : Option[K], until : Option[K]) =
      SortedMap.this.rangeImpl(from,until).keySet;
  }
  override def filterKeys(p : K => Boolean) : SortedMap.Projection[K,E] = new Filter(p);
  protected class Filter(p : K => Boolean) extends super.Filter(p) with SortedMap.Projection[K,E] {
    def compare(k0 : K, k1 : K) = SortedMap.this.compare(k0,k1);
    override def filterKeys(p0 : K => Boolean) : SortedMap.Projection[K,E] =
      SortedMap.this.filterKeys(k => p(k) && p0(k));
    override def rangeImpl(from : Option[K], until : Option[K]) : SortedMap.Projection[K,E] =
      SortedMap.this.Range(from, until).projection.filterKeys(p);
  }
  protected def Range(from : Option[K], until : Option[K]) : SortedMap.Projection[K,E] = new Range(from,until);
  protected class Range(from : Option[K], until : Option[K]) extends super.Filter(key => {
    ((from == None || (compare(from.get,key) <= 0)) &&
      (until == None || (compare(key,until.get) < 0)));
  }) with SortedMap.Projection[K,E] {
    def compare(k0 : K, k1 : K) = SortedMap.this.compare(k0,k1);
    private def contains0(key : K) =
      (from == None || (compare(from.get,key) <= 0)) &&
        (until == None || (compare(key,until.get) < 0));

    override def contains(key : K) = SortedMap.this.contains(key) && contains0(key);
    override def get(key : K) = if (!contains0(key)) None else SortedMap.this.get(key);
    override def put(key : K, elem : E) = {
      if (!contains0(key)) throw new IllegalArgumentException;
      SortedMap.this.put(key, elem);
    }
    override def rangeImpl(from : Option[K], until : Option[K]) : SortedMap[K,E] = {
      if (this.from != None && from == None) return rangeImpl(this.from, until);
      if (this.until != None && until == None) return rangeImpl(from, this.until);
      if (from != None && compare(this.from.get, from.get) > 0) return rangeImpl(this.from, until);
      if (until != None && compare(this.until.get, until.get) < 0) return rangeImpl(from, this.until);
      SortedMap.this.Range(from, until);
    }
    override def filterKeys(p : K => Boolean) : SortedMap.Projection[K,E] = new Filter(p);
    protected class Filter(p : K => Boolean) extends super.Filter(p) with SortedMap.Projection[K,E] {
      //def compare(k0 : K, k1 : K) = Range.this.compare(k0, k1);
      override def filterKeys(p0 : K => Boolean) = Range.this.projection.filterKeys(k => p(k) && p0(k));
      override def rangeImpl(from : Option[K], until : Option[K]) : SortedMap.Projection[K,E] =
        Range.this.rangeImpl(from,until).projection.filterKeys(p);
    }
  }
}
