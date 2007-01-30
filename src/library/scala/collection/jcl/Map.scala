/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A mutable map that is compatible with Java maps.
 *
 *  @author Sean McDirmid
 */
trait Map[K,E] extends MutableIterable[Tuple2[K,E]] with scala.collection.mutable.Map[K,E] {
  override def clear() = super[MutableIterable].clear;
  override def isEmpty = super[MutableIterable].isEmpty;
  override def keySet : Set[K] = new KeySet;
  override final def keys = keySet.elements;
  /** The values of this map as a projection, which means
      removals from the returned collection will remove the element from this map.
      @returns a projection of this map's elements.  */
  def valueSet : MutableIterable[E] = pmap(._2);
  def put(key : K, elem : E) : Option[E];
  def putAll(that : Iterable[Tuple2[K,E]]) : Unit =
    that.foreach(p => put(p._1, p._2));
  def remove(key : K) : Option[E] = {
    val i = elements;
    while (!i.hasNext) {
      val result = i.next;
      if (result._1 == key) {
        i.remove;
        return Some(result._2);
      }
    }
    return None;
  }
  override def has(pair : Tuple2[K,E]) = get(pair._1) match {
  case Some(e) if e == pair._2 => true;
  case _ => false;
  }
  override def get(key : K) = elements.find(p => p._1 == key).map(._2);
  override def update(key : K, e : E) : Unit = put(key,e);
  override def +(pair : Tuple2[K,E]) : this.type = {
    put(pair._1,pair._2); this;
  }
  override def +=(pair : Tuple2[K,E]) : Unit = put(pair._1, pair._2);
  override def -(key : K) : this.type = {
    remove(key); this;
  }
  override def -=(key : K) : Unit = remove(key);
  override def elements : MutableIterator[Tuple2[K,E]];
  /** Produces a filtered projection of this map that includes only entries of the map
   *  whose keys are true with respect to predicate "p."
   */
  def pfilter(p : K => Boolean) : jcl.Map[K,E] = new Filter(p);
  /**
   */
  def lense[F](f : E => F, g : F => E) : jcl.Map[K,F] = new Lense[F](f,g);

  protected class Lense[F](f : E => F, g : F => E) extends jcl.Map[K,F] {
    override def elements = Map.this.elements.map(k => Tuple2(k._1, f(k._2)));
    override def remove(key : K) = Map.this.remove(key).map(f);
    override def put(key : K, elem : F) = Map.this.put(key, g(elem)).map(f);
    override def get(key : K) = Map.this.get(key).map(f);
    override def pfilter(p : K => Boolean) : jcl.Map[K,F] =
      Map.this.pfilter(p).lense(f, g);
    override def lense[G](f0 : F => G, g0 : G => F) : jcl.Map[K,G] =
      Map.this.lense[G](x => f0(f(x)), y => g(g0(y)));
  }
  protected class Filter(p : K => Boolean) extends jcl.Map[K,E] {
    override def elements = Map.this.elements.filter(k => p(k._1));
    override def remove(key : K) = {
      if (!p(key)) throw new IllegalArgumentException;
      Map.this.remove(key);
    }
    override def contains(key : K) = p(key) && Map.this.contains(key);
    override def put(key : K, elem : E) = {
      if (!p(key)) throw new IllegalArgumentException;
      Map.this.put(key, elem);
    }
    override def get(key : K) = {
      if (!p(key)) None;
      else Map.this.get(key);
    }
    override def pfilter(p0 : K => Boolean) : jcl.Map[K,E] =
      Map.this.pfilter(k => p(k) && p0(k));
  }
  protected class KeySet extends Set[K] {
    override def add(k : K) = Map.this.put(k, default(k)) == None;
    override def elements = Map.this.elements.map(._1);
    override def has(k : K) = Map.this.contains(k);
  }

}
