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
  def valueSet : MutableIterable.Projection[E] = projection.map(_._2);

  override def put(key : K, elem : E) : Option[E] = throw new java.lang.AbstractMethodError

  override def ++=(that : Iterable[(K,E)]) : Unit =
    that.foreach(p => put(p._1, p._2));

  override def removeKey(key : K) : Option[E] = {
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
  override def get(key : K) = elements.find(p => p._1 == key).map(_._2);
  override def update(key : K, e : E) : Unit = put(key,e);
  override def +(pair : Tuple2[K,E]) : this.type = {
    put(pair._1,pair._2); this;
  }
  override def +=(pair : Tuple2[K,E]) : Unit = put(pair._1, pair._2);
  override def -(key : K) : this.type = {
    removeKey(key); this;
  }
  override def remove(p : (K,E)) = get(p._1) match {
  case Some(p._2) => this -= p._1; true
  case _ => false;
  }

  override def -=(key : K) : Unit = removeKey(key);
  override def elements : MutableIterator[Tuple2[K,E]];

  override def projection : Map.Projection[K,E] = new Map.Projection[K,E] {
    override def elements = Map.this.elements
    override def size = Map.this.size
    override def get(k : K) = Map.this.get(k)
    override def put(k : K, e : E) = Map.this.put(k, e)
  }
  /**
   */
  def lense[F](f : E => F, g : F => E) : jcl.Map.Projection[K,F] = new Lense[F](f,g);

  protected class Lense[F](f : E => F, g : F => E) extends jcl.Map.Projection[K,F] {
    override def elements = Map.this.elements.map(k => Tuple2(k._1, f(k._2)));
    override def removeKey(key : K) = Map.this.removeKey(key).map(f);
    override def put(key : K, elem : F) = Map.this.put(key, g(elem)).map(f);
    override def get(key : K) = Map.this.get(key).map(f);
    override def lense[G](f0 : F => G, g0 : G => F) : jcl.Map.Projection[K,G] =
      Map.this.lense[G](x => f0(f(x)), y => g(g0(y)));
    override def size = size0;
  }
  protected class KeySet extends Set[K] {
    override def size = Map.this.size;
    override def add(k : K) = Map.this.put(k, default(k)) == None;
    override def elements = Map.this.elements.map(_._1);
    override def has(k : K) = Map.this.contains(k);
  }
  override def filterKeys(p : K => Boolean) : Map.Projection[K,E] = new Filter(p);

  protected class Filter(p : K => Boolean) extends Map.Projection[K,E] {
    override def elements = {
      val i = Map.this.elements.filter(e => p(e._1));
      new MutableIterator[(K,E)] {
        def next = i.next
        def hasNext = i.hasNext
        def remove : Unit = throw new Error
      }
    }
   override def removeKey(key : K) = {
      if (!p(key)) throw new IllegalArgumentException;
      Map.this.removeKey(key);
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
    override def filterKeys(p0 : K => Boolean) : Map.Projection[K,E] =
      Map.this.filterKeys(e => p(e) && p0(e));

    override def size = size0;
  }
}

object Map {
  trait MutableIterableProjection[A] extends MutableIterable.Projection[A];
  trait Projection[K,E] extends MutableIterableProjection[(K,E)] with scala.collection.Map.Projection[K,E] with Map[K,E] {
    override def projection = this
    override def map[B](f : ((K,E)) => B) : MutableIterable.Projection[B] = super[MutableIterableProjection].map(f);
  }
}
