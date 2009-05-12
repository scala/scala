/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import generic._

trait Map[A, +B] extends Iterable[(A, B)]
                    with collection.Map[A, B]
                    with ImmutableMapTemplate[A, B, Map[A, B]] { self =>

  override def empty: Map[A, B] = Map.empty
  override def mapBuilder[A, B]: Builder[(A, B), Map[A, B]] = Map.newBuilder[A, B]

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  override def updated [B1 >: B](key: A, value: B1): Map[A, B1]
  def + [B1 >: B](kv: (A, B1)): Map[A, B1]

  /** A hash method compatible with <code>equals</code>
   */
  override def hashCode() =
    (Map.hashSeed /: this) (_ * 41 + _.hashCode)


  /** The same map with a given default function !!! todo: move to general maps? */
  def withDefault[B1 >: B](d: A => B1): Map[A, B1] = new Map.WithDefault[A, B1](this, d)

  /** The same map with a given default value */
  def withDefaultValue[B1 >: B](d: B1): Map[A, B1] = new Map.WithDefault[A, B1](this, x => d)
}

object Map extends ImmutableMapFactory[Map] {
  private val hashSeed = "Map".hashCode
  type Coll = Map[_, _]
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), Map[A, B], Coll] = new BuilderFactory[(A, B), Map[A, B], Coll] { def apply(from: Coll) = from.mapBuilder[A, B] }

  def empty[A, B]: Map[A, B] = FlexMap.empty

  class WithDefault[A, +B](underlying: Map[A, B], d: A => B) extends Map[A, B] {
    override def size = underlying.size
    def get(key: A) = underlying.get(key)
    def elements = underlying.elements
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[B1 >: B](key: A, value: B1): WithDefault[A, B1] = new WithDefault[A, B1](underlying.updated[B1](key, value), d)
    def + [B1 >: B](kv: (A, B1)): WithDefault[A, B1] = updated(kv._1, kv._2)
    def - (key: A): WithDefault[A, B] = new WithDefault(underlying - key, d)
    override def default(key: A): B = d(key)
  }
}

