/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package immutable

import java.lang.IllegalStateException

import scala.collection.generic.{BitOperations, DefaultSerializationProxy}
import scala.collection.mutable.{Builder, ImmutableBuilder, ListBuffer}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.language.implicitConversions

/** Utility class for long maps.
  */
private[immutable] object LongMapUtils extends BitOperations.Long {
  def branchMask(i: Long, j: Long) = highestOneBit(i ^ j)

  def join[T](p1: Long, t1: LongMap[T], p2: Long, t2: LongMap[T]): LongMap[T] = {
    val m = branchMask(p1, p2)
    val p = mask(p1, m)
    if (zero(p1, m)) LongMap.Bin(p, m, t1, t2)
    else LongMap.Bin(p, m, t2, t1)
  }

  def bin[T](prefix: Long, mask: Long, left: LongMap[T], right: LongMap[T]): LongMap[T] = (left, right) match {
    case (left, LongMap.Nil) => left
    case (LongMap.Nil, right) => right
    case (left, right) => LongMap.Bin(prefix, mask, left, right)
  }
}

import LongMapUtils.{Long => _, _}

/** A companion object for long maps.
  *
  *  @define Coll  `LongMap`
  */
object LongMap {
  def empty[T]: LongMap[T]  = LongMap.Nil
  def singleton[T](key: Long, value: T): LongMap[T] = LongMap.Tip(key, value)
  def apply[T](elems: (Long, T)*): LongMap[T] =
    elems.foldLeft(empty[T])((x, y) => x.updated(y._1, y._2))

  def from[V](coll: IterableOnce[(Long, V)]): LongMap[V] =
    newBuilder[V].addAll(coll).result()

  def newBuilder[V]: Builder[(Long, V), LongMap[V]] =
    new ImmutableBuilder[(Long, V), LongMap[V]](empty) {
      def addOne(elem: (Long, V)): this.type = { elems = elems + elem; this }
    }

  private[immutable] case object Nil extends LongMap[Nothing] {
    // Important, don't remove this! See IntMap for explanation.
    override def equals(that : Any) = that match {
      case _: this.type  => true
      case _: LongMap[_] => false // The only empty LongMaps are eq Nil
      case _             => super.equals(that)
    }
  }

  private[immutable] case class Tip[+T](key: Long, value: T) extends LongMap[T] {
    def withValue[S](s: S) =
      if (s.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[LongMap.Tip[S]]
      else LongMap.Tip(key, s)
  }

  private[immutable] case class Bin[+T](prefix: Long, mask: Long, left: LongMap[T], right: LongMap[T]) extends LongMap[T] {
    def bin[S](left: LongMap[S], right: LongMap[S]): LongMap[S] = {
      if ((this.left eq left) && (this.right eq right)) this.asInstanceOf[LongMap.Bin[S]]
      else LongMap.Bin[S](prefix, mask, left, right)
    }
  }

  implicit def toFactory[V](dummy: LongMap.type): Factory[(Long, V), LongMap[V]] = ToFactory.asInstanceOf[Factory[(Long, V), LongMap[V]]]

  @SerialVersionUID(3L)
  private[this] object ToFactory extends Factory[(Long, AnyRef), LongMap[AnyRef]] with Serializable {
    def fromSpecific(it: IterableOnce[(Long, AnyRef)]): LongMap[AnyRef] = LongMap.from[AnyRef](it)
    def newBuilder: Builder[(Long, AnyRef), LongMap[AnyRef]] = LongMap.newBuilder[AnyRef]
  }

  implicit def toBuildFrom[V](factory: LongMap.type): BuildFrom[Any, (Long, V), LongMap[V]] = ToBuildFrom.asInstanceOf[BuildFrom[Any, (Long, V), LongMap[V]]]
  private[this] object ToBuildFrom extends BuildFrom[Any, (Long, AnyRef), LongMap[AnyRef]] {
    def fromSpecific(from: Any)(it: IterableOnce[(Long, AnyRef)]) = LongMap.from(it)
    def newBuilder(from: Any) = LongMap.newBuilder[AnyRef]
  }

  implicit def iterableFactory[V]: Factory[(Long, V), LongMap[V]] = toFactory(this)
  implicit def buildFromLongMap[V]: BuildFrom[LongMap[_], (Long, V), LongMap[V]] = toBuildFrom(this)
}

// Iterator over a non-empty LongMap.
private[immutable] abstract class LongMapIterator[V, T](it: LongMap[V]) extends AbstractIterator[T] {

  // Basically this uses a simple stack to emulate conversion over the tree. However
  // because we know that Longs are only 64 bits we can have at most 64 LongMap.Bins and
  // one LongMap.Tip sitting on the tree at any point. Therefore we know the maximum stack
  // depth is 65
  var index = 0
  var buffer = new Array[AnyRef](65)

  def pop() = {
    index -= 1
    buffer(index).asInstanceOf[LongMap[V]]
  }

  def push(x: LongMap[V]): Unit = {
    buffer(index) = x.asInstanceOf[AnyRef]
    index += 1
  }
  push(it)

  /**
    * What value do we assign to a tip?
    */
  def valueOf(tip: LongMap.Tip[V]): T

  def hasNext = index != 0
  @tailrec
  final def next(): T =
    pop() match {
      case LongMap.Bin(_,_, t@LongMap.Tip(_, _), right) => {
        push(right)
        valueOf(t)
      }
      case LongMap.Bin(_, _, left, right) => {
        push(right)
        push(left)
        next()
      }
      case t@LongMap.Tip(_, _) => valueOf(t)
      // This should never happen. We don't allow LongMap.Nil in subtrees of the LongMap
      // and don't return an LongMapIterator for LongMap.Nil.
      case LongMap.Nil => throw new IllegalStateException("Empty maps not allowed as subtrees")
    }
}

private[immutable] class LongMapEntryIterator[V](it: LongMap[V]) extends LongMapIterator[V, (Long, V)](it){
  def valueOf(tip: LongMap.Tip[V]) = (tip.key, tip.value)
}

private[immutable] class LongMapValueIterator[V](it: LongMap[V]) extends LongMapIterator[V, V](it){
  def valueOf(tip: LongMap.Tip[V]) = tip.value
}

private[immutable] class LongMapKeyIterator[V](it: LongMap[V]) extends LongMapIterator[V, Long](it){
  def valueOf(tip: LongMap.Tip[V]) = tip.key
}

/**
  *  Specialised immutable map structure for long keys, based on
  *  [[https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452 Fast Mergeable Long Maps]]
  *  by Okasaki and Gill. Essentially a trie based on binary digits of the integers.
  *
  *  Note: This class is as of 2.8 largely superseded by HashMap.
  *
  *  @tparam T      type of the values associated with the long keys.
  *
  *  @define Coll `immutable.LongMap`
  *  @define coll immutable long integer map
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
sealed abstract class LongMap[+T] extends AbstractMap[Long, T]
  with StrictOptimizedMapOps[Long, T, Map, LongMap[T]]
  with Serializable {

  override protected def fromSpecific(coll: scala.collection.IterableOnce[(Long, T)] @uncheckedVariance): LongMap[T] = {
    //TODO should this be the default implementation of this method in StrictOptimizedIterableOps?
    val b = newSpecificBuilder
    b.sizeHint(coll)
    b.addAll(coll)
    b.result()
  }
  override protected def newSpecificBuilder: Builder[(Long, T), LongMap[T]] @uncheckedVariance =
    new ImmutableBuilder[(Long, T), LongMap[T]](empty) {
      def addOne(elem: (Long, T)): this.type = { elems = elems + elem; this }
    }

  override def empty: LongMap[T] = LongMap.Nil

  override def toList = {
    val buffer = new ListBuffer[(Long, T)]
    foreach(buffer += _)
    buffer.toList
  }

  /**
    * Iterator over key, value pairs of the map in unsigned order of the keys.
    *
    * @return an iterator over pairs of long keys and corresponding values.
    */
  def iterator: Iterator[(Long, T)] = this match {
    case LongMap.Nil => Iterator.empty
    case _ => new LongMapEntryIterator(this)
  }

  /**
    * Loops over the key, value pairs of the map in unsigned order of the keys.
    */
  override final def foreach[U](f: ((Long, T)) => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreach(f); right.foreach(f) }
    case LongMap.Tip(key, value) => f((key, value))
    case LongMap.Nil =>
  }

  override final def foreachEntry[U](f: (Long, T) => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreachEntry(f); right.foreachEntry(f) }
    case LongMap.Tip(key, value) => f(key, value)
    case LongMap.Nil =>
  }

  override def keysIterator: Iterator[Long] = this match {
    case LongMap.Nil => Iterator.empty
    case _ => new LongMapKeyIterator(this)
  }

  /**
    * Loop over the keys of the map. The same as keys.foreach(f), but may
    * be more efficient.
    *
    * @param f The loop body
    */
  final def foreachKey[U](f: Long => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreachKey(f); right.foreachKey(f) }
    case LongMap.Tip(key, _) => f(key)
    case LongMap.Nil =>
  }

  override def valuesIterator: Iterator[T] = this match {
    case LongMap.Nil => Iterator.empty
    case _ => new LongMapValueIterator(this)
  }

  /**
    * Loop over the values of the map. The same as values.foreach(f), but may
    * be more efficient.
    *
    * @param f The loop body
    */
  final def foreachValue[U](f: T => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreachValue(f); right.foreachValue(f) }
    case LongMap.Tip(_, value) => f(value)
    case LongMap.Nil =>
  }

  override protected[this] def className = "LongMap"

  override def isEmpty = this eq LongMap.Nil
  override def knownSize: Int = if (isEmpty) 0 else super.knownSize
  override def filter(f: ((Long, T)) => Boolean): LongMap[T] = this match {
    case LongMap.Bin(prefix, mask, left, right) => {
      val (newleft, newright) = (left.filter(f), right.filter(f))
      if ((left eq newleft) && (right eq newright)) this
      else bin(prefix, mask, newleft, newright)
    }
    case LongMap.Tip(key, value) =>
      if (f((key, value))) this
      else LongMap.Nil
    case LongMap.Nil => LongMap.Nil
  }

  override def transform[S](f: (Long, T) => S): LongMap[S] = this match {
    case b@LongMap.Bin(prefix, mask, left, right) => b.bin(left.transform(f), right.transform(f))
    case t@LongMap.Tip(key, value) => t.withValue(f(key, value))
    case LongMap.Nil => LongMap.Nil
  }

  final override def size: Int = this match {
    case LongMap.Nil => 0
    case LongMap.Tip(_, _) => 1
    case LongMap.Bin(_, _, left, right) => left.size + right.size
  }

  @tailrec
  final def get(key: Long): Option[T] = this match {
    case LongMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left.get(key) else right.get(key)
    case LongMap.Tip(key2, value) => if (key == key2) Some(value) else None
    case LongMap.Nil => None
  }

  @tailrec
  final override def getOrElse[S >: T](key: Long, default: => S): S = this match {
    case LongMap.Nil => default
    case LongMap.Tip(key2, value) => if (key == key2) value else default
    case LongMap.Bin(prefix, mask, left, right) =>
      if (zero(key, mask)) left.getOrElse(key, default) else right.getOrElse(key, default)
  }

  @tailrec
  final override def apply(key: Long): T = this match {
    case LongMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left(key) else right(key)
    case LongMap.Tip(key2, value) => if (key == key2) value else throw new IllegalArgumentException("Key not found")
    case LongMap.Nil => throw new IllegalArgumentException("key not found")
  }

  override def + [S >: T] (kv: (Long, S)): LongMap[S] = updated(kv._1, kv._2)

  override def updated[S >: T](key: Long, value: S): LongMap[S] = this match {
    case LongMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, LongMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) LongMap.Bin(prefix, mask, left.updated(key, value), right)
      else LongMap.Bin(prefix, mask, left, right.updated(key, value))
    case LongMap.Tip(key2, value2) =>
      if (key == key2) LongMap.Tip(key, value)
      else join(key, LongMap.Tip(key, value), key2, this)
    case LongMap.Nil => LongMap.Tip(key, value)
  }

  /**
    * Updates the map, using the provided function to resolve conflicts if the key is already present.
    *
    * Equivalent to
    * {{{
    *   this.get(key) match {
    *     case None => this.update(key, value)
    *     case Some(oldvalue) => this.update(key, f(oldvalue, value)
    *   }
    * }}}
    *
    * @tparam S     The supertype of values in this `LongMap`.
    * @param key    The key to update.
    * @param value  The value to use if there is no conflict.
    * @param f      The function used to resolve conflicts.
    * @return       The updated map.
    */
  def updateWith[S >: T](key: Long, value: S, f: (T, S) => S): LongMap[S] = this match {
    case LongMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, LongMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) LongMap.Bin(prefix, mask, left.updateWith(key, value, f), right)
      else LongMap.Bin(prefix, mask, left, right.updateWith(key, value, f))
    case LongMap.Tip(key2, value2) =>
      if (key == key2) LongMap.Tip(key, f(value2, value))
      else join(key, LongMap.Tip(key, value), key2, this)
    case LongMap.Nil => LongMap.Tip(key, value)
  }

  def removed(key: Long): LongMap[T] = this match {
    case LongMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) this
      else if (zero(key, mask)) bin(prefix, mask, left - key, right)
      else bin(prefix, mask, left, right - key)
    case LongMap.Tip(key2, _) =>
      if (key == key2) LongMap.Nil
      else this
    case LongMap.Nil => LongMap.Nil
  }

  /**
    * A combined transform and filter function. Returns an `LongMap` such that
    * for each `(key, value)` mapping in this map, if `f(key, value) == None`
    * the map contains no mapping for key, and if `f(key, value)`.
    *
    * @tparam S    The type of the values in the resulting `LongMap`.
    * @param f     The transforming function.
    * @return      The modified map.
    */
  def modifyOrRemove[S](f: (Long, T) => Option[S]): LongMap[S] = this match {
    case LongMap.Bin(prefix, mask, left, right) => {
      val newleft = left.modifyOrRemove(f)
      val newright = right.modifyOrRemove(f)
      if ((left eq newleft) && (right eq newright)) this.asInstanceOf[LongMap[S]]
      else bin(prefix, mask, newleft, newright)
    }
    case LongMap.Tip(key, value) => f(key, value) match {
      case None => LongMap.Nil
      case Some(value2) =>
        //hack to preserve sharing
        if (value.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) this.asInstanceOf[LongMap[S]]
        else LongMap.Tip(key, value2)
    }
    case LongMap.Nil => LongMap.Nil
  }

  /**
    * Forms a union map with that map, using the combining function to resolve conflicts.
    *
    * @tparam S      The type of values in `that`, a supertype of values in `this`.
    * @param that    The map to form a union with.
    * @param f       The function used to resolve conflicts between two mappings.
    * @return        Union of `this` and `that`, with identical key conflicts resolved using the function `f`.
    */
  def unionWith[S >: T](that: LongMap[S], f: (Long, S, S) => S): LongMap[S] = (this, that) match{
    case (LongMap.Bin(p1, m1, l1, r1), that@(LongMap.Bin(p2, m2, l2, r2))) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) join(p1, this, p2, that)
        else if (zero(p2, m1)) LongMap.Bin(p1, m1, l1.unionWith(that, f), r1)
        else LongMap.Bin(p1, m1, l1, r1.unionWith(that, f))
      } else if (shorter(m2, m1)){
        if (!hasMatch(p1, p2, m2)) join(p1, this, p2, that)
        else if (zero(p1, m2)) LongMap.Bin(p2, m2, this.unionWith(l2, f), r2)
        else LongMap.Bin(p2, m2, l2, this.unionWith(r2, f))
      }
      else {
        if (p1 == p2) LongMap.Bin(p1, m1, l1.unionWith(l2,f), r1.unionWith(r2, f))
        else join(p1, this, p2, that)
      }
    case (LongMap.Tip(key, value), x) => x.updateWith(key, value, (x, y) => f(key, y, x))
    case (x, LongMap.Tip(key, value)) => x.updateWith[S](key, value, (x, y) => f(key, x, y))
    case (LongMap.Nil, x) => x
    case (x, LongMap.Nil) => x
  }

  /**
    * Forms the intersection of these two maps with a combining function. The
    * resulting map is a map that has only keys present in both maps and has
    * values produced from the original mappings by combining them with `f`.
    *
    * @tparam S      The type of values in `that`.
    * @tparam R      The type of values in the resulting `LongMap`.
    * @param that    The map to intersect with.
    * @param f       The combining function.
    * @return        Intersection of `this` and `that`, with values for identical keys produced by function `f`.
    */
  def intersectionWith[S, R](that: LongMap[S], f: (Long, T, S) => R): LongMap[R] = (this, that) match {
    case (LongMap.Bin(p1, m1, l1, r1), that@LongMap.Bin(p2, m2, l2, r2)) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) LongMap.Nil
        else if (zero(p2, m1)) l1.intersectionWith(that, f)
        else r1.intersectionWith(that, f)
      } else if (m1 == m2) bin(p1, m1, l1.intersectionWith(l2, f), r1.intersectionWith(r2, f))
      else {
        if (!hasMatch(p1, p2, m2)) LongMap.Nil
        else if (zero(p1, m2)) this.intersectionWith(l2, f)
        else this.intersectionWith(r2, f)
      }
    case (LongMap.Tip(key, value), that) => that.get(key) match {
      case None => LongMap.Nil
      case Some(value2) => LongMap.Tip(key, f(key, value, value2))
    }
    case (_, LongMap.Tip(key, value)) => this.get(key) match {
      case None => LongMap.Nil
      case Some(value2) => LongMap.Tip(key, f(key, value2, value))
    }
    case (_, _) => LongMap.Nil
  }

  /**
    * Left biased intersection. Returns the map that has all the same mappings as this but only for keys
    * which are present in the other map.
    *
    * @tparam R      The type of values in `that`.
    * @param that    The map to intersect with.
    * @return        A map with all the keys both in `this` and `that`, mapped to corresponding values from `this`.
    */
  def intersection[R](that: LongMap[R]): LongMap[T] =
    this.intersectionWith(that, (key: Long, value: T, value2: R) => value)

  def ++[S >: T](that: LongMap[S]) =
    this.unionWith[S](that, (key, x, y) => y)

  @tailrec
  final def firstKey: Long = this match {
    case LongMap.Bin(_, _, l, r) => l.firstKey
    case LongMap.Tip(k, v) => k
    case LongMap.Nil => throw new IllegalStateException("Empty set")
  }

  @tailrec
  final def lastKey: Long = this match {
    case LongMap.Bin(_, _, l, r) => r.lastKey
    case LongMap.Tip(k , v) => k
    case LongMap.Nil => throw new IllegalStateException("Empty set")
  }

  def map[V2](f: ((Long, T)) => (Long, V2)): LongMap[V2] = LongMap.from(new View.Map(coll, f))

  def flatMap[V2](f: ((Long, T)) => IterableOnce[(Long, V2)]): LongMap[V2] = LongMap.from(new View.FlatMap(coll, f))

  override def concat[V1 >: T](that: scala.collection.IterableOnce[(Long, V1)]): LongMap[V1] =
    super.concat(that).asInstanceOf[LongMap[V1]] // Already has correct type but not declared as such

  override def ++ [V1 >: T](that: scala.collection.IterableOnce[(Long, V1)]): LongMap[V1] = concat(that)

  def collect[V2](pf: PartialFunction[(Long, T), (Long, V2)]): LongMap[V2] =
    strictOptimizedCollect(LongMap.newBuilder[V2], pf)

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(LongMap.toFactory[T](LongMap), this)
}
