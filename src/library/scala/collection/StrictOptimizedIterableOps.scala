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

package scala
package collection

import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance
import scala.runtime.Statics

/**
  * Trait that overrides iterable operations to take advantage of strict builders.
  *
  * @tparam A  Elements type
  * @tparam CC Collection type constructor
  * @tparam C  Collection type
  */
trait StrictOptimizedIterableOps[+A, +CC[_], +C]
  extends Any
    with IterableOps[A, CC, C] {

  // Optimized, push-based version of `partition`
  override def partition(p: A => Boolean): (C, C) = {
    val l, r = newSpecificBuilder
    iterator.foreach(x => (if (p(x)) l else r) += x)
    (l.result(), r.result())
  }

  override def span(p: A => Boolean): (C, C) = {
    val first = newSpecificBuilder
    val second = newSpecificBuilder
    val it = iterator
    var inFirst = true
    while (it.hasNext && inFirst) {
      val a = it.next()
      if (p(a)) {
        first += a
      } else {
        second += a
        inFirst = false
      }
    }
    while (it.hasNext) {
      second += it.next()
    }
    (first.result(), second.result())
  }

  override def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
    val first = iterableFactory.newBuilder[A1]
    val second = iterableFactory.newBuilder[A2]
    foreach { a =>
      val pair = asPair(a)
      first += pair._1
      second += pair._2
    }
    (first.result(), second.result())
  }

  override def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3]) = {
    val b1 = iterableFactory.newBuilder[A1]
    val b2 = iterableFactory.newBuilder[A2]
    val b3 = iterableFactory.newBuilder[A3]

    foreach { xyz =>
      val triple = asTriple(xyz)
      b1 += triple._1
      b2 += triple._2
      b3 += triple._3
    }
    (b1.result(), b2.result(), b3.result())
  }

  // The implementations of the following operations are not fundamentally different from
  // the view-based implementations, but they turn out to be slightly faster because
  // a couple of indirection levels are removed

  override def map[B](f: A => B): CC[B] =
    strictOptimizedMap(iterableFactory.newBuilder, f)

  /**
    * @param b Builder to use to build the resulting collection
    * @param f Element transformation function
    * @tparam B Type of elements of the resulting collection (e.g. `String`)
    * @tparam C2 Type of the resulting collection (e.g. `List[String]`)
    * @return The resulting collection
    */
  @inline protected[this] final def strictOptimizedMap[B, C2](b: mutable.Builder[B, C2], f: A => B): C2 = {
    val it = iterator
    while (it.hasNext) {
      b += f(it.next())
    }
    b.result()
  }

  override def flatMap[B](f: A => IterableOnce[B]): CC[B] =
    strictOptimizedFlatMap(iterableFactory.newBuilder, f)

  /**
    * @param b Builder to use to build the resulting collection
    * @param f Element transformation function
    * @tparam B Type of elements of the resulting collection (e.g. `String`)
    * @tparam C2 Type of the resulting collection (e.g. `List[String]`)
    * @return The resulting collection
    */
  @inline protected[this] final def strictOptimizedFlatMap[B, C2](b: mutable.Builder[B, C2], f: A => IterableOnce[B]): C2 = {
    val it = iterator
    while (it.hasNext) {
      b ++= f(it.next())
    }
    b.result()
  }

  /**
    * @param that Elements to concatenate to this collection
    * @param b Builder to use to build the resulting collection
    * @tparam B Type of elements of the resulting collections (e.g. `Int`)
    * @tparam C2 Type of the resulting collection (e.g. `List[Int]`)
    * @return The resulting collection
    */
  @inline protected[this] final def strictOptimizedConcat[B >: A, C2](that: IterableOnce[B], b: mutable.Builder[B, C2]): C2 = {
    b ++= this
    b ++= that
    b.result()
  }

  override def collect[B](pf: PartialFunction[A, B]): CC[B] =
    strictOptimizedCollect(iterableFactory.newBuilder, pf)

  /**
    * @param b Builder to use to build the resulting collection
    * @param pf Element transformation partial function
    * @tparam B Type of elements of the resulting collection (e.g. `String`)
    * @tparam C2 Type of the resulting collection (e.g. `List[String]`)
    * @return The resulting collection
    */
  @inline protected[this] final def strictOptimizedCollect[B, C2](b: mutable.Builder[B, C2], pf: PartialFunction[A, B]): C2 = {
    val marker = Statics.pfMarker
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      val v = pf.applyOrElse(elem, ((x: A) => marker).asInstanceOf[Function[A, B]])
      if (marker ne v.asInstanceOf[AnyRef]) b += v
    }
    b.result()
  }

  override def flatten[B](implicit toIterableOnce: A => IterableOnce[B]): CC[B] =
    strictOptimizedFlatten(iterableFactory.newBuilder)

  /**
    * @param b Builder to use to build the resulting collection
    * @param toIterableOnce Evidence that `A` can be seen as an `IterableOnce[B]`
    * @tparam B Type of elements of the resulting collection (e.g. `Int`)
    * @tparam C2 Type of the resulting collection (e.g. `List[Int]`)
    * @return The resulting collection
    */
  @inline protected[this] final def strictOptimizedFlatten[B, C2](b: mutable.Builder[B, C2])(implicit toIterableOnce: A => IterableOnce[B]): C2 = {
    val it = iterator
    while (it.hasNext) {
      b ++= toIterableOnce(it.next())
    }
    b.result()
  }

  override def zip[B](that: IterableOnce[B]): CC[(A @uncheckedVariance, B)] =
    strictOptimizedZip(that, iterableFactory.newBuilder[(A, B)])

  /**
    * @param that Collection to zip with this collection
    * @param b Builder to use to build the resulting collection
    * @tparam B Type of elements of the second collection (e.g. `String`)
    * @tparam C2 Type of the resulting collection (e.g. `List[(Int, String)]`)
    * @return The resulting collection
    */
  @inline protected[this] final def strictOptimizedZip[B, C2](that: IterableOnce[B], b: mutable.Builder[(A, B), C2]): C2 = {
    val it1 = iterator
    val it2 = that.iterator
    while (it1.hasNext && it2.hasNext) {
      b += ((it1.next(), it2.next()))
    }
    b.result()
  }

  override def zipWithIndex: CC[(A @uncheckedVariance, Int)] = {
    val b = iterableFactory.newBuilder[(A, Int)]
    var i = 0
    val it = iterator
    while (it.hasNext) {
      b += ((it.next(), i))
      i += 1
    }
    b.result()
  }

  override def scanLeft[B](z: B)(op: (B, A) => B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b.sizeHint(this, delta = 0)
    var acc = z
    b += acc
    val it = iterator
    while (it.hasNext) {
      acc = op(acc, it.next())
      b += acc
    }
    b.result()
  }

  override def filter(pred: A => Boolean): C = filterImpl(pred, isFlipped = false)

  override def filterNot(pred: A => Boolean): C = filterImpl(pred, isFlipped = true)

  protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): C = {
    val b = newSpecificBuilder
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      if (pred(elem) != isFlipped) {
        b += elem
      }
    }
    b.result()
  }

  // Optimized, push-based version of `partitionMap`
  override def partitionMap[A1, A2](f: A => Either[A1, A2]): (CC[A1], CC[A2]) = {
    val l = iterableFactory.newBuilder[A1]
    val r = iterableFactory.newBuilder[A2]
    foreach { x =>
      f(x) match {
        case Left(x1) => l += x1
        case Right(x2) => r += x2
      }
    }
    (l.result(), r.result())
  }

  // Optimization avoids creation of second collection
  override def tapEach[U](f: A => U): C  = {
    foreach(f)
    coll
  }

  /** A collection containing the last `n` elements of this collection.
    * $willForceEvaluation
    */
  override def takeRight(n: Int): C = {
    val b = newSpecificBuilder
    b.sizeHintBounded(n, toIterable: @nowarn("cat=deprecation"))
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      lead.next()
      it.next()
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  /** The rest of the collection without its `n` last elements. For
    *  linear, immutable collections this should avoid making a copy.
    *  $willForceEvaluation
    */
  override def dropRight(n: Int): C = {
    val b = newSpecificBuilder
    if (n >= 0) b.sizeHint(this, delta = -n)
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      b += it.next()
      lead.next()
    }
    b.result()
  }
}
