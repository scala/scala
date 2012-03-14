/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import scala.reflect.ClassManifest
import generic.CanBuildFrom

/**
 *  A class representing `Array[T]`.
 *
 *  @tparam T    type of the elements in this wrapped array.
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 *  @since 2.8
 *  @define Coll WrappedArray
 *  @define coll wrapped array
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
abstract sealed class FlatArray[T]
extends AbstractSeq[T]
    with IndexedSeq[T]
    with IndexedSeqOptimized[T, FlatArray[T]]
{

  override protected[this] def thisCollection: FlatArray[T] = this
  override protected[this] def toCollection(repr: FlatArray[T]): FlatArray[T] = repr

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): T

  /** Update element at given index */
  def update(index: Int, elem: T): Unit

  override def stringPrefix = "FlatArray"

  override protected[this] def newBuilder: Builder[T, FlatArray[T]] = ??? // implemented in FlatArray.Impl

  /** Clones this object, including the underlying Array. */
  override def clone: FlatArray[T] = ??? // implemented in FlatArray.Impl
}


/** A companion object used to create instances of `WrappedArray`.
 */
object FlatArray {

  def empty[Boxed, Unboxed](elems: Boxed*)
      (implicit boxings: BoxingConversions[Boxed, Unboxed], elemManifest: ClassManifest[Unboxed]): FlatArray[Boxed] = apply()

  def apply[Boxed, Unboxed](elems: Boxed*)
      (implicit boxings: BoxingConversions[Boxed, Unboxed], elemManifest: ClassManifest[Unboxed]): FlatArray[Boxed] = {
    val b = newBuilder[Boxed, Unboxed]
    b.sizeHint(elems.length)
    b ++= elems
    b.result
  }

  def newBuilder[Boxed, Unboxed]
      (implicit boxings: BoxingConversions[Boxed, Unboxed], elemManifest: ClassManifest[Unboxed]): Builder[Boxed, FlatArray[Boxed]] =
    new Bldr[Boxed, Unboxed](boxings, elemManifest)

  implicit def canBuildFrom[Boxed, Unboxed](
    implicit 
      boxings: BoxingConversions[Boxed, Unboxed], 
      elemManifest: ClassManifest[Unboxed]): CanBuildFrom[FlatArray[_], Boxed, FlatArray[Boxed]] =
    new CanBuildFrom[FlatArray[_], Boxed, FlatArray[Boxed]] {
      def apply(from: FlatArray[_]): Builder[Boxed, FlatArray[Boxed]] =
        newBuilder[Boxed, Unboxed]
      def apply: Builder[Boxed, FlatArray[Boxed]] =
        newBuilder[Boxed, Unboxed]
    }

  private class Bldr[Boxed, Unboxed](boxings: BoxingConversions[Boxed, Unboxed], manifest: ClassManifest[Unboxed]) extends Builder[Boxed, FlatArray[Boxed]] {

    private var elems: Array[Unboxed] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def resize(size: Int) {
      val newelems = manifest.newArray(size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      elems = newelems
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Boxed): this.type = {
      ensureSize(size + 1)
      elems(size) = boxings.unbox(elem)
      size += 1
      this
    }

    def clear() {
      size = 0
    }

    def result(): FlatArray[Boxed] = {
      if (capacity == 0 || capacity != size) resize(size)
      new FlatArray.Impl(elems, boxings, manifest)
    }
  }

  private class Impl[Boxed, Unboxed](
    elems: Array[Unboxed], 
    boxings: BoxingConversions[Boxed, Unboxed],
    elemManifest: ClassManifest[Unboxed]) extends FlatArray[Boxed] {

    def length = elems.length

    def apply(idx: Int): Boxed = boxings.box(elems(idx))

    def update(idx: Int, elem: Boxed) = elems(idx) = boxings.unbox(elem)

    /** Creates new builder for this collection ==> move to subclasses
     */
    override protected[this] def newBuilder: Builder[Boxed, FlatArray[Boxed]] =
      new Bldr[Boxed, Unboxed](boxings, elemManifest)

    /** Clones this object, including the underlying Array. */
    override def clone: FlatArray[Boxed] = new Impl[Boxed, Unboxed](elems.clone(), boxings, elemManifest)
  }
}
