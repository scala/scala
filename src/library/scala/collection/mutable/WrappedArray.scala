/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: WrappedArray.scala 18589 2009-08-27 14:45:35Z odersky $


package scala.collection.mutable

import Predef._
import scala.reflect.ClassManifest
import collection.generic._

/**
 *  <p>A class representing <code>Array[T]</code></p>
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 */
abstract class WrappedArray[A] extends Vector[A] with VectorLike[A, WrappedArray[A]] with Proxy { self =>

  override protected[this] def thisCollection: WrappedArray[A] = this
  override protected[this] def toCollection(repr: WrappedArray[A]): WrappedArray[A] = repr

  /** The manifest of the element type */
  def elemManifest: ClassManifest[A]

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): A

  /** Update element at given index */
  def update(index: Int, elem: A): Unit

  /** The underlying array */
  def array: AnyRef

  /** The original of a proxy represented by a wrapped array */
  override def self = repr

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[A, WrappedArray[A]] =
    new WrappedArrayBuilder[A](elemManifest)
}
