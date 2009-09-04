/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ArrayVector.scala 18589 2009-08-27 14:45:35Z odersky $


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
abstract class ArrayVector[A] extends Vector[A] with VectorTemplate[A, ArrayVector[A]] { self =>

  /** The manifest of the element type */
  def elemManifest: ClassManifest[A]

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): A

  /** Update element at given index */
  def update(index: Int, elem: A): Unit

  /** The underlying array */
  def value: AnyRef

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[A, ArrayVector[A]] =
    new ArrayVectorBuilder[A](elemManifest)
}
