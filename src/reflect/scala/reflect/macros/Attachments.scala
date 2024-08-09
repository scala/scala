/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package macros

import reflect.internal.util.Position
import scala.runtime.ClassValueCompat

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  Attachments provide a way to associate custom metadata with symbols and trees.
 *
 *  Along with `symbol` and `tpe`, which represent core metadata of trees, each tree
 *  carries the `attachments` field that can store other metadata: compiler-defined (e.g. positions) or user-defined.
 *  Same story is true for symbols, which also have extensible metadata by the virtue
 *  of the same `attachments` field.
 *
 *  Typically attachments just store a [[scala.reflect.api.Position]], but they can be extended to
 *  encompass arbitrary payloads. Payloads are stored in type-indexed slots, which can be read with `get[T]` and written
 *  with `update[T]` and `remove[T]`.
 *
 *  This API doesn't have much use in the runtime reflection API (the [[scala.reflect.api]] package), but it might be of help
 *  for macro writers, providing a way to coordinate multiple macros operating on the same code. Therefore the `attachments`
 *  field is only declared in trees and symbols belonging to [[scala.reflect.macros.Universe]].
 */
abstract class Attachments { self =>

  /** The position type of this attachment */
  type Pos >: Null // <: api.Position

  /** The underlying position */
  def pos: Pos

  /** Creates a copy of this attachment with the position replaced by `newPos` */
  def withPos(newPos: Pos): Attachments { type Pos = self.Pos }

  /** The underlying payload with the guarantee that no two elements have the same type. */
  def all: Set[Any]

  private def matchesTag[T: ClassTag]: (Any => Boolean) = {
    // OPT: avoid lambda allocation for each call to `remove`, etc.
    Attachments.matchesTagCache.get(classTag[T].runtimeClass)
  }

  /** An underlying payload of the given class type `T`. */
  def get[T: ClassTag]: Option[T] = {
    val it = all.iterator
    val matchesTagFn = matchesTag[T]
    while (it.hasNext) { // OPT: hotspot, hand roll `Set.find`.
      val datum = it.next()
      if (matchesTagFn(datum)) return Some(datum.asInstanceOf[T])
    }
    None
  }

  /** Check underlying payload contains an instance of type `T`. */
  def contains[T: ClassTag]: Boolean = !isEmpty && {
    val it = all.iterator
    val matchesTagFn = matchesTag[T]
    while (it.hasNext) { // OPT: hotspot, hand roll `Set.exists`.
      val datum = it.next()
      if (matchesTagFn(datum)) return true
    }
    false
  }

  /** Creates a copy of this attachment with the payload slot of T added/updated with the provided value.
   *  Replaces an existing payload of the same type, if exists.
   */
  def update[T: ClassTag](attachment: T): Attachments { type Pos = self.Pos } =
    new NonemptyAttachments[Pos](this.pos, remove[T].all + attachment)

  /** Creates a copy of this attachment with the payload of the given class type `T` removed. */
  def remove[T: ClassTag]: Attachments { type Pos = self.Pos } = {
    if (!all.exists(matchesTag[T])) this // OPT immutable.Set.filter doesn't structurally share on 2.12 collections.
    else {
      val newAll = all filterNot matchesTag[T]
      if (newAll.isEmpty) pos.asInstanceOf[Attachments { type Pos = self.Pos }]
      else if (newAll.size == 1) new SingleAttachment[Pos](pos, newAll.head)
      else new NonemptyAttachments[Pos](this.pos, newAll)
    }
  }
  /** Creates a copy of this attachment with the given element removed. */
  final def removeElement[T](attachment: T): Attachments { type Pos = self.Pos } = {
    val newAll = all - attachment
    if (newAll eq all) this
    else if (newAll.isEmpty) pos.asInstanceOf[Attachments { type Pos = self.Pos }]
    else new NonemptyAttachments[Pos](this.pos, newAll)
  }

  /** Creates a copy of this attachment with the given element added. */
  final def addElement[T](attachment: T): Attachments { type Pos = self.Pos } = {
    val newAll = all + attachment
    if (newAll eq all) this // i.e., this was the same attachment as before
    else new NonemptyAttachments[Pos](this.pos, newAll)
  }

  /** Tests if the given element is attached. */
  final def containsElement(element: Any): Boolean = {
    all.contains(element)
  }

  def isEmpty: Boolean
  def cloneAttachments: Attachments { type Pos = self.Pos } = this
}

private object Attachments {
  private val matchesTagCache = new ClassValueCompat[Function1[Any, Boolean]] {
    override def computeValue(cls: Class[_]): Function[Any, Boolean] = cls.isInstance(_)
  }
}

private[reflect] abstract class EmptyAttachments extends Attachments { self: Position =>
  final override def all: Set[Any] = Set.empty
  final override def get[T: ClassTag]: Option[T] = None
  final override def contains[T: ClassTag]: Boolean = false
  final override def update[T: ClassTag](newAtt: T): Attachments { type Pos = self.Pos } =
    new SingleAttachment[Pos](pos, newAtt)
  final override def remove[T: ClassTag]: Attachments { type Pos = self.Pos } = this
  final override def isEmpty: Boolean = true
}

private final class SingleAttachment[P >: Null](override val pos: P, val att: Any) extends Attachments {
  type Pos = P
  def withPos(newPos: Pos) = new SingleAttachment[Pos](newPos, att)
  override def isEmpty: Boolean = false
  override def cloneAttachments: Attachments { type Pos = P } = new SingleAttachment[P](pos, att)
  override def all = Set.empty[Any] + att
  override def contains[T](implicit tt: ClassTag[T]) = tt.runtimeClass.isInstance(att)
  override def get[T](implicit tt: ClassTag[T]) = if (contains(tt)) Some(att.asInstanceOf[T]) else None
  override def update[T](newAtt: T)(implicit tt: ClassTag[T]) =
    if (contains(tt)) new SingleAttachment[P](pos, newAtt)
    else new NonemptyAttachments[P](pos, Set.empty[Any] + att + newAtt)
  override def remove[T](implicit tt: ClassTag[T]) =
    if (contains(tt)) pos.asInstanceOf[Attachments { type Pos = P }] else this
  override def toString = s"SingleAttachment at $pos: $att"
}

// scala/bug#7018: This used to be an inner class of `Attachments`, but that led to a memory leak in the
// IDE via $outer pointers.
private final class NonemptyAttachments[P >: Null](override val pos: P, override val all: Set[Any]) extends Attachments {
  type Pos = P
  def withPos(newPos: Pos) = new NonemptyAttachments(newPos, all)
  override def isEmpty: Boolean = false
  override def cloneAttachments: Attachments { type Pos = P } = new NonemptyAttachments[P](pos, all)
}
