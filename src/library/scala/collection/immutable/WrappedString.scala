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

import scala.Predef.{wrapString => _, assert}
import scala.collection.Stepper.EfficientSplit
import scala.collection.convert.impl.CharStringStepper
import scala.collection.mutable.{Builder, StringBuilder}

/**
  *  This class serves as a wrapper augmenting `String`s with all the operations
  *  found in indexed sequences.
  *
  *  The difference between this class and `StringOps` is that calling transformer
  *  methods such as `filter` and `map` will yield an object of type `WrappedString`
  *  rather than a `String`.
  *
  *  @param self    a string contained within this wrapped string
  *
  *  @define Coll `WrappedString`
  *  @define coll wrapped string
  */
@SerialVersionUID(3L)
final class WrappedString(private val self: String) extends AbstractSeq[Char] with IndexedSeq[Char]
  with IndexedSeqOps[Char, IndexedSeq, WrappedString]
  with Serializable {

  def apply(i: Int): Char = self.charAt(i)

  override protected def fromSpecific(coll: scala.collection.IterableOnce[Char]): WrappedString = WrappedString.fromSpecific(coll)
  override protected def newSpecificBuilder: Builder[Char, WrappedString] = WrappedString.newBuilder
  override def empty: WrappedString = WrappedString.empty

  override def slice(from: Int, until: Int): WrappedString = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= self.length)
      return WrappedString.empty

    val end = if (until > length) length else until
    new WrappedString(self.substring(start, end))
  }
  override def length = self.length
  override def toString = self
  override def view: StringView = new StringView(self)

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Char, S]): S with EfficientSplit = {
    val st = new CharStringStepper(self, 0, self.length)
    val r =
      if (shape.shape == StepperShape.CharShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S with EfficientSplit]
  }

  override def startsWith[B >: Char](that: IterableOnce[B], offset: Int = 0): Boolean =
    that match {
      case s: WrappedString => self.startsWith(s.self, offset)
      case _                => super.startsWith(that, offset)
    }

  override def endsWith[B >: Char](that: collection.Iterable[B]): Boolean =
    that match {
      case s: WrappedString => self.endsWith(s.self)
      case _                => super.endsWith(that)
    }

  override def indexOf[B >: Char](elem: B, from: Int = 0): Int = elem match {
    case c: Char => self.indexOf(c, from)
    case _       => super.indexOf(elem, from)
  }

  override def lastIndexOf[B >: Char](elem: B, end: Int = length - 1): Int =
    elem match {
      case c: Char => self.lastIndexOf(c, end)
      case _       => super.lastIndexOf(elem, end)
    }

  override def copyToArray[B >: Char](xs: Array[B], start: Int, len: Int): Int =
    (xs: Any) match {
      case chs: Array[Char] =>
        val copied = IterableOnce.elemsToCopyToArray(length, chs.length, start, len)
        self.getChars(0, copied, chs, start)
        copied
      case _                => super.copyToArray(xs, start, len)
    }

  override def appendedAll[B >: Char](suffix: IterableOnce[B]): IndexedSeq[B] =
    suffix match {
      case s: WrappedString => new WrappedString(self concat s.self)
      case _                => super.appendedAll(suffix)
    }

  override def sameElements[B >: Char](o: IterableOnce[B]) = o match {
    case s: WrappedString => self == s.self
    case _                => super.sameElements(o)
  }

  override protected[this] def className = "WrappedString"

  override protected final def applyPreferredMaxLength: Int = Int.MaxValue
  override def equals(other: Any): Boolean = other match {
    case that: WrappedString =>
      this.self == that.self
    case _ =>
      super.equals(other)
  }
}

/** A companion object for wrapped strings.
  */
@SerialVersionUID(3L)
object WrappedString extends SpecificIterableFactory[Char, WrappedString] {
  def fromSpecific(it: IterableOnce[Char]): WrappedString = {
    val b = newBuilder
    b.sizeHint(it)
    b ++= it
    b.result()
  }
  val empty: WrappedString = new WrappedString("")
  def newBuilder: Builder[Char, WrappedString] =
    new StringBuilder().mapResult(x => new WrappedString(x))

  implicit class UnwrapOp(private val value: WrappedString) extends AnyVal {
    def unwrap: String = value.self
  }
}
