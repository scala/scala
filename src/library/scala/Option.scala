/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import Predef._
import annotation.experimental
import collection.{Iterator, Iterable}
import collection.immutable.List

object Option
{
  /** An implicit conversion that converts an option to an iterable value
   */
  implicit def option2Iterable[A](xo: Option[A]): Iterable[A] = xo.toList

  /** An Option factory which creates Some(value) if the argument is not null,
   *  and None if it is null.
   *
   *  @param  x the value
   *  @return   Some(value) if value != null, None if value == null
   */
  @experimental
  def apply[A](x: A): Option[A] = if (x == null) None else Some(x)
}

/** This class represents optional values. Instances of <code>Option</code>
 *  are either instances of case class <code>Some</code> or it is case
 *  object <code>None</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.1, 16/01/2007
 */
sealed abstract class Option[+A] extends Product {

  /** True if the option is the <code>None</code> value, false otherwise.
   */
  def isEmpty: Boolean

  /** True if the option is a <code>Some</code>(...) false otherwise.
   */
  def isDefined: Boolean = !isEmpty

  /** get the value of this option.
   *  @requires that the option is nonEmpty.
   *  @throws Predef.NoSuchElementException if the option is empty.
   */
  def get: A

  @deprecated("use <code>getOrElse</code> instead")
  def get[B >: A](default: B): B = this match {
    case None => default
    case Some(x) => x
  }

  /** If the option is nonempty return its value,
   *  otherwise return the result of evaluating a default expression.
   *
   *  @param default  the default expression.
   */
  def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.get

  /** If the option is nonempty return its value,
   *  otherwise return the Zero for this type.
   */
  def orZero[B >: A](implicit z: Zero[B]): B =
    this getOrElse z.zero

  /** The option's value if it is nonempty, or <code>null</code> if it is empty.
   *  The use of null of course is discouraged, but code written to use Options
   *  often must interface with code which expects and returns nulls.
   */
  @experimental
  def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = this getOrElse null

  /** If the option is nonempty, return a function applied to its value,
   *  wrapped in a Some i.e. <code>Some(f(this.get))</code>.
   *  Otherwise return <code>None</code>.
   *
   *  @param  f   the function to apply
   */
  def map[B](f: A => B): Option[B] =
    if (isEmpty) None else Some(f(this.get))

  /** If the option is nonempty, return a function applied to its value.
   *  Otherwise return None.
   *  @param  f   the function to apply
   */
  def flatMap[B](f: A => Option[B]): Option[B] =
    if (isEmpty) None else f(this.get)

  /** If the option is nonempty and the given predicate <code>p</code>
   *  yields <code>false</code> on its value, return <code>None</code>.
   *  Otherwise return the option value itself.
   *
   *  @param  p   the predicate used for testing.
   */
  def filter(p: A => Boolean): Option[A] =
    if (isEmpty || p(this.get)) this else None

  /** If the option is nonempty, p(value), otherwise false.
   *
   *  @param  p   the predicate to test
   */
  def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /** Apply the given procedure <code>f</code> to the option's value,
   *  if it is nonempty. Do nothing if it is empty.
   *
   *  @param  f   the procedure to apply.
   */
  def foreach[U](f: A => U) {
    if (!isEmpty) f(this.get)
  }

  /** If the given partial function <code>pf</code> is defined for the
   *  option's value, apply it to the value.  Otherwise, None.
   *
   *  @param  pf   the partial function.
   */
  @experimental
  def filterMap[B](pf: PartialFunction[Any, B]): Option[B] =
    if (!isEmpty && pf.isDefinedAt(this.get)) Some(pf(this.get)) else None

  /** If the option is nonempty return it,
   *  otherwise return the result of evaluating an alternative expression.
   *  @param alternative  the alternative expression.
   */
  def orElse[B >: A](alternative: => Option[B]): Option[B] =
    if (isEmpty) alternative else this

  /** An singleton iterator returning the option's value if it is nonempty
   *  or the empty iterator if the option is empty.
   */
  def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(this.get)

  /** A singleton list containing the option's value if it is nonempty
   *  or the empty list if the option is empty.
   */
  def toList: List[A] =
    if (isEmpty) List() else List(this.get)

  /** An <code>Either</code> that is a <code>Left</code> with the given argument
   * <code>left</code> if this is empty, or a <code>Right</code> if this is nonempty with the
   * option's value.
   */
  def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(this.get)

  /** An <code>Either</code> that is a <code>Right</code> with the given argument
   * <code>right</code> if this is empty, or a <code>Left</code> if this is nonempty with the
   * option's value.
   */
  def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(this.get)
}

/** Class <code>Some[A]</code> represents existing values of type
 *  <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
final case class Some[+A](x: A) extends Option[A] {
  def isEmpty = false
  def get = x
}


/** This case object represents non-existent values.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
case object None extends Option[Nothing] {
  def isEmpty = true
  def get = throw new NoSuchElementException("None.get")
}
