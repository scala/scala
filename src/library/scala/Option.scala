/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

object Option {

  import scala.language.implicitConversions

  /** An implicit conversion that converts an option to an iterable value
   */
  implicit def option2Iterable[A](xo: Option[A]): Iterable[A] = xo.toList

  /** An Option factory which creates Some(x) if the argument is not null,
   *  and None if it is null.
   *
   *  @param  x the value
   *  @return   Some(value) if value != null, None if value == null
   */
  def apply[A](x: A): Option[A] = if (x == null) None else Some(x)

  /** An Option factory which returns `None` in a manner consistent with
   *  the collections hierarchy.
   */
  def empty[A] : Option[A] = None
}

/** Represents optional values. Instances of `Option`
 *  are either an instance of $some or the object $none.
 *
 *  The most idiomatic way to use an $option instance is to treat it
 *  as a collection or monad and use `map`,`flatMap`, `filter`, or
 *  `foreach`:
 *
 *  {{{
 *  val name: Option[String] = request getParameter "name"
 *  val upper = name map { _.trim } filter { _.length != 0 } map { _.toUpperCase }
 *  println(upper getOrElse "")
 *  }}}
 *
 *  Note that this is equivalent to {{{
 *  val upper = for {
 *    name <- request getParameter "name"
 *    trimmed <- Some(name.trim)
 *    upper <- Some(trimmed.toUpperCase) if trimmed.length != 0
 *  } yield upper
 *  println(upper getOrElse "")
 *  }}}
 *
 *  Because of how for comprehension works, if $none is returned
 *  from `request.getParameter`, the entire expression results in
 *  $none
 *
 *  This allows for sophisticated chaining of $option values without
 *  having to check for the existence of a value.
 *
 *  A less-idiomatic way to use $option values is via pattern matching: {{{
 *  val nameMaybe = request getParameter "name"
 *  nameMaybe match {
 *    case Some(name) =>
 *      println(name.trim.toUppercase)
 *    case None =>
 *      println("No name value")
 *  }
 *  }}}
 *
 *  @note Many of the methods in here are duplicative with those
 *  in the Traversable hierarchy, but they are duplicated for a reason:
 *  the implicit conversion tends to leave one with an Iterable in
 *  situations where one could have retained an Option.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.1, 16/01/2007
 *  @define none `None`
 *  @define some [[scala.Some]]
 *  @define option [[scala.Option]]
 *  @define p `p`
 *  @define f `f`
 *  @define coll option
 *  @define Coll `Option`
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define collectExample
 *  @define undefinedorder
 *  @define thatinfo the class of the returned collection. In the standard library configuration, `That` is `Iterable[B]`
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the result class `That` from the current
 *    representation type `Repr` and the new element type `B`.
 */
@SerialVersionUID(-114498752079829388L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
sealed abstract class Option[+A] extends Product with Serializable {
  self =>

  /** Returns true if the option is $none, false otherwise.
   */
  def isEmpty: Boolean

  /** Returns true if the option is an instance of $some, false otherwise.
   */
  def isDefined: Boolean = !isEmpty

  /** Returns the option's value.
   *  @note The option must be nonEmpty.
   *  @throws java.util.NoSuchElementException if the option is empty.
   */
  def get: A

  /** Returns the option's value if the option is nonempty, otherwise
   * return the result of evaluating `default`.
   *
   *  @param default  the default expression.
   */
  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.get

  /** Returns the option's value if it is nonempty,
   * or `null` if it is empty.
   * Although the use of null is discouraged, code written to use
   * $option must often interface with code that expects and returns nulls.
   * @example {{{
   * val initialText: Option[String] = getInitialText
   * val textField = new JComponent(initialText.orNull,20)
   * }}}
   */
  @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = this getOrElse ev(null)

  /** Returns a $some containing the result of applying $f to this $option's
   * value if this $option is nonempty.
   * Otherwise return $none.
   *
   *  @note This is similar to `flatMap` except here,
   *  $f does not need to wrap its result in an $option.
   *
   *  @param  f   the function to apply
   *  @see flatMap
   *  @see foreach
   */
  @inline final def map[B](f: A => B): Option[B] =
    if (isEmpty) None else Some(f(this.get))

  /** Returns the result of applying $f to this $option's
   *  value if the $option is nonempty.  Otherwise, evaluates
   *  expression `ifEmpty`.
   *
   *  @note This is equivalent to `$option map f getOrElse ifEmpty`.
   *
   *  @param  ifEmpty the expression to evaluate if empty.
   *  @param  f       the function to apply if nonempty.
   */
  @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(this.get)

  /** Returns the result of applying $f to this $option's value if
   * this $option is nonempty.
   * Returns $none if this $option is empty.
   * Slightly different from `map` in that $f is expected to
   * return an $option (which could be $none).
   *
   *  @param  f   the function to apply
   *  @see map
   *  @see foreach
   */
  @inline final def flatMap[B](f: A => Option[B]): Option[B] =
    if (isEmpty) None else f(this.get)

  def flatten[B](implicit ev: A <:< Option[B]): Option[B] =
    if (isEmpty) None else ev(this.get)

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns true. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  @inline final def filter(p: A => Boolean): Option[A] =
    if (isEmpty || p(this.get)) this else None

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns false. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  @inline final def filterNot(p: A => Boolean): Option[A] =
    if (isEmpty || !p(this.get)) this else None

  /** Returns false if the option is $none, true otherwise.
   *  @note   Implemented here to avoid the implicit conversion to Iterable.
   */
  final def nonEmpty = isDefined

  /** Necessary to keep $option from being implicitly converted to
   *  [[scala.collection.Iterable]] in `for` comprehensions.
   */
  @inline final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /** We need a whole WithFilter class to honor the "doesn't create a new
   *  collection" contract even though it seems unlikely to matter much in a
   *  collection with max size 1.
   */
  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Option[B] = self filter p map f
    def flatMap[B](f: A => Option[B]): Option[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /** Tests whether the option contains a given value as an element.
   *
   *  @example {{{
   *  // Returns true because Some instance contains string "something" which equals "something".
   *  Some("something") contains "something"
   *
   *  // Returns false because "something" != "anything".
   *  Some("something") contains "anything"
   *
   *  // Returns false when method called on None.
   *  None contains "anything"
   *  }}}
   *
   *  @param elem the element to test.
   *  @return `true` if the option has an element that is equal (as
   *  determined by `==`) to `elem`, `false` otherwise.
   */
  final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && this.get == elem

  /** Returns true if this option is nonempty '''and''' the predicate
   * $p returns true when applied to this $option's value.
   * Otherwise, returns false.
   *
   *  @param  p   the predicate to test
   */
  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /** Returns true if this option is empty '''or''' the predicate
   * $p returns true when applied to this $option's value.
   *
   *  @param  p   the predicate to test
   */
  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(this.get)

  /** Apply the given procedure $f to the option's value,
   *  if it is nonempty. Otherwise, do nothing.
   *
   *  @param  f   the procedure to apply.
   *  @see map
   *  @see flatMap
   */
  @inline final def foreach[U](f: A => U) {
    if (!isEmpty) f(this.get)
  }

  /** Returns a $some containing the result of
   * applying `pf` to this $option's contained
   * value, '''if''' this option is
   * nonempty '''and''' `pf` is defined for that value.
   * Returns $none otherwise.
   *
   *  @example {{{
   *  // Returns Some(HTTP) because the partial function covers the case.
   *  Some("http") collect {case "http" => "HTTP"}
   *
   *  // Returns None because the partial function doesn't cover the case.
   *  Some("ftp") collect {case "http" => "HTTP"}
   *
   *  // Returns None because the option is empty. There is no value to pass to the partial function.
   *  None collect {case value => value}
   *  }}}
   *
   *  @param  pf   the partial function.
   *  @return the result of applying `pf` to this $option's
   *  value (if possible), or $none.
   */
  @inline final def collect[B](pf: PartialFunction[A, B]): Option[B] =
    if (!isEmpty) pf.lift(this.get) else None

  /** Returns this $option if it is nonempty,
   *  otherwise return the result of evaluating `alternative`.
   *  @param alternative the alternative expression.
   */
  @inline final def orElse[B >: A](alternative: => Option[B]): Option[B] =
    if (isEmpty) alternative else this

  /** Returns a singleton iterator returning the $option's value
   * if it is nonempty, or an empty iterator if the option is empty.
   */
  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(this.get)

  /** Returns a singleton list containing the $option's value
   * if it is nonempty, or the empty list if the $option is empty.
   */
  def toList: List[A] =
    if (isEmpty) List() else new ::(this.get, Nil)

  /** Returns a [[scala.util.Left]] containing the given
   * argument `left` if this $option is empty, or
   * a [[scala.util.Right]] containing this $option's value if
   * this is nonempty.
   *
   * @param left the expression to evaluate and return if this is empty
   * @see toLeft
   */
  @inline final def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(this.get)

  /** Returns a [[scala.util.Right]] containing the given
   * argument `right` if this is empty, or
   * a [[scala.util.Left]] containing this $option's value
   * if this $option is nonempty.
   *
   * @param right the expression to evaluate and return if this is empty
   * @see toRight
   */
  @inline final def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(this.get)
}

/** Class `Some[A]` represents existing values of type
 *  `A`.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
@SerialVersionUID(1234815782226070388L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
final case class Some[+A](@deprecatedName('x, "2.12.0") value: A) extends Option[A] {
  def isEmpty = false
  def get = value

  @deprecated("Use .value instead.", "2.12.0") def x: A = value
}


/** This case object represents non-existent values.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
@SerialVersionUID(5066590221178148012L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
case object None extends Option[Nothing] {
  def isEmpty = true
  def get = throw new NoSuchElementException("None.get")
}
