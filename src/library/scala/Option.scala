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

object Option {

  import scala.language.implicitConversions

  /** An implicit conversion that converts an option to an iterable value */
  implicit def option2Iterable[A](xo: Option[A]): Iterable[A] =
    if (xo.isEmpty) Iterable.empty else Iterable.single(xo.get)

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

  /** When a given condition is true, evaluates the `a` argument and returns
   *  Some(a). When the condition is false, `a` is not evaluated and None is
   *  returned.
   */
  def when[A](cond: Boolean)(a: => A): Option[A] =
    if (cond) Some(a) else None

  /** Unless a given condition is true, this will evaluate the `a` argument and
   *  return Some(a). Otherwise, `a` is not evaluated and None is returned.
   */
  @inline def unless[A](cond: Boolean)(a: => A): Option[A] =
    when(!cond)(a)
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
 * These are useful methods that exist for both $some and $none.
 *  - [[isDefined]] — True if not empty
 *  - [[isEmpty]] — True if empty
 *  - [[nonEmpty]] — True if not empty
 *  - [[orElse]] — Evaluate and return alternate optional value if empty
 *  - [[getOrElse]] — Evaluate and return alternate value if empty
 *  - [[get]] — Return value, throw exception if empty
 *  - [[fold]] —  Apply function on optional value, return default if empty
 *  - [[map]] — Apply a function on the optional value
 *  - [[flatMap]] — Same as map but function must return an optional value
 *  - [[foreach]] — Apply a procedure on option value
 *  - [[collect]] — Apply partial pattern match on optional value
 *  - [[filter]] — An optional value satisfies predicate
 *  - [[filterNot]] — An optional value doesn't satisfy predicate
 *  - [[exists]] — Apply predicate on optional value, or false if empty
 *  - [[forall]] — Apply predicate on optional value, or true if empty
 *  - [[contains]] — Checks if value equals optional value, or false if empty
 *  - [[zip]] — Combine two optional values to make a paired optional value
 *  - [[unzip]] — Split an optional pair to two optional values
 *  - [[unzip3]] — Split an optional triple to three optional values
 *  - [[toList]] — Unary list of optional value, otherwise the empty list
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
 * Interacting with code that can occasionally return null can be
 * safely wrapped in $option to become $none and $some otherwise. {{{
 * val abc = new java.util.HashMap[Int, String]
 * abc.put(1, "A")
 * bMaybe = Option(abc.get(2))
 * bMaybe match {
 *   case Some(b) =>
 *     println(s"Found \$b")
 *   case None =>
 *     println("Not found")
 * }
 * }}}
 *
 *  @note Many of the methods in here are duplicative with those
 *  in the Iterable hierarchy, but they are duplicated for a reason:
 *  the implicit conversion tends to leave one with an Iterable in
 *  situations where one could have retained an Option.
 *
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
 */
@SerialVersionUID(-114498752079829388L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
sealed abstract class Option[+A] extends IterableOnce[A] with Product with Serializable {
  self =>

  /** Returns true if the option is $none, false otherwise.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(_) => false
   *   case None    => true
   * }
   * }}}
   */
  final def isEmpty: Boolean = this eq None

  /** Returns true if the option is an instance of $some, false otherwise.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(_) => true
   *   case None    => false
   * }
   * }}}
   */
  final def isDefined: Boolean = !isEmpty

  override final def knownSize: Int = if (isEmpty) 0 else 1

  /** Returns the option's value.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => x
   *   case None    => throw new Exception
   * }
   * }}}
   *  @note The option must be nonempty.
   *  @throws NoSuchElementException if the option is empty.
   */
  def get: A

  /** Returns the option's value if the option is nonempty, otherwise
   * return the result of evaluating `default`.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => x
   *   case None    => default
   * }
   * }}}
   *
   *  @param default  the default expression.
   */
  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.get

  /** Returns the option's value if it is nonempty,
   * or `null` if it is empty.
   *
   * Although the use of null is discouraged, code written to use
   * $option must often interface with code that expects and returns nulls.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => x
   *   case None    => null
   * }
   * }}}
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
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => Some(f(x))
   *   case None    => None
   * }
   * }}}
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
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => f(x)
   *   case None    => ifEmpty
   * }
   * }}}
   * This is also equivalent to:
   * {{{
   * option map f getOrElse ifEmpty
   * }}}
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
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => f(x)
   *   case None    => None
   * }
   * }}}
   *  @param  f   the function to apply
   *  @see map
   *  @see foreach
   */
  @inline final def flatMap[B](f: A => Option[B]): Option[B] =
    if (isEmpty) None else f(this.get)

  /** Returns the nested $option value if it is nonempty.  Otherwise,
   * return $none.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(Some(b)) => Some(b)
   *   case _             => None
   * }
   * }}}
   * @example {{{
   * Some(Some("something")).flatten
   * }}}
   *
   * @param ev an implicit conversion that asserts that the value is
   *           also an $option.
   * @see flatMap
   */
  def flatten[B](implicit ev: A <:< Option[B]): Option[B] =
    if (isEmpty) None else ev(this.get)

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns true. Otherwise, return $none.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) if p(x) => Some(x)
   *   case _               => None
   * }
   * }}}
   *  @param  p   the predicate used for testing.
   */
  @inline final def filter(p: A => Boolean): Option[A] =
    if (isEmpty || p(this.get)) this else None

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns false. Otherwise, return $none.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) if !p(x) => Some(x)
   *   case _                => None
   * }
   * }}}
   *  @param  p   the predicate used for testing.
   */
  @inline final def filterNot(p: A => Boolean): Option[A] =
    if (isEmpty || !p(this.get)) this else None

  /** Returns false if the option is $none, true otherwise.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(_) => true
   *   case None    => false
   * }
   * }}}
   *  @note   Implemented here to avoid the implicit conversion to Iterable.
   */
  final def nonEmpty: Boolean = isDefined

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
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => x == elem
   *   case None    => false
   * }
   * }}}
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
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => p(x)
   *   case None    => false
   * }
   * }}}
   *  @param  p   the predicate to test
   */
  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /** Returns true if this option is empty '''or''' the predicate
   * $p returns true when applied to this $option's value.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => p(x)
   *   case None    => true
   * }
   * }}}
   *  @param  p   the predicate to test
   */
  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(this.get)

  /** Apply the given procedure $f to the option's value,
   *  if it is nonempty. Otherwise, do nothing.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => f(x)
   *   case None    => ()
   * }
   * }}}
   *  @param  f   the procedure to apply.
   *  @see map
   *  @see flatMap
   */
  @inline final def foreach[U](f: A => U): Unit = {
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
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => Some(x)
   *   case None    => alternative
   * }
   * }}}
   *  @param alternative the alternative expression.
   */
  @inline final def orElse[B >: A](alternative: => Option[B]): Option[B] =
    if (isEmpty) alternative else this

  /** Returns a $some formed from this option and another option
   *  by combining the corresponding elements in a pair.
   *  If either of the two options is empty, $none is returned.
   *
   *  This is equivalent to:
   *  {{{
   *  (option1, option2) match {
   *    case (Some(x), Some(y)) => Some((x, y))
   *    case _                  => None
   *  }
   *  }}}
   *  @example {{{
   *  // Returns Some(("foo", "bar")) because both options are nonempty.
   *  Some("foo") zip Some("bar")
   *
   *  // Returns None because `that` option is empty.
   *  Some("foo") zip None
   *
   *  // Returns None because `this` option is empty.
   *  None zip Some("bar")
   *  }}}
   *
   *  @param  that   the options which is going to be zipped
   */
  final def zip[A1 >: A, B](that: Option[B]): Option[(A1, B)] =
    if (isEmpty || that.isEmpty) None else Some((this.get, that.get))

  /** Converts an Option of a pair into an Option of the first element and an Option of the second element.
    *
    *  This is equivalent to:
    *  {{{
    *  option match {
    *    case Some((x, y)) => (Some(x), Some(y))
    *    case _            => (None,    None)
    *  }
    *  }}}
    *  @tparam A1    the type of the first half of the element pair
    *  @tparam A2    the type of the second half of the element pair
    *  @param asPair an implicit conversion which asserts that the element type
    *                of this Option is a pair.
    *  @return       a pair of Options, containing, respectively, the first and second half
    *                of the element pair of this Option.
    */
  final def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (Option[A1], Option[A2]) = {
    if (isEmpty)
      (None, None)
    else {
      val e = asPair(this.get)
      (Some(e._1), Some(e._2))
    }
  }

  /** Converts an Option of a triple into three Options, one containing the element from each position of the triple.
    *
    *  This is equivalent to:
    *  {{{
    *  option match {
    *    case Some((x, y, z)) => (Some(x), Some(y), Some(z))
    *    case _               => (None,    None,    None)
    *  }
    *  }}}
    *  @tparam A1      the type of the first of three elements in the triple
    *  @tparam A2      the type of the second of three elements in the triple
    *  @tparam A3      the type of the third of three elements in the triple
    *  @param asTriple an implicit conversion which asserts that the element type
    *                  of this Option is a triple.
    *  @return         a triple of Options, containing, respectively, the first, second, and third
    *                  elements from the element triple of this Option.
    */
  final def unzip3[A1, A2, A3](implicit asTriple: A <:< (A1, A2, A3)): (Option[A1], Option[A2], Option[A3]) = {
    if (isEmpty)
      (None, None, None)
    else {
      val e = asTriple(this.get)
      (Some(e._1), Some(e._2), Some(e._3))
    }
  }

  /** Returns a singleton iterator returning the $option's value
   * if it is nonempty, or an empty iterator if the option is empty.
   */
  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(this.get)

  /** Returns a singleton list containing the $option's value
   * if it is nonempty, or the empty list if the $option is empty.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => List(x)
   *   case None    => Nil
   * }
   * }}}
   */
  def toList: List[A] =
    if (isEmpty) List() else new ::(this.get, Nil)

  /** Returns a [[scala.util.Left]] containing the given
   * argument `left` if this $option is empty, or
   * a [[scala.util.Right]] containing this $option's value if
   * this is nonempty.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => Right(x)
   *   case None    => Left(left)
   * }
   * }}}
   * @param left the expression to evaluate and return if this is empty
   * @see toLeft
   */
  @inline final def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(this.get)

  /** Returns a [[scala.util.Right]] containing the given
   * argument `right` if this is empty, or
   * a [[scala.util.Left]] containing this $option's value
   * if this $option is nonempty.
   *
   * This is equivalent to:
   * {{{
   * option match {
   *   case Some(x) => Left(x)
   *   case None    => Right(right)
   * }
   * }}}
   * @param right the expression to evaluate and return if this is empty
   * @see toRight
   */
  @inline final def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(this.get)
}

/** Class `Some[A]` represents existing values of type
 *  `A`.
 */
@SerialVersionUID(1234815782226070388L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
final case class Some[+A](value: A) extends Option[A] {
  def get: A = value
}


/** This case object represents non-existent values.
 */
@SerialVersionUID(5066590221178148012L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
case object None extends Option[Nothing] {
  def get: Nothing = throw new NoSuchElementException("None.get")
}
