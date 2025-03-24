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
package util

/** Represents a value of one of two possible types (a disjoint union).
 *  An instance of `Either` is an instance of either [[scala.util.Left]] or [[scala.util.Right]].
 *
 *  A common use of `Either` is as an alternative to [[scala.Option]] for dealing
 *  with possibly missing values.  In this usage, [[scala.None]] is replaced
 *  with a [[scala.util.Left]] which can contain useful information.
 *  [[scala.util.Right]] takes the place of [[scala.Some]].  Convention dictates
 *  that `Left` is used for failure and `Right` is used for success.
 *
 *  For example, you could use `Either[String, Int]` to indicate whether a
 *  received input is a `String` or an `Int`.
 *
 *  {{{
 *  import scala.io.StdIn._
 *  val in = readLine("Type Either a string or an Int: ")
 *  val result: Either[String,Int] =
 *    try Right(in.toInt)
 *    catch {
 *      case e: NumberFormatException => Left(in)
 *    }
 *
 *  result match {
 *    case Right(x) => s"You passed me the Int: \$x, which I will increment. \$x + 1 = \${x+1}"
 *    case Left(x)  => s"You passed me the String: \$x"
 *  }
 *  }}}
 *
 *  `Either` is right-biased, which means that `Right` is assumed to be the default case to
 *  operate on. If it is `Left`, operations like `map` and `flatMap` return the `Left` value unchanged:
 *
 *  {{{
 *  def doubled(i: Int) = i * 2
 *  Right(42).map(doubled) // Right(84)
 *  Left(42).map(doubled)  // Left(42)
 *  }}}
 *
 *  Since `Either` defines the methods `map` and `flatMap`, it can also be used in for comprehensions:
 *  {{{
 *  val right1 = Right(1)   : Right[Double, Int]
 *  val right2 = Right(2)
 *  val right3 = Right(3)
 *  val left23 = Left(23.0) : Left[Double, Int]
 *  val left42 = Left(42.0)
 *
 *  for {
 *    x <- right1
 *    y <- right2
 *    z <- right3
 *  } yield x + y + z // Right(6)
 *
 *  for {
 *    x <- right1
 *    y <- right2
 *    z <- left23
 *  } yield x + y + z // Left(23.0)
 *
 *  for {
 *    x <- right1
 *    y <- left23
 *    z <- right2
 *  } yield x + y + z // Left(23.0)
 *
 *  // Guard expressions are not supported:
 *  for {
 *    i <- right1
 *    if i > 0
 *  } yield i
 *  // error: value withFilter is not a member of Right[Double,Int]
 *
 *  // Similarly, refutable patterns are not supported:
 *  for (x: Int <- right1) yield x
 *  // error: value withFilter is not a member of Right[Double,Int]
 *
 *  // To use a filtered value, convert to an Option first,
 *  // which drops the Left case, as None contains no value:
 *  for {
 *    i <- right1.toOption
 *    if i > 0
 *  } yield i
 *
 *  }}}
 *
 *  Since `for` comprehensions use `map` and `flatMap`, the types
 *  of function parameters used in the expression must be inferred.
 *  These types are constrained by the `Either` values. In particular,
 *  because of right-biasing, `Left` values may require an explicit
 *  type argument for type parameter `B`, the right value. Otherwise,
 *  it might be inferred as `Nothing`.
 *
 *  {{{
 *  for {
 *    x <- left23
 *    y <- right1
 *    z <- left42  // type at this position: Either[Double, Nothing]
 *  } yield x + y + z
 *  //            ^
 *  // error: ambiguous reference to overloaded definition,
 *  // both method + in class Int of type (x: Char)Int
 *  // and  method + in class Int of type (x: Byte)Int
 *  // match argument types (Nothing)
 *
 *  for (x <- right2 ; y <- left23) yield x + y  // Left(23.0)
 *  for (x <- right2 ; y <- left42) yield x + y  // error
 *
 *  for {
 *    x <- right1
 *    y <- left42  // type at this position: Either[Double, Nothing]
 *    z <- left23
 *  } yield x + y + z
 *  // Left(42.0), but unexpectedly a `Either[Double,String]`
 *  }}}
 */
sealed abstract class Either[+A, +B] extends Product with Serializable {
  /** Projects this `Either` as a `Left`.
   *
   *  This allows for-comprehensions over the left side of `Either` instances,
   *  reversing `Either`'s usual right-bias.
   *
   *  For example {{{
   *  for (s <- Left("flower").left) yield s.length // Left(6)
   *  }}}
   *
   *  Continuing the analogy with [[scala.Option]], a `LeftProjection` declares
   *  that `Left` should be analogous to `Some` in some code.
   *
   *  {{{
   *  // using Option
   *  def interactWithDB(x: Query): Option[Result] =
   *    try Some(getResultFromDatabase(x))
   *    catch {
   *      case _: SQLException => None
   *    }
   *
   *  // this will only be executed if interactWithDB returns a Some
   *  val report = for (result <- interactWithDB(someQuery)) yield generateReport(result)
   *  report match {
   *    case Some(r) => send(r)
   *    case None    => log("report not generated, not sure why...")
   *  }
   *
   *  // using Either
   *  def interactWithDB(x: Query): Either[Exception, Result] =
   *    try Right(getResultFromDatabase(x))
   *    catch {
   *      case e: SQLException => Left(e)
   *    }
   *
   *   // run a report only if interactWithDB returns a Right
   *   val report = for (result <- interactWithDB(someQuery)) yield generateReport(result)
   *   report match {
   *     case Right(r) => send(r)
   *     case Left(e)  => log(s"report not generated, reason was \$e")
   *   }
   *   // only report errors
   *   for (e <- interactWithDB(someQuery).left) log(s"query failed, reason was \$e")
   *   }}}
   */
  def left = Either.LeftProjection(this)

  /** Projects this `Either` as a `Right`.
   *
   *  Because `Either` is right-biased, this method is not normally needed.
   */
  @deprecated("Either is now right-biased, use methods directly on Either", "2.13.0")
  def right = Either.RightProjection(this)

  /** Applies `fa` if this is a `Left` or `fb` if this is a `Right`.
   *
   *  @example {{{
   *  val result = util.Try("42".toInt).toEither
   *  result.fold(
   *    e => s"Operation failed with \$e",
   *    v => s"Operation produced value: \$v"
   *  )
   *  }}}
   *
   *  @param fa the function to apply if this is a `Left`
   *  @param fb the function to apply if this is a `Right`
   *  @return the results of applying the function
   */
  def fold[C](fa: A => C, fb: B => C): C = this match {
    case Right(b) => fb(b)
    case Left(a)  => fa(a)
  }

  /** If this is a `Left`, then return the left value in `Right` or vice versa.
   *
   *  @example {{{
   *  val left: Either[String, Int]  = Left("left")
   *  val right: Either[Int, String] = left.swap // Result: Right("left")
   *  }}}
   *  @example {{{
   *  val right = Right(2)
   *  val left  = Left(3)
   *  for {
   *    r1 <- right
   *    r2 <- left.swap
   *  } yield r1 * r2 // Right(6)
   *  }}}
   */
  def swap: Either[B, A] = this match {
    case Left(a)  => Right(a)
    case Right(b) => Left(b)
  }

  /** Joins an `Either` through `Right`.
   *
   *  This method requires that the right side of this `Either` is itself
   *  an `Either` type. That is, this must be some type like: {{{
   *  Either[A, Either[A, C]]
   *  }}} (which respects the type parameter bounds, shown below.)
   *
   *  If this instance is a `Right[Either[A, C]]` then the contained `Either[A, C]`
   *  will be returned, otherwise this value will be returned unmodified.
   *
   *  @example {{{
   *  Right[String, Either[String, Int]](Right(12)).joinRight // Result: Right(12)
   *  Right[String, Either[String, Int]](Left("flower")).joinRight // Result: Left("flower")
   *  Left[String, Either[String, Int]]("flower").joinRight // Result: Left("flower")
   *  }}}
   *
   * This method, and `joinLeft`, are analogous to `Option#flatten`
   */
  def joinRight[A1 >: A, B1 >: B, C](implicit ev: B1 <:< Either[A1, C]): Either[A1, C] = this match {
    case Right(b) => b
    case _        => this.asInstanceOf[Either[A1, C]]
  }

  /** Joins an `Either` through `Left`.
   *
   *  This method requires that the left side of this `Either` is itself an
   *  `Either` type. That is, this must be some type like: {{{
   *  Either[Either[C, B], B]
   *  }}} (which respects the type parameter bounds, shown below.)
   *
   *  If this instance is a `Left[Either[C, B]]` then the contained `Either[C, B]`
   *  will be returned, otherwise this value will be returned unmodified.
   *
   *  {{{
   *  Left[Either[Int, String], String](Right("flower")).joinLeft // Result: Right("flower")
   *  Left[Either[Int, String], String](Left(12)).joinLeft // Result: Left(12)
   *  Right[Either[Int, String], String]("daisy").joinLeft // Result: Right("daisy")
   *  }}}
   *
   *  This method, and `joinRight`, are analogous to `Option#flatten`.
   */
  def joinLeft[A1 >: A, B1 >: B, C](implicit ev: A1 <:< Either[C, B1]): Either[C, B1] = this match {
    case Left(a) => a
    case _       => this.asInstanceOf[Either[C, B1]]
  }

  /** Executes the given side-effecting function if this is a `Right`.
   *
   *  {{{
   *  Right(12).foreach(println) // prints "12"
   *  Left(12).foreach(println)  // doesn't print
   *  }}}
   *  @param f The side-effecting function to execute.
   */
  def foreach[U](f: B => U): Unit = this match {
    case Right(b) => f(b)
    case _        =>
  }

  /** Returns the value from this `Right` or the given argument if this is a `Left`.
   *
   *  {{{
   *  Right(12).getOrElse(17) // 12
   *  Left(12).getOrElse(17)  // 17
   *  }}}
   */
  def getOrElse[B1 >: B](or: => B1): B1 = this match {
    case Right(b) => b
    case _        => or
  }

  /** Returns this `Right` or the given argument if this is a `Left`.
   *
   *  {{{
   *  Right(1) orElse Left(2) // Right(1)
   *  Left(1) orElse Left(2)  // Left(2)
   *  Left(1) orElse Left(2) orElse Right(3) // Right(3)
   *  }}}
   */
  def orElse[A1 >: A, B1 >: B](or: => Either[A1, B1]): Either[A1, B1] = this match {
    case Right(_) => this
    case _        => or
  }

  /** Returns `true` if this is a `Right` and its value is equal to `elem` (as determined by `==`),
   *  returns `false` otherwise.
   *
   *  {{{
   *  // Returns true because value of Right is "something" which equals "something".
   *  Right("something") contains "something"
   *
   *  // Returns false because value of Right is "something" which does not equal "anything".
   *  Right("something") contains "anything"
   *
   *  // Returns false because it's not a Right value.
   *  Left("something") contains "something"
   *  }}}
   *
   *  @param elem    the element to test.
   *  @return `true` if this is a `Right` value equal to `elem`.
   */
  final def contains[B1 >: B](elem: B1): Boolean = this match {
    case Right(b) => b == elem
    case _        => false
  }

  /** Returns `true` if `Left` or returns the result of the application of
   *  the given predicate to the `Right` value.
   *
   *  {{{
   *  Right(12).forall(_ > 10)    // true
   *  Right(7).forall(_ > 10)     // false
   *  Left(12).forall(_ => false) // true
   *  }}}
   */
  def forall(f: B => Boolean): Boolean = this match {
    case Right(b) => f(b)
    case _        => true
  }

  /** Returns `false` if `Left` or returns the result of the application of
   *  the given predicate to the `Right` value.
   *
   *  {{{
   *  Right(12).exists(_ > 10)   // true
   *  Right(7).exists(_ > 10)    // false
   *  Left(12).exists(_ => true) // false
   *  }}}
   */
  def exists(p: B => Boolean): Boolean = this match {
    case Right(b) => p(b)
    case _        => false
  }

  /** Binds the given function across `Right`.
   *
   *  @param f The function to bind across `Right`.
   */
  def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = this match {
    case Right(b) => f(b)
    case _        => this.asInstanceOf[Either[A1, B1]]
  }


  /** Returns the right value if this is right
    * or this value if this is left
    *
    * @example {{{
    * val  l: Either[String, Either[String, Int]] = Left("pancake")
    * val rl: Either[String, Either[String, Int]] = Right(Left("flounder"))
    * val rr: Either[String, Either[String, Int]] = Right(Right(7))
    *
    *  l.flatten //Either[String, Int]: Left("pancake")
    * rl.flatten //Either[String, Int]: Left("flounder")
    * rr.flatten //Either[String, Int]: Right(7)
    * }}}
    *
    * Equivalent to `flatMap(id => id)`
    */
  def flatten[A1 >: A, B1](implicit ev: B <:< Either[A1, B1]): Either[A1, B1] = flatMap(ev)

  /** The given function is applied if this is a `Right`.
   *
   *  {{{
   *  Right(12).map(x => "flower") // Result: Right("flower")
   *  Left(12).map(x => "flower")  // Result: Left(12)
   *  }}}
   */
  def map[B1](f: B => B1): Either[A, B1] = this match {
    case Right(b) => Right(f(b))
    case _        => this.asInstanceOf[Either[A, B1]]
  }

  /** Returns `Right` with the existing value of `Right` if this is a `Right`
   *  and the given predicate `p` holds for the right value,
   *  or `Left(zero)` if this is a `Right` and the given predicate `p` does not hold for the right value,
   *  or `Left` with the existing value of `Left` if this is a `Left`.
   *
   * {{{
   * Right(12).filterOrElse(_ > 10, -1)   // Right(12)
   * Right(7).filterOrElse(_ > 10, -1)    // Left(-1)
   * Left(7).filterOrElse(_ => false, -1) // Left(7)
   * }}}
   */
  def filterOrElse[A1 >: A](p: B => Boolean, zero: => A1): Either[A1, B] = this match {
    case Right(b) if !p(b) => Left(zero)
    case _                 => this
  }

  /** Returns a `Seq` containing the `Right` value if
   *  it exists or an empty `Seq` if this is a `Left`.
   *
   * {{{
   * Right(12).toSeq // Seq(12)
   * Left(12).toSeq  // Seq()
   * }}}
   */
  def toSeq: collection.immutable.Seq[B] = this match {
    case Right(b) => collection.immutable.Seq(b)
    case _        => collection.immutable.Seq.empty
  }

  /** Returns a `Some` containing the `Right` value
   *  if it exists or a `None` if this is a `Left`.
   *
   * {{{
   * Right(12).toOption // Some(12)
   * Left(12).toOption  // None
   * }}}
   */
  def toOption: Option[B] = this match {
    case Right(b) => Some(b)
    case _        => None
  }

  def toTry(implicit ev: A <:< Throwable): Try[B] = this match {
    case Right(b) => Success(b)
    case Left(a)  => Failure(a)
  }

  /** Returns `true` if this is a `Left`, `false` otherwise.
   *
   *  {{{
   *  Left("tulip").isLeft // true
   *  Right("venus fly-trap").isLeft // false
   *  }}}
   */
  def isLeft: Boolean

  /** Returns `true` if this is a `Right`, `false` otherwise.
   *
   *  {{{
   *  Left("tulip").isRight // false
   *  Right("venus fly-trap").isRight // true
   *  }}}
   */
  def isRight: Boolean
}

/** The left side of the disjoint union, as opposed to the [[scala.util.Right]] side.
 */
final case class Left[+A, +B](value: A) extends Either[A, B] {
  def isLeft  = true
  def isRight = false

  /**
    * Upcasts this `Left[A, B]` to `Either[A, B1]`
    * {{{
    *   Left(1)                   // Either[Int, Nothing]
    *   Left(1).withRight[String] // Either[Int, String]
    * }}}
    */
  def withRight[B1 >: B]: Either[A, B1] = this

}

/** The right side of the disjoint union, as opposed to the [[scala.util.Left]] side.
 */
final case class Right[+A, +B](value: B) extends Either[A, B] {
  def isLeft  = false
  def isRight = true

  /**
    * Upcasts this `Right[A, B]` to `Either[A1, B]`
    * {{{
    *   Right("x")               // Either[Nothing, String]
    *   Right("x").withLeft[Int] // Either[Int, String]
    * }}}
    */
  def withLeft[A1 >: A]: Either[A1, B] = this

}

object Either {

  /** If the condition is satisfied, return the given `B` in `Right`,
   *  otherwise, return the given `A` in `Left`.
   *
   *  {{{
   *  val userInput: String = readLine()
   *  Either.cond(
   *    userInput.forall(_.isDigit) && userInput.size == 10,
   *    PhoneNumber(userInput),
   *    s"The input (\$userInput) does not look like a phone number"
   *  }}}
   */
  def cond[A, B](test: Boolean, right: => B, left: => A): Either[A, B] =
    if (test) Right(right) else Left(left)

  /** Allows use of a `merge` method to extract values from Either instances
   *  regardless of whether they are Left or Right.
   *
   *  {{{
   *  val l = Left(List(1)): Either[List[Int], Vector[Int]]
   *  val r = Right(Vector(1)): Either[List[Int], Vector[Int]]
   *  l.merge: Seq[Int] // List(1)
   *  r.merge: Seq[Int] // Vector(1)
   *  }}}
   */
  implicit class MergeableEither[A](private val x: Either[A, A]) extends AnyVal {
    def merge: A = x match {
      case Right(a) => a
      case Left(a)  => a
    }
  }

  /** Projects an `Either` into a `Left`.
   *
   *  @see [[scala.util.Either#left]]
   */
  final case class LeftProjection[+A, +B](e: Either[A, B]) {
    /** Returns the value from this `Left` or throws `NoSuchElementException`
     *  if this is a `Right`.
     *
     *  {{{
     *  Left(12).left.get  // 12
     *  Right(12).left.get // NoSuchElementException
     *  }}}
     *
     *  @throws NoSuchElementException if the projection is [[scala.util.Right]]
     */
    @deprecated("use `Either.swap.getOrElse` instead", "2.13.0")
    def get: A = e match {
      case Left(a) => a
      case _       => throw new NoSuchElementException("Either.left.get on Right")
    }

    /** Executes the given side-effecting function if this is a `Left`.
     *
     *  {{{
     *  Left(12).left.foreach(x => println(x))  // prints "12"
     *  Right(12).left.foreach(x => println(x)) // doesn't print
     *  }}}
     *  @param f The side-effecting function to execute.
     */
    def foreach[U](f: A => U): Unit = e match {
      case Left(a) => f(a)
      case _       => ()
    }

    /** Returns the value from this `Left` or the given argument if this is a `Right`.
     *
     *  {{{
     *  Left(12).left.getOrElse(17)  // 12
     *  Right(12).left.getOrElse(17) // 17
     *  }}}
     */
    def getOrElse[A1 >: A](or: => A1): A1 = e match {
      case Left(a) => a
      case _       => or
    }

    /** Returns `true` if `Right` or returns the result of the application of
     *  the given function to the `Left` value.
     *
     *  {{{
     *  Left(12).left.forall(_ > 10)  // true
     *  Left(7).left.forall(_ > 10)   // false
     *  Right(12).left.forall(_ > 10) // true
     *  }}}
     */
    def forall(p: A => Boolean): Boolean = e match {
      case Left(a) => p(a)
      case _       => true
    }

    /** Returns `false` if `Right` or returns the result of the application of
     *  the given function to the `Left` value.
     *
     *  {{{
     *  Left(12).left.exists(_ > 10)  // true
     *  Left(7).left.exists(_ > 10)   // false
     *  Right(12).left.exists(_ > 10) // false
     *  }}}
     */
    def exists(p: A => Boolean): Boolean = e match {
      case Left(a) => p(a)
      case _       => false
    }

    /** Binds the given function across `Left`.
     *
     *  {{{
     *  Left(12).left.flatMap(x => Left("scala")) // Left("scala")
     *  Right(12).left.flatMap(x => Left("scala")) // Right(12)
     *  }}}
     *  @param f The function to bind across `Left`.
     */
    def flatMap[A1, B1 >: B](f: A => Either[A1, B1]): Either[A1, B1] = e match {
      case Left(a) => f(a)
      case _       => e.asInstanceOf[Either[A1, B1]]
    }

    /** Maps the function argument through `Left`.
     *
     *  {{{
     *  Left(12).left.map(_ + 2) // Left(14)
     *  Right[Int, Int](12).left.map(_ + 2) // Right(12)
     *  }}}
     */
    def map[A1](f: A => A1): Either[A1, B] = e match {
      case Left(a) => Left(f(a))
      case _       => e.asInstanceOf[Either[A1, B]]
    }

    /** Returns `None` if this is a `Right` or if the given predicate
     *  `p` does not hold for the left value, otherwise, returns a `Left`.
     *
     *  {{{
     *  Left(12).left.filter(_ > 10)  // Some(Left(12))
     *  Left(7).left.filter(_ > 10)   // None
     *  Right(12).left.filter(_ > 10) // None
     *  }}}
     */
    @deprecated("Use `filterToOption`, which more accurately reflects the return type", "2.13.0")
    def filter[B1](p: A => Boolean): Option[Either[A, B1]] = e match {
      case x @ Left(a) if p(a) => Some(x.asInstanceOf[Either[A, B1]])
      case _                   => None
    }

    /** Returns `None` if this is a `Right` or if the given predicate
     *  `p` does not hold for the left value, otherwise, returns a `Left`.
     *
     *  {{{
     *  Left(12).left.filterToOption(_ > 10)  // Some(Left(12))
     *  Left(7).left.filterToOption(_ > 10)   // None
     *  Right(12).left.filterToOption(_ > 10) // None
     *  }}}
     */
    def filterToOption[B1](p: A => Boolean): Option[Either[A, B1]] = e match {
      case x @ Left(a) if p(a) => Some(x.asInstanceOf[Either[A, B1]])
      case _                   => None
    }

    /** Returns a `Seq` containing the `Left` value if it exists or an empty
     *  `Seq` if this is a `Right`.
     *
     *  {{{
     *  Left(12).left.toSeq // Seq(12)
     *  Right(12).left.toSeq // Seq()
     *  }}}
     */
    def toSeq: Seq[A] = e match {
      case Left(a) => Seq(a)
      case _       => Seq.empty
    }

    /** Returns a `Some` containing the `Left` value if it exists or a
     *  `None` if this is a `Right`.
     *
     *  {{{
     *  Left(12).left.toOption // Some(12)
     *  Right(12).left.toOption // None
     *  }}}
     */
    def toOption: Option[A] = e match {
      case Left(a) => Some(a)
      case _       => None
    }
  }

  /** Projects an `Either` into a `Right`.
   *
   *  Because `Either` is already right-biased, this class is not normally needed.
   *  (It is retained in the library for now for easy cross-compilation between Scala
   *  2.11 and 2.12.)
   */
  @deprecated("Either is now right-biased, calls to `right` should be removed", "2.13.0")
  final case class RightProjection[+A, +B](e: Either[A, B]) {

    /** Returns the value from this `Right` or throws
     *  `NoSuchElementException` if this is a `Left`.
     *
     *  {{{
     *  Right(12).right.get // 12
     *  Left(12).right.get // NoSuchElementException
     *  }}}
     *
     * @throws NoSuchElementException if the projection is `Left`.
     */
    @deprecated("Use `Either.toOption.get` instead", "2.13.0")
    def get: B = e match {
      case Right(b) => b
      case _        => throw new NoSuchElementException("Either.right.get on Left")
    }

    /** Executes the given side-effecting function if this is a `Right`.
     *
     *  {{{
     *  Right(12).right.foreach(x => println(x)) // prints "12"
     *  Left(12).right.foreach(x => println(x))  // doesn't print
     *  }}}
     *  @param f The side-effecting function to execute.
     */
    def foreach[U](f: B => U): Unit = e match {
      case Right(b) => f(b)
      case _        => ()
    }

    /** Returns the value from this `Right` or the given argument if this is a `Left`.
     *
     *  {{{
     *  Right(12).right.getOrElse(17) // 12
     *  Left(12).right.getOrElse(17)  // 17
     *  }}}
     */
    def getOrElse[B1 >: B](or: => B1): B1 = e match {
      case Right(b) => b
      case _        => or
    }

    /** Returns `true` if `Left` or returns the result of the application of
     *  the given function to the `Right` value.
     *
     *  {{{
     *  Right(12).right.forall(_ > 10) // true
     *  Right(7).right.forall(_ > 10)  // false
     *  Left(12).right.forall(_ > 10)  // true
     *  }}}
     */
    def forall(f: B => Boolean): Boolean = e match {
      case Right(b) => f(b)
      case _        => true
    }

    /** Returns `false` if `Left` or returns the result of the application of
     *  the given function to the `Right` value.
     *
     *  {{{
     *  Right(12).right.exists(_ > 10)  // true
     *  Right(7).right.exists(_ > 10)   // false
     *  Left(12).right.exists(_ > 10)   // false
     *  }}}
     */
    def exists(p: B => Boolean): Boolean = e match {
      case Right(b) => p(b)
      case _        => false
    }

    /** Binds the given function across `Right`.
     *
     *  @param f The function to bind across `Right`.
     */
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = e match {
      case Right(b) => f(b)
      case _        => e.asInstanceOf[Either[A1, B1]]
    }

    /** The given function is applied if this is a `Right`.
     *
     *  {{{
     *  Right(12).right.map(x => "flower") // Result: Right("flower")
     *  Left(12).right.map(x => "flower")  // Result: Left(12)
     *  }}}
     */
    def map[B1](f: B => B1): Either[A, B1] = e match {
      case Right(b) => Right(f(b))
      case _        => e.asInstanceOf[Either[A, B1]]
    }

    /** Returns `None` if this is a `Left` or if the
     *  given predicate `p` does not hold for the right value,
     *  otherwise, returns a `Right`.
     *
     * {{{
     * Right(12).right.filter(_ > 10) // Some(Right(12))
     * Right(7).right.filter(_ > 10)  // None
     * Left(12).right.filter(_ > 10)  // None
     * }}}
     */
    @deprecated("Use `filterToOption`, which more accurately reflects the return type", "2.13.0")
    def filter[A1](p: B => Boolean): Option[Either[A1, B]] = e match {
      case Right(b) if p(b) => Some(Right(b))
      case _                => None
    }

    /** Returns `None` if this is a `Left` or if the
     *  given predicate `p` does not hold for the right value,
     *  otherwise, returns a `Right`.
     *
     * {{{
     * Right(12).right.filterToOption(_ > 10) // Some(Right(12))
     * Right(7).right.filterToOption(_ > 10)  // None
     * Left(12).right.filterToOption(_ > 10)  // None
     * }}}
     */
    def filterToOption[A1](p: B => Boolean): Option[Either[A1, B]] = e match {
      case r @ Right(b) if p(b) => Some(r.asInstanceOf[Either[A1, B]])
      case _                    => None
    }

    /** Returns a `Seq` containing the `Right` value if
     *  it exists or an empty `Seq` if this is a `Left`.
     *
     * {{{
     * Right(12).right.toSeq // Seq(12)
     * Left(12).right.toSeq // Seq()
     * }}}
     */
    def toSeq: Seq[B] = e match {
      case Right(b) => Seq(b)
      case _        => Seq.empty
    }

    /** Returns a `Some` containing the `Right` value
     *  if it exists or a `None` if this is a `Left`.
     *
     * {{{
     * Right(12).right.toOption // Some(12)
     * Left(12).right.toOption // None
     * }}}
     */
    def toOption: Option[B] = e match {
      case Right(b) => Some(b)
      case _        => None
    }
  }
}
