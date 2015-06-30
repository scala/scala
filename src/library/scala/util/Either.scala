/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package util

import scala.language.implicitConversions

/** Represents a value of one of two possible types (a disjoint union.)
 *  Instances of Either are either an instance of [[scala.util.Left]] or [[scala.util.Right]].
 *
 *  A common use of Either is as an alternative to [[scala.Option]] for dealing
 *  with possible missing values.  In this usage, [[scala.None]] is replaced
 *  with a [[scala.util.Left]] which can contain useful information.
 *  [[scala.util.Right]] takes the place of [[scala.Some]].  Convention dictates
 *  that Left is used for failure and Right is used for success.
 *
 *  For example, you could use `Either[String, Int]` to detect whether a
 *  received input is a String or an Int.
 *
 *  {{{
 *  val in = Console.readLine("Type Either a string or an Int: ")
 *  val result: Either[String,Int] = try {
 *      Right(in.toInt)
 *    } catch {
 *      case e: Exception =>
 *        Left(in)
 *  }
 *
 *  println( result match {
 *    case Right(x) => "You passed me the Int: " + x + ", which I will increment. " + x + " + 1 = " + (x+1)
 *    case Left(x) => "You passed me the String: " + x
 *  })
 *  }}}
 * 
 * === Projections ===
 * 
 *  A ''projection'' can be used to selectively operate on a value of type Either,
 *  depending on whether it is of type Left or Right. For example, to transform an
 *  Either using a function, in the case where it's a Left, one can first apply
 *  the `left` projection and invoke `map` on that projected Either. If a `right`
 *  projection is applied to that Left, the original Left is returned, unmodified.
 *
 *  {{{
 *  val l: Either[String, Int] = Left("flower")
 *  val r: Either[String, Int] = Right(12)
 *  l.left.map(_.size): Either[Int, Int] // Left(6)
 *  r.left.map(_.size): Either[Int, Int] // Right(12)
 *  l.right.map(_.toDouble): Either[String, Double] // Left("flower")
 *  r.right.map(_.toDouble): Either[String, Double] // Right(12.0)
 *  }}}
 *
 *  Like with other types which define a `map` method, the same can be achieved
 *  using a for-comprehension:
 *  {{{
 *  for (s <- l.left) yield s.size // Left(6)
 *  }}}
 *
 * === Biasing Either values ===
 * 
 *  You may also ''bias'' `Either` values,
 *  decorating them so they can be operated upon and used in for comprehensions without
 *  extracting projections. If we treat `Either` as analogous to `Option`, a [[Either.RightBias$ right-biased]] `Either` treats `Right` instances as
 *  good values while `Left` instances represent failures, more descriptive variations on `None`. 
 *  (`Either` can also be [[Either.LeftBias$ left-biased]], but right-biasing is conventional and should
 *  be preferred.)
 *
 *  Biased `Either` values support pattern matches, variable assignments, and
 *  conditionals in for comprehensions, which left- and right-projections do not. 
 *  
 *  You can right-bias `Either` very simply via the following import statement:
 *  {{{ 
 *    import Either.RightBias._
 *
 *    val a : Either[String,Int] = Right(1)
 *    val b : Either[String,Int] = Right(99)
 * 
 *    for( v <- a; w <- b ) yield v+w          // Right(100)
 *    for( v <- a; w <- b if v > 10) yield v+w // throws NoSuchElementException
 *  }}}
 * 
 *  In order to support filter operations without throwing an Exception upon failure,
 *  a right bias may define a left-side token to indicate emptiness.
 *
 *  {{{
 *    val RightBias = Either.RightBias.withEmptyToken[String]("EMPTY")
 *    import RightBias._
 * 
 *    val a : Either[String,Int] = Right(1)
 *    val b : Either[String,Int] = Right(99)
 * 
 *    for( v <- a; w <- b ) yield v+w          // Right(100)
 *    for( v <- a; w <- b if v > 10) yield v+w // Left(EMPTY)
 *  }}}
 *
 *  For very extensive documentation of biased `Either`, please see [[Either.RightBias$ RightBias]]
 *  (or, if you wish to defy convention, [[Either.LeftBias$ LeftBias]]).
 * 
 *  For documentation of operations supported by biased `Either` values, see [[Either.RightBias.withEmptyToken.Ops right-biased ops]] / [[Either.RightBias.withEmptyToken.Ops left-biased ops]].
 * 
 *  @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 *  @version 1.0, 11/10/2008
 *  @since 2.7
 */
sealed abstract class Either[+A, +B] extends Product with Serializable {
  /**
   * Projects this `Either` as a `Left`.
   */
  def left = Either.LeftProjection(this)

  /**
   * Projects this `Either` as a `Right`.
   */
  def right = Either.RightProjection(this)

  /**
   * Applies `fa` if this is a `Left` or `fb` if this is a `Right`.
   *
   * @example {{{
   * val result: Either[Exception, Value] = possiblyFailingOperation()
   * log(result.fold(
   *   ex => "Operation failed with " + ex,
   *   v => "Operation produced value: " + v
   * ))
   * }}}
   *
   * @param fa the function to apply if this is a `Left`
   * @param fb the function to apply if this is a `Right`
   * @return the results of applying the function
   */
  def fold[X](fa: A => X, fb: B => X) = this match {
    case Left(a) => fa(a)
    case Right(b) => fb(b)
  }

  /**
   * If this is a `Left`, then return the left value in `Right` or vice versa.
   *
   * @example {{{
   * val l: Either[String, Int] = Left("left")
   * val r: Either[Int, String] = l.swap // Result: Right("left")
   * }}}
   */
  def swap = this match {
    case Left(a) => Right(a)
    case Right(b) => Left(b)
  }

  /**
   * Joins an `Either` through `Right`.
   *
   * This method requires that the right side of this Either is itself an
   * Either type. That is, this must be some type like: {{{
   * Either[A, Either[A, C]]
   * }}} (which respects the type parameter bounds, shown below.)
   *
   * If this instance is a Right[Either[A, C]] then the contained Either[A, C]
   * will be returned, otherwise this value will be returned unmodified.
   *
   * @example {{{
   * Right[String, Either[String, Int]](Right(12)).joinRight // Result: Right(12)
   * Right[String, Either[String, Int]](Left("flower")).joinRight // Result: Left("flower")
   * Left[String, Either[String, Int]]("flower").joinRight // Result: Left("flower")
   * }}}
   *
   * This method, and `joinLeft`, are analogous to `Option#flatten`
   */
  def joinRight[A1 >: A, B1 >: B, C](implicit ev: B1 <:< Either[A1, C]): Either[A1, C] = this match {
    case Left(a)  => Left(a)
    case Right(b) => b
  }

  /**
   * Joins an `Either` through `Left`.
   *
   * This method requires that the left side of this Either is itself an
   * Either type. That is, this must be some type like: {{{
   * Either[Either[C, B], B]
   * }}} (which respects the type parameter bounds, shown below.)
   *
   * If this instance is a Left[Either[C, B]] then the contained Either[C, B]
   * will be returned, otherwise this value will be returned unmodified.
   *
   * {{{
   * Left[Either[Int, String], String](Right("flower")).joinLeft // Result: Right("flower")
   * Left[Either[Int, String], String](Left(12)).joinLeft // Result: Left(12)
   * Right[Either[Int, String], String]("daisy").joinLeft // Result: Right("daisy")
   * }}}
   *
   * This method, and `joinRight`, are analogous to `Option#flatten`
   */
  def joinLeft[A1 >: A, B1 >: B, C](implicit ev: A1 <:< Either[C, B1]): Either[C, B1] = this match {
    case Left(a)  => a
    case Right(b) => Right(b)
  }

  /**
   * Returns `true` if this is a `Left`, `false` otherwise.
   *
   * {{{
   * Left("tulip").isLeft // true
   * Right("venus fly-trap").isLeft // false
   * }}}
   */
  def isLeft: Boolean

  /**
   * Returns `true` if this is a `Right`, `false` otherwise.
   *
   * {{{
   * Left("tulip").isRight // false
   * Right("venus fly-trap").isRight // true
   * }}}
   */
  def isRight: Boolean
}

/**
 * The left side of the disjoint union, as opposed to the [[scala.util.Right]] side.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 * @version 1.0, 11/10/2008
 */
final case class Left[+A, +B](a: A) extends Either[A, B] {
  def isLeft = true
  def isRight = false
}

/**
 * The right side of the disjoint union, as opposed to the [[scala.util.Left]] side.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 * @version 1.0, 11/10/2008
 */
final case class Right[+A, +B](b: B) extends Either[A, B] {
  def isLeft = false
  def isRight = true
}

object Either {

  /**
   * Allows use of a `merge` method to extract values from Either instances
   * regardless of whether they are Left or Right.
   *
   * {{{
   * val l = Left(List(1)): Either[List[Int], Vector[Int]]
   * val r = Right(Vector(1)): Either[List[Int], Vector[Int]]
   * l.merge: Seq[Int] // List(1)
   * r.merge: Seq[Int] // Vector(1)
   * }}}
   */
  implicit class MergeableEither[A](private val x: Either[A, A]) extends AnyVal {
    def merge: A = x match {
      case Left(a)  => a
      case Right(a) => a
    }
  }

  /**
   * Projects an `Either` into a `Left`.
   *
   * This allows for-comprehensions over Either instances - for example {{{
   * for (s <- Left("flower").left) yield s.length // Left(6)
   * }}}
   *
   * Continuing the analogy with [[scala.Option]], a `LeftProjection` declares
   * that `Left` should be analogous to `Some` in some code.
   *
   * {{{
   * // using Option:
   * def interactWithDB(x: Query): Option[Result] =
   *   try {
   *     Some(getResultFromDatabase(x))
   *   } catch {
   *     case ex => None
   *   }
   *
   * // this will only be executed if interactWithDB returns a Some
   * val report =
   *   for (r <- interactWithDB(someQuery)) yield generateReport(r)
   * if (report.isDefined)
   *   send(report)
   * else
   *   log("report not generated, not sure why...")
   * }}}
   *
   * {{{
  * // using Either
   * def interactWithDB(x: Query): Either[Exception, Result] =
   *   try {
   *     Right(getResultFromDatabase(x))
   *   } catch {
   *     case ex => Left(ex)
   *   }
   *
   * // this will only be executed if interactWithDB returns a Right
   * val report =
   *   for (r <- interactWithDB(someQuery).right) yield generateReport(r)
   * if (report.isRight)
   *   send(report)
   * else
   *   log("report not generated, reason was " + report.left.get)
   * }}}
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
   * @version 1.0, 11/10/2008
   */
  final case class LeftProjection[+A, +B](e: Either[A, B]) {
    /**
     * Returns the value from this `Left`. Throws `java.util.NoSuchElementException`
     * if this is a `Right`.
     *
     * {{{
     * Left(12).left.get // 12
     * Right(12).left.get // NoSuchElementException
     * }}}
     *
     * @throws java.util.NoSuchElementException if the projection is [[scala.util.Right]]
     */
    def get = e match {
      case Left(a) => a
      case Right(_) =>  throw new NoSuchElementException("Either.left.value on Right")
    }

    /**
     * Executes the given side-effecting function if this is a `Left`.
     *
     * {{{
     * Left(12).left.foreach(x => println(x))  // prints "12"
     * Right(12).left.foreach(x => println(x)) // doesn't print
     * }}}
     * @param f The side-effecting function to execute.
     */
    def foreach[U](f: A => U) = e match {
      case Left(a) => f(a)
      case Right(_) => {}
    }

    /**
     * Returns the value from this `Left` or the given argument if this is a
     * `Right`.
     *
     * {{{
     * Left(12).left.getOrElse(17)  // 12
     * Right(12).left.getOrElse(17) // 17
     * }}}
     *
     */
    def getOrElse[AA >: A](or: => AA) = e match {
      case Left(a) => a
      case Right(_) => or
    }

    /**
     * Returns `true` if `Right` or returns the result of the application of
     * the given function to the `Left` value.
     *
     * {{{
     * Left(12).left.forall(_ > 10)  // true
     * Left(7).left.forall(_ > 10)   // false
     * Right(12).left.forall(_ > 10) // true
     * }}}
     *
     */
    def forall(f: A => Boolean) = e match {
      case Left(a) => f(a)
      case Right(_) => true
    }

    /**
     * Returns `false` if `Right` or returns the result of the application of
     * the given function to the `Left` value.
     *
     * {{{
     * Left(12).left.exists(_ > 10)  // true
     * Left(7).left.exists(_ > 10)   // false
     * Right(12).left.exists(_ > 10) // false
     * }}}
     *
     */
    def exists(f: A => Boolean) = e match {
      case Left(a) => f(a)
      case Right(_) => false
    }

    /**
     * Binds the given function across `Left`.
     *
     * {{{
     * Left(12).left.flatMap(x => Left("scala")) // Left("scala")
     * Right(12).left.flatMap(x => Left("scala") // Right(12)
     * }}}
     * @param f The function to bind across `Left`.
     */
    def flatMap[BB >: B, X](f: A => Either[X, BB]) = e match {
      case Left(a) => f(a)
      case Right(b) => Right(b)
    }

    /**
     * Maps the function argument through `Left`.
     *
     * {{{
     * Left(12).left.map(_ + 2) // Left(14)
     * Right[Int, Int](12).left.map(_ + 2) // Right(12)
     * }}}
     */
    def map[X](f: A => X) = e match {
      case Left(a) => Left(f(a))
      case Right(b) => Right(b)
    }

    /**
     * Returns `None` if this is a `Right` or if the given predicate
     * `p` does not hold for the left value, otherwise, returns a `Left`.
     *
     * {{{
     * Left(12).left.filter(_ > 10)  // Some(Left(12))
     * Left(7).left.filter(_ > 10)   // None
     * Right(12).left.filter(_ > 10) // None
     * }}}
     */
    def filter[Y](p: A => Boolean): Option[Either[A, Y]] = e match {
      case Left(a) => if(p(a)) Some(Left(a)) else None
      case Right(b) => None
    }

    /**
     * Returns a `Seq` containing the `Left` value if it exists or an empty
     * `Seq` if this is a `Right`.
     *
     * {{{
     * Left(12).left.toSeq // Seq(12)
     * Right(12).left.toSeq // Seq()
     * }}}
     */
    def toSeq = e match {
      case Left(a) => Seq(a)
      case Right(_) => Seq.empty
    }

    /**
     * Returns a `Some` containing the `Left` value if it exists or a
     * `None` if this is a `Right`.
     *
     * {{{
     * Left(12).left.toOption // Some(12)
     * Right(12).left.toOption // None
     * }}}
     */
    def toOption = e match {
      case Left(a) => Some(a)
      case Right(_) => None
    }
  }

  /**
   * Projects an `Either` into a `Right`.
   *
   * This allows for-comprehensions over Either instances - for example {{{
   * for (s <- Right("flower").right) yield s.length // Right(6)
   * }}}
   *
   * Continuing the analogy with [[scala.Option]], a `RightProjection` declares
   * that `Right` should be analogous to `Some` in some code.
   *
   * Analogous to `LeftProjection`, see example usage in its documentation above.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
   * @version 1.0, 11/10/2008
   */
  final case class RightProjection[+A, +B](e: Either[A, B]) {

    /**
     * Returns the value from this `Right`. Throws
     * `java.util.NoSuchElementException` if this is a `Left`.
     *
     * {{{
     * Right(12).right.get // 12
     * Left(12).right.get // NoSuchElementException
     * }}}
     *
     * @throws java.util.NoSuchElementException if the projection is `Left`.
     */
    def get = e match {
      case Left(_) =>  throw new NoSuchElementException("Either.right.value on Left")
      case Right(a) => a
    }

    /**
     * Executes the given side-effecting function if this is a `Right`.
     *
     * {{{
     * Right(12).right.foreach(x => println(x)) // prints "12"
     * Left(12).right.foreach(x => println(x))  // doesn't print
     * }}}
     * @param f The side-effecting function to execute.
     */
    def foreach[U](f: B => U) = e match {
      case Left(_) => {}
      case Right(b) => f(b)
    }

    /**
     * Returns the value from this `Right` or the given argument if this is a
     * `Left`.
     *
     * {{{
     * Right(12).right.getOrElse(17) // 12
     * Left(12).right.getOrElse(17)  // 17
     * }}}
     */
    def getOrElse[BB >: B](or: => BB) = e match {
      case Left(_) => or
      case Right(b) => b
    }

    /**
     * Returns `true` if `Left` or returns the result of the application of
     * the given function to the `Right` value.
     *
     * {{{
     * Right(12).right.forall(_ > 10) // true
     * Right(7).right.forall(_ > 10)  // false
     * Left(12).right.forall(_ > 10)  // true
     * }}}
     */
    def forall(f: B => Boolean) = e match {
      case Left(_) => true
      case Right(b) => f(b)
    }

    /**
     * Returns `false` if `Left` or returns the result of the application of
     * the given function to the `Right` value.
     *
     * {{{
     * Right(12).right.exists(_ > 10)  // true
     * Right(7).right.exists(_ > 10)   // false
     * Left(12).right.exists(_ > 10)   // false
     * }}}
     */
    def exists(f: B => Boolean) = e match {
      case Left(_) => false
      case Right(b) => f(b)
    }

    /**
     * Binds the given function across `Right`.
     *
     * @param f The function to bind across `Right`.
     */
    def flatMap[AA >: A, Y](f: B => Either[AA, Y]) = e match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }

    /**
     * The given function is applied if this is a `Right`.
     *
     * {{{
     * Right(12).right.map(x => "flower") // Result: Right("flower")
     * Left(12).right.map(x => "flower")  // Result: Left(12)
     * }}}
     */
    def map[Y](f: B => Y) = e match {
      case Left(a) => Left(a)
      case Right(b) => Right(f(b))
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
    def filter[X](p: B => Boolean): Option[Either[X, B]] = e match {
      case Left(_) => None
      case Right(b) => if(p(b)) Some(Right(b)) else None
    }

    /** Returns a `Seq` containing the `Right` value if
     *  it exists or an empty `Seq` if this is a `Left`.
     *
     * {{{
     * Right(12).right.toSeq // Seq(12)
     * Left(12).right.toSeq // Seq()
     * }}}
     */
    def toSeq = e match {
      case Left(_) => Seq.empty
      case Right(b) => Seq(b)
    }

    /** Returns a `Some` containing the `Right` value
     *  if it exists or a `None` if this is a `Left`.
     *
     * {{{
     * Right(12).right.toOption // Some(12)
     * Left(12).right.toOption // None
     * }}}
     */
    def toOption = e match {
      case Left(_) => None
      case Right(b) => Some(b)
    }
  }

  /** If the condition is satisfied, return the given `B` in `Right`,
   *  otherwise, return the given `A` in `Left`.
   *
   * {{{
   * val userInput: String = ...
   * Either.cond(
   *   userInput.forall(_.isDigit) && userInput.size == 10,
   *   PhoneNumber(userInput),
   *   "The input (%s) does not look like a phone number".format(userInput)
   * }}}
   */
  def cond[A, B](test: Boolean, right: => B, left: => A): Either[A, B] =
    if (test) Right(right) else Left(left)

  // begin left- / right-biased Either stuff
  trait Bias[+E] {
    def empty : E;

    def isLeftBias  : Boolean;
    def isRightBias : Boolean = !isLeftBias;
    def conformsToBias[A,B]( target : Either[A,B] ) : Boolean = {
      target match {
        case Left( _ )  => isLeftBias;
        case Right( _ ) => isRightBias;
      }
    }
  }
  /**
    *  This object contains utilities for defining an environment in which `Either` instances are "right-biased".
    * 
    *  Right-biased means instances of `Either[A,B]` can be operated upon via methods that render thme 
    *  quite analogous to an `Option[A]`, except that failures are represented by a `Left` containing information about
    *  the problem rathar than by uninformative `None`. In particular,
    *  a right-biased `Either` can be used directly in a for comprehension. Unlike a [[scala.util.Either.RightProjection RightProjection]],
    *  a right-biased `Either` supports all features of a for comprehension, including pattern matching, filtering,
    *  and variable assignment.
    * 
    *  Right-biasing decorates vanilla `Either[A,B]` with the following API.
    * {{{
    *   def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z]
    *   def map[Z]( f : B => Z )                         : Either[A,Z]
    *   def withFilter( p : B => Boolean )               : Either[A,B]
    *   def exists( f : B => Boolean )                   : Boolean     
    *   def forall( f : B => Boolean )                   : Boolean     
    *   def foreach[U]( f : B => U )                     : Any         
    *   def get                                          : B           
    *   def getOrElse[BB>:B]( or : =>BB )                : BB          
    *   def toOption                                     : Option[B]   
    *   def toSeq                                        : collection.Seq[B]
    *   def xget                                         : A                
    *   def xgetOrElse[AA>:A]( or : =>AA )               : AA               
    *   def xmap[Z]( f : A => Z )                        : Either[Z,B]      
    *   def replaceIfEmpty[AA>:A]( replacement : =>AA )  : Either[AA,B]     
    *   def isEmpty                                      : Boolean          
    *   def isLeftBiased                                 : Boolean          
    *   def isRightBiased                                : Boolean          
    *   def conformsToBias                               : Boolean          
    * }}}
    * 
    *  For the full documentation, plase see [[RightBias.withEmptyToken.Ops Ops]].
    * 
    *  To enable right-biasing, you first define a bias, typically via the [[Either.RightBias.withEmptyToken$ Either.RightBias.withEmptyToken]]
    *  factory.
    * {{{
    *     // will impart a right-bias suitable to Either[A,String]
    *     val RightBias = Either.RightBias.withEmptyToken("EMPTY")
    *     import RightBias._
    * }}}
    *  The specified "empty token" will define the value in the `Left` object that will be used to indicate
    *  when a filter or pattern-match of a right-side object fails.
    * 
    *  If you don't wish to specify an empty token, you may use the more convenient
    * {{{
    *     import Either.RightBias._
    * }}}
    *  However, if you do not specify an empty token, filter operations or pattern match failures that would leave the right-side
    *  empty will result in the raising of a `java.util.NoSuchElementException`.
    * 
    *  You may also create a right-biased environment within a trait, class, object, or package by
    *  having the entity extend the trait [[RightBias! RightBias]] or the abstract base class
    *  [[RightBias.Base]].
    * 
    *  '''A Detailed example'''
    * 
    *  Consider a hypothetical process that must read some binary data from a URL, parse the data into
    *  a `Record`, ensure that record's year is no older than 2011, and then operate upon
    *  the data via one of several external services, depending on the `opCode`, ultimately producing a `Result`. 
    *  Things that can go wrong include network errors on loading, 
    *  parsing problems, failure of the timestamp condition, and failures with the external service.
    * 
    *  With `Option`, one might define a process like:
    * {{{
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String )                   : Option[Seq[Byte]] = ???
    *     def parseRecord( bytes : Seq[Byte] )          : Option[Record]    = ???
    *     def process( opCode : Int, data : Seq[Byte] ) : Option[Result]    = ???
    * 
    *     def processURL( url : String ) : Option[Result] = {
    *       for {
    *         rawBytes                     <- loadURL( url )
    *         Record( opCode, data, year ) <- parseRecord( rawBytes )
    *         result                       <- process( opCode, data ) if year >= 2011
    *       } yield result
    *     }
    * }}}
    *  That's fine, but if anything goes wrong, clients will receive a value `None`, and have no specific information
    *  about what the problem was. Instead of option, we might try to use Either, defining error codes
    *  for things that can go wrong. (We'll let our error codes be `Int` for this example, but ''errors should 
    *  be represented by descriptive kinds of object in real applications.'') Conventionally,
    *  we'll let good values arrive as `Right` values and embed our error codes in `Left` objects.
    * {{{
    *     val ErrIO               = 1
    *     val ErrNoParse          = 2
    *     val ErrBadYear          = 3
    *     val ErrProcessorFailure = 4
    * 
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String )                   : Either[Int,Seq[Byte]] = ??? // fails w/ErrIO
    *     def parseRecord( bytes : Seq[Byte] )          : Either[Int,Record]    = ??? // fails w/ErrNoParse
    *     def process( opCode : Int, data : Seq[Byte] ) : Either[Int,Result]    = ??? // fails w/ErrProcessorFailure
    * 
    *     // we try to use the right projection but the
    *     // for comprehension fails to compile
    *     def processURL( url : String ) : Either[Int,Result] = {
    *       for {
    *         rawBytes                     <- loadURL( url ).right
    *         Record( opCode, data, year ) <- parseRecord( rawBytes ).right
    *         result                       <- process( opCode, data ).right if year >= 2011
    *       } yield result
    *     }
    * }}}
    *   Unfortunately, this fails to compile, because [[scala.util.Either.RightProjection]] does not support
    *   pattern-matching (used to extract `opCode`, `data`, and `year`) or filtering (which we perform based on `year`).
    * 
    *   Instead, we can right-bias the `Either`:
    * {{{
    *     val ErrEmpty            = -1
    *     val ErrIO               =  1
    *     val ErrNoParse          =  2
    *     val ErrBadYear          =  3
    *     val ErrProcessorFailure =  4
    * 
    *     // right-bias Either 
    *     val RightBias = Either.RightBias.withEmptyToken( ErrEmpty ) 
    *     import RightBias._
    * 
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String )                   : Either[Int,Seq[Byte]] = ??? // fails w/ErrIO
    *     def parseRecord( bytes : Seq[Byte] )          : Either[Int,Record]    = ??? // fails w/ErrNoParse
    *     def process( opCode : Int, data : Seq[Byte] ) : Either[Int,Result]    = ??? // fails w/ErrProcessorFailure
    * 
    *     // use Either directly, rather than a projection, in the for comprehension
    *     def processURL( url : String ) : Either[Int,Result] = {
    *       for {
    *         rawBytes                     <- loadURL( url )
    *         Record( opCode, data, year ) <- parseRecord( rawBytes )
    *         result                       <- process( opCode, data ) if year >= 2011
    *       } yield result
    *     }
    * }}}
    * 
    *   There is a problem: `ErrEmpty`, would be the result of the year filter failing, 
    *   when it should be `ErrBadYear`. 
    * 
    *   The most general and straightforward wat to fix this is to replace empty
    *   tokens as they arrive with more informative errors. For this purpose,
    *   biased `Either` offers a method called [[RightBias.Ops.replaceIfEmpty]]: 
    * 
    * {{{
    *     // use Either directly, rather than a projection, in the for comprehension
    *     def processURL( url : String ) : Either[Int,Result] = {
    *       val processed = {
    *         for {
    *           rawBytes                     <- loadURL( url )
    *           Record( opCode, data, year ) <- parseRecord( rawBytes )
    *           result                       <- process( opCode, data ) if year >= 2011
    *         } yield result
    *       }
    *       processed.replaceIfEmpty( ErrBadYear )
    *     }
    * }}}
    * 
    *   The scope of the right-bias is the scope of the import. An alternative way to
    *   ensure empty tokens yield meaningful information is to define separate biases
    *   for separate contexts.
    * 
    * {{{
    *     val ProcessURLBias       = Either.RightBias.withEmptyToken( ErrBadYear ) 
    *     val EnableHyperdriveBias = Either.RightBias.withEmptyToken( ErrSpaceTimeRupture )
    *     ...
    * 
    *     def processURL( url : String ) : Either[Int,Result] = {
    *       import ProcessURLBias._
    *       for {
    *         rawBytes                     <- loadURL( url )
    *         Record( opCode, data, year ) <- parseRecord( rawBytes )
    *         result                       <- process( opCode, data ) if year >= 2011
    *       } yield result
    *     }
    * }}}
    */ 
  final object RightBias {

    private[Either] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( true ) ) );

    /**
      * A value class implementing the operations of a [[RightBias$ right-biased]] `Either`,
      * which throws a `java.util.NoSuchElementException` if a filter operation
      * or pattern match results in empty.
      * 
      * Typical use
      * {{{
      *   import Either.RightBias._
      * }}}
      * 
      * For documentation of the operations, please see [[RightBias.withEmptyToken.Ops]]
      * 
      * For more, please see [[RightBias$ RightBias]].
      */
    implicit class Ops[A,B]( val target : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = DefaultThrowingOps.flatMap[A,AA,B,Z]( target )( f );
      def map[Z]( f : B => Z )                         : Either[A,Z]  = DefaultThrowingOps.map( target )( f );
      def withFilter( p : B => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( target )( p );

      // extra ops
      def exists( f : B => Boolean )                  : Boolean           = DefaultThrowingOps.exists( target )( f );
      def forall( f : B => Boolean )                  : Boolean           = DefaultThrowingOps.forall( target )( f );
      def foreach[U]( f : B => U )                    : Any               = DefaultThrowingOps.foreach( target )( f );
      def get                                         : B                 = DefaultThrowingOps.get( target );
      def getOrElse[ BB >: B ]( or : =>BB )           : BB                = DefaultThrowingOps.getOrElse[A,B,BB]( target )( or );
      def toOption                                    : Option[B]         = DefaultThrowingOps.toOption( target );
      def toSeq                                       : collection.Seq[B] = DefaultThrowingOps.toSeq( target );
      def xget                                        : A                 = DefaultThrowingOps.xget( target );
      def xgetOrElse[AA>:A]( or : =>AA )              : AA                = DefaultThrowingOps.xgetOrElse[A,AA,B]( target )( or );
      def xmap[Z]( f : A => Z )                       : Either[Z,B]       = DefaultThrowingOps.xmap( target )( f );
      def replaceIfEmpty[AA>:A]( replacement : =>AA ) : Either[AA,B]      = DefaultThrowingOps.replaceIfEmpty[A,AA,B]( target )( replacement );
      def isEmpty                                     : Boolean           = DefaultThrowingOps.isEmpty( target );
      def isLeftBiased                                : Boolean           = DefaultThrowingOps.isLeftBias;
      def isRightBiased                               : Boolean           = DefaultThrowingOps.isRightBias;
      def conformsToBias                              : Boolean           = DefaultThrowingOps.conformsToBias( target );
    }

    /**
      * A factory for a typeclass instance which implements the operations of a right-biased `Either`. 
      * An unsuccessful filter or pattern match will result in a Left containing a specified empty token of type `E`.
      * 
      * Typical use
      * {{{
      *   val RightBias = Either.RightBias.withEmptyToken(-1); //suitable for Either[Int,A]
      *   import RightBias._
      * }}}
      * For more, please see [[RightBias$ RightBias]].
      */
    object withEmptyToken {
      /**
        * Implements the operations of a right-biased `Either`, whose specific behavior -- especially with respect
        * to handling of an empty value resulting from a filter or failed pattern match -- is defined by
        * constructor argument `opsTypeClass`.
        * 
        * Clients can extend this is they wish to defined specialized right-biased `Either` instances
        * that define extra operations.
        * {{{
        *   val EmptyThrowable = {
        *     // the stack-trace on construction has no relevance to operations that return this token
        *     // so we clear that stack trace
        *     val tmp = new NoSuchElementException("EMPTY -- A value was right empty after a failed filter or pattern match operation.");
        *     tmp.setStackTrace( Array.empty[StackTraceElement] ); 
        *     tmp
        *   }
        *   val RightBias = Either.RightBias.withEmptyToken[Throwable]( EmptyThrowable );
        *   implicit class RightBiasOps[B]( target : Either[Throwable,B] ) extends Either.RightBias.withEmptyToken.AbstractOps( target )( RightBias ) {
        *      def raise : Unit = {
        *        target match {
        *          case Left( t ) => throw t;
        *          case Right( _ ) => ();
        *        }
        *      }
        *   }
        * }}}
        * 
        * For more, see [[RightBias$ RightBias]].
        */ 
      abstract class AbstractOps[A,B]( target : Either[A,B] )( opsTypeClass : Either.RightBias.withEmptyToken.Generic[A] ) {

        // monad ops
        /**
          * Binds the given function across `Right` of athe target right-biased `Either`.
          *
          * {{{
          * Left(12).flatMap(x => Left("scala"))  // Left(12)
          * Right(12).flatMap(x => Right("scala") // Right("scala")
          * }}}
          * @param f The function to bind across `Left`.
          */
        def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = opsTypeClass.flatMap[A,AA,B,Z]( target )( f );
        /**
          * Maps the function argument through `Right` of the target right-biased `Either`.
          *
          * {{{
          * Left(12).map(_ + 2)  // Left(12)
          * Right(12).map(_ + 2) // Right(14)
          * }}}
          */
        def map[Z]( f : B => Z )                         : Either[A,Z]  = opsTypeClass.map( target )( f );
        /**
          * Returns the target right-biased `Either` unchanged if it is a `Right` ''and'' the predicate `p`
          * holds for its value, or if it is any `Left`.
          * 
          * Returns a `Left` containing the empty token if the target is a `Right` but predicate `p`
          * fails to hold. If no empty token has been set, a `java.util.NoSuchElementException` will
          * be thrown.
          *
          * {{{
          * val RightBias = RightBias.withEmptyToken[Int](-1);
          * import RightBias._
          * 
          * Left(7).withFilter(_ > 10)   // Left(7)
          * Right(7).withFilter(_ > 10)  // Left(-1)
          * Right(12).withFilter(_ > 10) // Right(12)
          * }}}
          */
        def withFilter( p : B => Boolean )               : Either[A,B]  = opsTypeClass.withFilter( target )( p );

        // extra ops
        /**
          * Returns `false` if the target right-biased `Either` is a `Left` or returns the result of the application of
          * the given function to the `Right` value.
          *
          * {{{
          * Right(12).exists(_ > 10) // true
          * Right(7).exists(_ > 10)  // false
          * Left(12).exists(_ > 10)  // false
          * }}}
          *
          */
        def exists( f : B => Boolean )                  : Boolean           = opsTypeClass.exists( target )( f );
        /**
          * Returns `true` if the target right-biased `Either` is `Left` or returns 
          * the result of the application of the given function to the `Right` value.
          *
          * {{{
          * Right(12).forall(_ > 10) // true
          * Right(7).forall(_ > 10)  // false
          * Left(12).forall(_ > 10)  // true
          * }}}
          *
          */
        def forall( f : B => Boolean )                  : Boolean           = opsTypeClass.forall( target )( f );
        /**
          * Executes the given side-effecting function if the target right-biased `Either` is a `Right`.
          *
          * {{{
          * Left(12).foreach(x => println(x))  // doesn't print
          * Right(12).foreach(x => println(x)) // prints "12"
          * }}}
          * 
          * @param f The side-effecting function to execute.
          */
        def foreach[U]( f : B => U )                    : Any               = opsTypeClass.foreach( target )( f );
        /**
          * Returns the value if the target right-biased `Either` is a `Right` or 
          * throws `java.util.NoSuchElementException` if the target is a `Left`.
          *
          * {{{
          * Left(12).get  // NoSuchElementException
          * Right(12).get // 12
          * }}}
          *
          * @throws java.util.NoSuchElementException if the target is [[scala.util.Left]]
          */
        def get                                         : B                 = opsTypeClass.get( target );
        /**
          * Returns the value from the target right-biased `Either` if it is a `Right`,
          * or the argument `or` if it is a `Left`.
          *
          * {{{
          * Left(12).getOrElse(17)  // 12
          * Right(12).getOrElse(17) // 17
          * }}}
          *
          */
        def getOrElse[BB>:B]( or : =>BB )               : BB                = opsTypeClass.getOrElse[A,B,BB]( target )( or );
        /**
          * Returns a `Some` containing the `Right` value of the target right-biased `Either` if it exists or a
          * `None` if the target is a `Left`.
          *
          * {{{
          * Left(12).toOption  // None
          * Right(12).toOption // Some(12)
          * }}}
          */
        def toOption                                    : Option[B]         = opsTypeClass.toOption( target );
        /**
          * Returns a `Seq` containing the `Right` value if it exists or an empty
          * `Seq` if the target right-biased `Either` is a `Left`.
          *
          * {{{
          * Left(12).toSeq  // Seq()
          * Right(12).toSeq // Seq(12)
          * }}}
          */
        def toSeq                                       : collection.Seq[B] = opsTypeClass.toSeq( target );
        /**
          *  Returns `true` if this right-biased `Either` is a `Left`
          *  that contains the empty token, signifying empty. Returns `false`
          *  for any other error or value.
          */ 
        def isEmpty                                     : Boolean           = opsTypeClass.isEmpty( target );
        /**
          * "cross-get" -- Returns the value if the target right-biased `Either` 
          * does ''not'' conform to its bias, that is, if it is a `Left`.
          * Throws `java.util.NoSuchElementException` if the target is a `Right`.
          *
          * {{{
          * Left(12).xget  // 12
          * Right(12).xget // NoSuchElementException
          * }}}
          *
          * @throws java.util.NoSuchElementException if the target is [[scala.util.Right]]
          */
        def xget                                        : A                 = opsTypeClass.xget( target );
        /**
          * "cross-getOrElse" -- Returns the value from the target right-biased `Either` 
          * if it does ''not'' conform to its bias and is a `Left`.
          * Returns the argument `or` if it is a `Right`.
          *
          * {{{
          * Right(12).xgetOrElse(17) // 17
          * Left(12).xgetOrElse(17)  // 12
          * }}}
          *
          */
        def xgetOrElse[AA>:A]( or : =>AA )              : AA                = opsTypeClass.xgetOrElse[A,AA,B]( target )( or );
        /**
          * "cross-map" -- Maps the function argument through `Left` ''against'' the bias the target right-biased `Either`.
          *
          * {{{
          * Left(12).map(_ + 2)  // Left(14)
          * Right(12).map(_ + 2) // Right(12)
          * }}}
          */
        def xmap[Z]( f : A => Z )                       : Either[Z,B]       = opsTypeClass.xmap( target )( f );
        /**
          * If the target right-biased `Either` is a `Left` containing the empty token,
          * returns a `Left` containing `replacement`. Otherwise returns the target `Either` unchanged.
          *
          * {{{
          * // with empty token Int -1
          * 
          * Left(12).replaceIfEmpty(99)  // Left(12)
          * Left(-1).replaceIfEmpty(99)  // Left(99)
          * Right(12).replaceIfEmpty(99) // Right(12)
          * Right(-1).replaceIfEmpty(99) // Right(-1)
          * }}}
          */
        def replaceIfEmpty[AA>:A]( replacement : =>AA ) : Either[AA,B]      = opsTypeClass.replaceIfEmpty[A,AA,B]( target )( replacement );
        /**
          * Returns `true` when called on a wrapper representing a left-biased `Either`, `false` otherwise.
          */
        def isLeftBiased                                : Boolean           = opsTypeClass.isLeftBias;
        /**
          * Returns `true` when called on a wrapper representing a right-biased `Either`, `false` otherwise.
          */
        def isRightBiased                               : Boolean           = opsTypeClass.isRightBias;
        /**
          * Returns `true` if this wrapper represents a left-biased `Either` wrapping a `Left` or a 
          * right-biased `Either` wrapping a `Right`. Returns false if the bias of this wrapper is not
          * consistent with the type of the target `Either`.
          * 
          * It is safe to call [[get]] on a biased `Either` for which `conformsToBias` is `true`.
          * 
          * It is safe to call [[xget]] on a biased `Either` for which `conformsToBias` is `false`.
          */
        def conformsToBias                              : Boolean           = opsTypeClass.conformsToBias( target );
      }

      implicit final class Ops[A,B]( target : Either[A,B] )( implicit opsTypeClass : Either.RightBias.withEmptyToken.Generic[A] ) extends AbstractOps( target )( opsTypeClass );

      trait Generic[+E] extends Either.Bias[E] {
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Left should be created on each
         * invocation.
         */ 
        protected def leftEmpty : Left[E,Nothing];

        def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean;

        // monad ops
        def flatMap[A>:E,AA>:A,B,Z]( target : Either[A,B] )( f : B => Either[AA,Z] ) : Either[AA,Z] = {
          target match {
            case Left( _ )  => target.asInstanceOf[Left[A,Z]]
            case Right( b ) => f( b )
          }
        }
        def map[A>:E,B,Z]( target : Either[A,B] )( f : B => Z ) : Either[A,Z] = {
          target match {
            case Left( _ )  => target.asInstanceOf[Left[A,Z]]
            case Right( b ) => Right( f( b ) )
          }
        }
        def withFilter[A>:E,B]( target : Either[A,B] )( p : B => Boolean ) : Either[A,B] = {
          target match {
            case      Left( _ ) => target;
            case r @ Right( b ) => if ( p(b) ) r else leftEmpty;
          }
        }

        // extra ops
        def exists[A>:E,B]( target : Either[A,B] )( f : B => Boolean ) : Boolean = {
          target match {
            case Left( _ )  => false;
            case Right( b ) => f( b );
          }
        }
        def forall[A>:E,B]( target : Either[A,B] )( f : B => Boolean ) : Boolean = {
          target match {
            case Left( _ )  => true;
            case Right( b ) => f( b );
          }
        }
        def foreach[A>:E,B,U]( target : Either[A,B] )( f : B => U ) : Any = {
          target match {
            case Left( _ )  => ();
            case Right( b ) => f( b );
          }
        }
        def get[A>:E,B]( target : Either[A,B] ) : B = {
          target match {
            case Left( _ )  => throw new NoSuchElementException( NoSuchRightMessage );
            case Right( b ) => b;
          }
        }
        def getOrElse[A>:E,B,BB>:B]( target : Either[A,B] )( or : =>BB ) : BB = {
          target match {
            case Left( _ )  => or;
            case Right( b ) => b;
          }
        }
        def toOption[A>:E,B]( target : Either[A,B] ) : Option[B] = {
          target match {
            case Left( _ )  => None;
            case Right( b ) => Some( b );
          }
        }
        def toSeq[A>:E,B]( target : Either[A,B] ) : collection.Seq[B] = {
          target match {
            case Left( _ )  => collection.Seq.empty[B];
            case Right( b ) => collection.Seq( b );
          }
        }
        def xget[A>:E,B]( target : Either[A,B] ) : A = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => throw new NoSuchElementException( NoSuchXLeftMessage );
          }
        }
        def xgetOrElse[A>:E,AA>:A,B]( target : Either[A,B] )( or : =>AA ) : AA = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => or;
          }
        }
        def xmap[A>:E,B,Z]( target : Either[A,B] )( f : A => Z ) : Either[Z,B] = {
          target match {
            case Left( a )  => Left( f( a ) )
            case Right( _ ) => target.asInstanceOf[Right[Z,B]]
          }
        }
        def replaceIfEmpty[A>:E,AA>:A,B]( target : Either[A,B] )( replacement : =>AA ) : Either[AA,B] = {
          if (isEmpty( target )) Left( replacement ) else target;
        }
        def isLeftBias  : Boolean = false;

        implicit def toOps[A>:E,B]( target : Either[A,B] ) : RightBias.withEmptyToken.Ops[A,B] = new RightBias.withEmptyToken.Ops[A,B]( target )( this )
      }
      def apply[E]( token : E ) : withEmptyToken[E] = new withEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      /**
        * A typeclass instance which implements the operations of a right-biased `Either` 
        * that signals empty by throwing some Throwable as specified in a by-name parameter.
        * 
        * Typical use
        * {{{
        *   val RightBias = Either.RightBias.withEmptyToken.Throwing( new NoValueAvailableException ); 
        *   import RightBias._
        * }}}
        * For more, please see [[RightBias$ RightBias]].
        */
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends withEmptyToken.Generic[Nothing] {
        override protected def leftEmpty : Nothing = empty;

        override def empty : Nothing = throw throwableBuilder;

        override def isEmpty[A,B]( target : Either[A,B] ) : Boolean = false; // no state represents empty, empties cannot be formed as an Exception is thrown when it is tried
      }
    }
    /**
      * A typeclass instance which implements the operations of a right-biased `Either`. 
      * An unsuccessful filter or pattern match will result in a Left containing a specified empty token of type `E`.
      * 
      * Typical use
      * {{{
      *   val RightBias = Either.RightBias.withEmptyToken(-1); //suitable for Either[Int,A]
      *   import RightBias._
      * }}}
      * For more, please see [[RightBias$ RightBias]].
      */
    final class withEmptyToken[+E] private( override val empty : E ) extends withEmptyToken.Generic[E] {
      override protected val leftEmpty : Left[E,Nothing] = Left(empty);

      override def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean = (target == leftEmpty);
    }
    /**
      * May be extended by classes, objects, traits, and packages (via package objects).
      * 
      * Within entities that extend this class
      * values of type `Either` will be right-biased, and have
      * the full-suite of right-biased operations (including operations suitable to for
      * comprehensions) automatically made available.
      * 
      * Empty `Either` values will be represented by Left values containing the token 
      * of type `L` passed to the superclass constructor.
      * 
      * '''If you are using polymorphic error types, be sure to specify the type
      *  argument to Base, as too narrow a type may be inferred from a token
      *  of a derived type.'''
      *  In English, this means you should extend `Either.RightBias.Base[Err]( ErrEmpty )`, not just `Either.RightBias.Base( ErrEmpty )`.
      * 
      * ''Note: Entities that do not need to extend another class can extend the abstract
      * class [[Either.RightBias.Base]], which allows definition of the Empty token in the
      * superclass constructor rather than overriding [[EmptyTokenDefinition]].''
      * 
      * For more information, please see [[RightBias$ RightBias]]
      * 
      * {{{
      * class Clown extends Either.RightBias.Base[Clown.Error]( Clown.Error.Empty ) {
      *   import Clown.{Cash,Laugh,Error};
      * 
      *   // use Either values directly in for comprehensions
      *   def perform : Either[Error,Cash] = {
      *      for {
      *        laugh <- makeFunny
      *        money <- begForCash( laugh ) if laugh.loudness > 2
      *      } yield money
      *   }
      * 
      *   def makeFunny : Either[Error,Laugh] = ???
      *   def begForCash( laugh : Laugh )  : Either[Error,Cash] = ???
      * }
      * object Clown {
      *   final object Error {
      *     case object Empty extends Error;
      *     case object JokeBombed extends Error;
      *     case object NoAudience extends Error;
      *     case object Cheapskates extends Error;
      *   }
      *   sealed trait Error;
      * 
      *   case class Laugh( loudness : Int );
      *   case class Cash( quantity : Int, currencyCode : String );
      * }
      * }}}
      * 
      */ 
    abstract class Base[L]( emptyToken : L ) extends RightBias[L] {
      override val EmptyTokenDefinition = RightBias.withEmptyToken[L]( emptyToken )
    }
  }
  /**
    * Right-biasing trait that may be extended by classes, objects, traits, and packages (via package objects).
    * 
    * Within entities that extend this trait
    * values of type `Either` will be right-biased, and have
    * the full-suite of right-biased operations (including operations suitable to for
    * comprehensions) automatically made available.
    * 
    * By default, transformation of a right-biased `Either` to empty by a failed pattern
    * match or filter operation will provoke a `java.util.NoSuchElementException`. 
    * Override [[EmptyTokenDefinition]] to prevent this and have emptiness
    * represented by an error token of type `L`.
    * 
    * ''Note: Entities that do not need to extend another class can extend the abstract
    * class [[Either.RightBias.Base]], which allows definition of the Empty token in the
    * superclass constructor rather than overriding [[EmptyTokenDefinition]].''
    * 
    * For more information, please see [[RightBias$ RightBias]]
    * 
    * {{{
    * class Clown extends Either.RightBias[Clown.Error] {
    *   import Clown.{Cash,Laugh,Error};
    * 
    *   // override to specify an empty token
    *   override val EmptyTokenDefinition = Either.RightBias.withEmptyToken(Error.Empty);
    * 
    *   // use Either values directly in for comprehensions
    *   def perform : Either[Error,Cash] = {
    *      for {
    *        laugh <- makeFunny
    *        money <- begForCash( laugh ) if laugh.loudness > 2
    *      } yield money
    *   }
    * 
    *   def makeFunny : Either[Error,Laugh] = ???
    *   def begForCash( laugh : Laugh )  : Either[Error,Cash] = ???
    * }
    * object Clown {
    *   final object Error {
    *     case object Empty extends Error;
    *     case object JokeBombed extends Error;
    *     case object NoAudience extends Error;
    *     case object Cheapskates extends Error;
    *   }
    *   sealed trait Error;
    * 
    *   case class Laugh( loudness : Int );
    *   case class Cash( quantity : Int, currencyCode : String );
    * }
    * }}}
    * 
    */ 
  trait RightBias[L] {
    val EmptyTokenDefinition : Either.RightBias.withEmptyToken.Generic[L] = RightBias.DefaultThrowingOps;

    /**
      * Override this value to specify an empty token of type `L`. A Left object with this
      * value will be returned by a failed filter or pattern match operation. If this
      * value is not overridden, the default behavior will be to throw a `java.util.NoSuchElementException`
      * on an unmatched pattern or filter. 
      * {{{
      *   // suppose errors on right-biased Either values are represented with Int codes
      *   // with -1 representing "empty"
      *   override val EmptyTokenDefinition = Either.RightBias.withEmptyToken(-1);
      * }}}
      */ 
    implicit def toRightBiasEtherOps[R]( target : Either[L,R] ) : RightBias.withEmptyToken.AbstractOps[L,R] = new RightBias.withEmptyToken.Ops[L,R]( target )( EmptyTokenDefinition );
  }
  /**
    *  This object contains utilities for defining an environment in which `Either` instances are "left-biased".
    * 
    *  Left-biased means instances of `Either[A,B]` can be operated upon via methods that render thme 
    *  quite analogous to an `Option[A]`, except that failures are represented by a `Right` containing information about
    *  the problem rathar than by uninformative `None`. In particular,
    *  a left-biased `Either` can be used directly in a for comprehension. Unlike a [[scala.util.Either.LeftProjection LeftProjection]],
    *  a left-biased `Either` supports all features of a for comprehension, including pattern matching, filtering,
    *  and variable assignment.
    * 
    *  Left-biasing decorates vanilla `Either[A,B]` with the following API.
    * {{{
    *   def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB]
    *   def map[Z]( f : A => Z )                         : Either[Z,B]
    *   def withFilter( p : A => Boolean )               : Either[A,B]
    *   def exists( f : A => Boolean )                   : Boolean
    *   def forall( f : A => Boolean )                   : Boolean
    *   def foreach[U]( f : A => U )                     : Any
    *   def get                                          : A
    *   def getOrElse[AA >: A ]( or : =>AA )             : AA
    *   def toOption                                     : Option[A]
    *   def toSeq                                        : collection.Seq[A]
    *   def xget                                         : B
    *   def xgetOrElse[BB>:B]( or : =>BB )               : BB
    *   def xmap[Z]( f : B => Z )                        : Either[A,Z]
    *   def replaceIfEmpty[BB>:B]( replacement : =>BB )  : Either[A,BB]
    *   def isEmpty                                      : Boolean          
    *   def isLeftBiased                                 : Boolean
    *   def isRightBiased                                : Boolean
    *   def conformsToBias                               : Boolean
    * }}}
    * 
    *  For the full documentation, plase see [[LeftBias.withEmptyToken.Ops Ops]].
    * 
    *  To enable left-biasing, you first define a bias, typically via the [[Either.LeftBias.withEmptyToken$ Either.LeftBias.withEmptyToken]]
    *  factory.
    * {{{
    *     // will impart a left-bias suitable to Either[A,String]
    *     val LeftBias = Either.LeftBias.withEmptyToken("EMPTY")
    *     import LeftBias._
    * }}}
    *  The specified "empty token" will define the value in the `Right` object that will be used to indicate
    *  when a filter or pattern-match of a left-side object fails.
    * 
    *  If you don't wish to specify an empty token, you may use the more convenient
    * {{{
    *     import Either.LeftBias._
    * }}}
    *  However, if you do not specify an empty token, filter operations or pattern match failures that would leave the left-side
    *  empty will result in the raising of a `java.util.NoSuchElementException`.
    * 
    *  You may also create a left-biased environment within a trait, class, object, or package by
    *  having the entity extend the trait [[LeftBias! LeftBias]] or the abstract base class
    *  [[LeftBias.Base]].
    * 
    *  '''A Detailed example'''
    * 
    *  Consider a hypothetical process that must read some binary data from a URL, parse the data into
    *  a `Record`, ensure that record's year is no older than 2011, and then operate upon
    *  the data via one of several external services, depending on the `opCode`, ultimately producing a `Result`. 
    *  Things that can go wrong include network errors on loading, 
    *  parsing problems, failure of the timestamp condition, and failures with the external service.
    * 
    *  With `Option`, one might define a process like:
    * {{{
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String )                   : Option[Seq[Byte]] = ???
    *     def parseRecord( bytes : Seq[Byte] )          : Option[Record] = ???
    *     def process( opCode : Int, data : Seq[Byte] ) : Option[Result] = ???
    * 
    *     def processURL( url : String ) : Option[Result] = {
    *       for {
    *         rawBytes                     <- loadURL( url )
    *         Record( opCode, data, year ) <- parseRecord( rawBytes )
    *         result                       <- process( opCode, data ) if year >= 2011
    *       } yield result
    *     }
    * }}}
    *  That's fine, but if anything goes wrong, clients will receive a value `None`, and have no specific information
    *  about what the problem was. Instead of option, we might try to use Either, defining error codes
    *  for things that can go wrong. (We'll let our error codes be `Int` for this example, but ''errors should 
    *  be represented by descriptive kinds of object in real applications.'') Unconventionally (since we are considering left-biased
    *  `Either` here), we'll let good values arrive as `Left` values and embed our error codes in `Right` objects.
    * {{{
    *     val ErrIO               = 1
    *     val ErrNoParse          = 2
    *     val ErrBadYear          = 3
    *     val ErrProcessorFailure = 4
    * 
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String )                   : Either[Seq[Byte],Int] = ??? // fails w/ErrIO
    *     def parseRecord( bytes : Seq[Byte] )          : Either[Record,Int]    = ??? // fails w/ErrNoParse
    *     def process( opCode : Int, data : Seq[Byte] ) : Either[Result,Int]    = ??? // fails w/ErrProcessorFailure
    * 
    *     // we try to use the left projection but the
    *     // for comprehension fails to compile
    *     def processURL( url : String ) : Either[Result,Int] = {
    *       for {
    *         rawBytes                     <- loadURL( url ).left
    *         Record( opCode, data, year ) <- parseRecord( rawBytes ).left
    *         result                       <- process( opCode, data ).left if year >= 2011
    *       } yield result
    *     }
    * }}}
    *   Unfortunately, this fails to compile, because [[scala.util.Either.LeftProjection]] does not support
    *   pattern-matching (used to extract `opCode`, `data`, and `year`) or filtering (which we perform based on `year`).
    * 
    *   Instead, we can left-bias the `Either`:
    * {{{
    *     val ErrEmpty            = -1
    *     val ErrIO               =  1
    *     val ErrNoParse          =  2
    *     val ErrBadYear          =  3
    *     val ErrProcessorFailure =  4
    * 
    *     // left-bias Either 
    *     val LeftBias = Either.LeftBias.withEmptyToken( ErrEmpty ) 
    *     import LeftBias._
    * 
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String )                   : Either[Seq[Byte],Int] = ??? // fails w/ErrIO
    *     def parseRecord( bytes : Seq[Byte] )          : Either[Record,Int]    = ??? // fails w/ErrNoParse
    *     def process( opCode : Int, data : Seq[Byte] ) : Either[Result,Int]    = ??? // fails w/ErrProcessorFailure
    * 
    *     // use Either directly, rather than a projection, in the for comprehension
    *     def processURL( url : String ) : Either[Result,Int] = {
    *       for {
    *         rawBytes                     <- loadURL( url )
    *         Record( opCode, data, year ) <- parseRecord( rawBytes )
    *         result                       <- process( opCode, data ) if year >= 2011
    *       } yield result
    *     }
    * }}}
    * 
    *   There is a problem: `ErrEmpty`, would be the result of the year filter failing, 
    *   when it should be `ErrBadYear`. 
    * 
    *   The most general and straightforward wat to fix this is to replace empty
    *   tokens as they arrive with more informative errors. For this purpose,
    *   biased `Either` offers a method called [[LeftBias.Ops.replaceIfEmpty]]: 
    * 
    * {{{
    *     // use Either directly, rather than a projection, in the for comprehension
    *     def processURL( url : String ) : Either[Result,Int] = {
    *       val processed = {
    *         for {
    *           rawBytes                     <- loadURL( url )
    *           Record( opCode, data, year ) <- parseRecord( rawBytes )
    *           result                       <- process( opCode, data ) if year >= 2011
    *         } yield result
    *       }
    *       processed.replaceIfEmpty( ErrBadYear )
    *     }
    * }}}
    * 
    *   The scope of the left-bias is the scope of the import. An alternative way to
    *   ensure empty tokens yield meaningful information is to define separate biases
    *   for separate contexts.
    * 
    * {{{
    *     val ProcessURLBias       = Either.LeftBias.withEmptyToken( ErrBadYear ) 
    *     val EnableHyperdriveBias = Either.LeftBias.withEmptyToken( ErrSpaceTimeRupture )
    *     ...
    * 
    *     def processURL( url : String ) : Either[Result,Int] = {
    *       import ProcessURLBias._
    *       for {
    *         rawBytes                     <- loadURL( url )
    *         Record( opCode, data, year ) <- parseRecord( rawBytes )
    *         result                       <- process( opCode, data ) if year >= 2011
    *       } yield result
    *     }
    * }}}
    */ 
  final object LeftBias {

    private[Either] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( false ) ) );

    /**
      * A value class implementing the operations of a [[LeftBias$ left-biased]] `Either`,
      * which throws a `java.util.NoSuchElementException` if a filter operation
      * or pattern match results in empty.
      * 
      * Typical use
      * {{{
      *   import Either.LeftBias._
      * }}}
      * 
      * For documentation of the operations, please see [[LeftBias.withEmptyToken.Ops]]
      * 
      * For more, please see [[LeftBias$ LeftBias]].
      */
    implicit class Ops[A,B]( val target : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = DefaultThrowingOps.flatMap[A,B,BB,Z]( target )( f );
      def map[Z]( f : A => Z )                         : Either[Z,B]  = DefaultThrowingOps.map( target )( f );
      def withFilter( p : A => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( target )( p );

      // extra ops
      def exists( f : A => Boolean )                  : Boolean           = DefaultThrowingOps.exists( target )( f );
      def forall( f : A => Boolean )                  : Boolean           = DefaultThrowingOps.forall( target )( f );
      def foreach[U]( f : A => U )                    : Any               = DefaultThrowingOps.foreach( target )( f );
      def get                                         : A                 = DefaultThrowingOps.get( target );
      def getOrElse[AA >: A ]( or : =>AA )            : AA                = DefaultThrowingOps.getOrElse[A,AA,B]( target )( or );
      def toOption                                    : Option[A]         = DefaultThrowingOps.toOption( target );
      def toSeq                                       : collection.Seq[A] = DefaultThrowingOps.toSeq( target );
      def isEmpty                                     : Boolean           = DefaultThrowingOps.isEmpty( target );
      def xget                                        : B                 = DefaultThrowingOps.xget( target );
      def xgetOrElse[BB>:B]( or : =>BB )              : BB                = DefaultThrowingOps.xgetOrElse[A,B,BB]( target )( or );
      def xmap[Z]( f : B => Z )                       : Either[A,Z]       = DefaultThrowingOps.xmap( target )( f );
      def replaceIfEmpty[BB>:B]( replacement : =>BB ) : Either[A,BB]      = DefaultThrowingOps.replaceIfEmpty[A,B,BB]( target )( replacement )
      def isLeftBiased                                : Boolean           = DefaultThrowingOps.isLeftBias;
      def isRightBiased                               : Boolean           = DefaultThrowingOps.isRightBias;
      def conformsToBias                              : Boolean           = DefaultThrowingOps.conformsToBias( target );
    }

    /**
      * A factory for a typeclass instance which implements the operations of a left-biased `Either`. 
      * An unsuccessful filter or pattern match will result in a Right containing a specified empty token of type `E`.
      * 
      * Typical use
      * {{{
      *   val LeftBias = Either.LeftBias.withEmptyToken(-1); //suitable for Either[A,Int]
      *   import LeftBias._
      * }}}
      * For more, please see [[LeftBias$ LeftBias]].
      */
    object withEmptyToken {
      /**
        * Implements the operations of a left-biased `Either`, whose specific behavior -- especially with respect
        * to handling of an empty value resulting from a filter or failed pattern match -- is defined by
        * constructor argument `opsTypeClass`.
        * 
        * Clients can extend this is they wish to defined specialized left-biased `Either` instances
        * that define extra operations.
        * {{{
        *   val EmptyThrowable = {
        *     // the stack-trace on construction has no relevance to operations that return this token
        *     // so we clear that stack trace
        *     val tmp = new NoSuchElementException("EMPTY -- A value was left empty after a failed filter or pattern match operation.");
        *     tmp.setStackTrace( Array.empty[StackTraceElement] ); 
        *     tmp
        *   }
        *   val LeftBias = Either.LeftBias.withEmptyToken[Throwable]( EmptyThrowable );
        *   implicit class LeftBiasOps[A]( target : Either[A,Throwable] ) extends Either.LeftBias.withEmptyToken.AbstractOps( target )( LeftBias ) {
        *      def raise : Unit = {
        *        target match {
        *          case Left(_) => ();
        *          case Right( t ) => throw t;
        *        }
        *      }
        *   }
        * }}}
        * 
        * For more, see [[LeftBias$ LeftBias]].
        */ 
      abstract class AbstractOps[A,B]( target : Either[A,B] )( opsTypeClass : Either.LeftBias.withEmptyToken.Generic[B] ) {

        // monad ops
        /**
          * Binds the given function across `Left` of the target left-biased `Either`.
          *
          * {{{
          * Left(12).flatMap(x => Left("scala"))  // Left("scala")
          * Right(12).flatMap(x => Right("scala") // Right(12)
          * }}}
          * @param f The function to bind across `Left`.
          */
        def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = opsTypeClass.flatMap[A,B,BB,Z]( target )( f );
        /**
          * Maps the function argument through `Left` of the target left-biased `Either`.
          *
          * {{{
          * Left(12).map(_ + 2)  // Left(14)
          * Right(12).map(_ + 2) // Right(12)
          * }}}
          */
        def map[Z]( f : A => Z ) : Either[Z,B] = opsTypeClass.map( target )( f );
        /**
          * Returns the target left-biased `Either` unchanged if it is a `Left` ''and'' the predicate `p`
          * holds for its value, or if it is any `Right`.
          * 
          * Returns a `Right` containing the empty token if the target is a `Left` but predicate `p`
          * fails to hold. If no empty token has been set, a `java.util.NoSuchElementException` will
          * be thrown.
          *
          * {{{
          * val LeftBias = LeftBias.withEmptyToken[Int](-1);
          * import LeftBias._
          * 
          * Left(12).withFilter(_ > 10)  // Left(12)
          * Left(7).withFilter(_ > 10)   // Right(-1)
          * Right(12).withFilter(_ > 10) // Right(12)
          * }}}
          */
        def withFilter( p : A => Boolean ) : Either[A,B] = opsTypeClass.withFilter( target )( p );

        // extra ops
        /**
          * Returns `false` if the target left-biased `Either` is a `Right` or returns the result of the application of
          * the given function to the `Left` value.
          *
          * {{{
          * Left(12).exists(_ > 10)  // true
          * Left(7).exists(_ > 10)   // false
          * Right(12).exists(_ > 10) // false
          * }}}
          *
          */
        def exists( f : A => Boolean ) : Boolean = opsTypeClass.exists( target )( f );
        /**
          * Returns `true` if the target left-biased `Either` is `Right` or returns 
          * the result of the application of the given function to the `Left` value.
          *
          * {{{
          * Left(12).forall(_ > 10)  // true
          * Left(7).forall(_ > 10)   // false
          * Right(12).forall(_ > 10) // true
          * }}}
          *
          */
        def forall( f : A => Boolean ) : Boolean = opsTypeClass.forall( target )( f );
        /**
          * Executes the given side-effecting function if the target left-biased `Either` is a `Left`.
          *
          * {{{
          * Left(12).foreach(x => println(x))  // prints "12"
          * Right(12).foreach(x => println(x)) // doesn't print
          * }}}
          * 
          * @param f The side-effecting function to execute.
          */
        def foreach[U]( f : A => U ) : Any = opsTypeClass.foreach( target )( f );
        /**
          * Returns the value if the target left-biased `Either` is a `Left` or 
          * throws `java.util.NoSuchElementException` if the target is a `Right`.
          *
          * {{{
          * Left(12).get  // 12
          * Right(12).get // NoSuchElementException
          * }}}
          *
          * @throws java.util.NoSuchElementException if the target is [[scala.util.Right]]
          */
        def get : A = opsTypeClass.get( target );
        /**
          * Returns the value from the target left-biased `Either` if it is a `Left`,
          * or the argument `or` if it is a `Right`.
          *
          * {{{
          * Left(12).getOrElse(17)  // 12
          * Right(12).getOrElse(17) // 17
          * }}}
          *
          */
        def getOrElse[AA >: A ]( or : =>AA ) : AA = opsTypeClass.getOrElse[A,AA,B]( target )( or );
        /**
          * Returns a `Some` containing the `Left` value of the target left-biased `Either` if it exists or a
          * `None` if the target is a `Right`.
          *
          * {{{
          * Left(12).toOption  // Some(12)
          * Right(12).toOption // None
          * }}}
          */
        def toOption : Option[A] = opsTypeClass.toOption( target );
        /**
          * Returns a `Seq` containing the `Left` value if it exists or an empty
          * `Seq` if the target left-biased `Either` is a `Right`.
          *
          * {{{
          * Left(12).toSeq  // Seq(12)
          * Right(12).toSeq // Seq()
          * }}}
          */
        def toSeq : collection.Seq[A] = opsTypeClass.toSeq( target );
        /**
          *  Returns `true` if this left-biased `Either` is a `Right`
          *  that contains the empty token, signifying empty. Returns `false`
          *  for any other error or value.
          */ 
        def isEmpty : Boolean = opsTypeClass.isEmpty( target );
        /**
          * "cross-get" -- Returns the value if the target left-biased `Either` 
          * does ''not'' conform to its bias, that is, if it is a `Right`.
          * Throws `java.util.NoSuchElementException` if the target is a `Left`.
          *
          * {{{
          * Left(12).xget  // NoSuchElementException
          * Right(12).xget // 12
          * }}}
          *
          * @throws java.util.NoSuchElementException if the target is [[scala.util.Left]]
          */
        def xget : B = opsTypeClass.xget( target );
        /**
          * "cross-getOrElse" -- Returns the value from the target left-biased `Either` 
          * if it does ''not'' conform to its bias and is a `Right`.
          * Returns the argument `or` if it is a `Left`.
          *
          * {{{
          * Right(12).xgetOrElse(17) // 12
          * Left(12).xgetOrElse(17)  // 17
          * }}}
          *
          */
        def xgetOrElse[BB>:B]( or : =>BB ) : BB = opsTypeClass.xgetOrElse[A,B,BB]( target )( or );
        /**
          * "cross-map" -- Maps the function argument through `Right` ''against'' the bias the target left-biased `Either`.
          *
          * {{{
          * Left(12).map(_ + 2)  // Left(12)
          * Right(12).map(_ + 2) // Right(14)
          * }}}
          */
        def xmap[Z]( f : B => Z ) : Either[A,Z] = opsTypeClass.xmap( target )( f );
        /**
          * If the target left-biased `Either` is a `Right` containing the empty token,
          * returns a `Right` containing `replacement`. Otherwise returns the target `Either` unchanged.
          *
          * {{{
          * // with empty token Int -1
          * 
          * Left(12).replaceIfEmpty(99)  // Left(12)
          * Left(-1).replaceIfEmpty(99)  // Left(-1)
          * Right(12).replaceIfEmpty(99) // Right(12)
          * Right(-1).replaceIfEmpty(99) // Right(99)
          * }}}
          */
        def replaceIfEmpty[BB>:B]( replacement : =>BB ) : Either[A,BB] = opsTypeClass.replaceIfEmpty[A,B,BB]( target )( replacement )
        /**
          * Returns `true` when called on a wrapper representing a left-biased `Either`, `false` otherwise.
          */
        def isLeftBiased : Boolean = opsTypeClass.isLeftBias;
        /**
          * Returns `true` when called on a wrapper representing a right-biased `Either`, `false` otherwise.
          */
        def isRightBiased : Boolean = opsTypeClass.isRightBias;
        /**
          * Returns `true` if this wrapper represents a left-biased `Either` wrapping a `Left` or a 
          * right-biased `Either` wrapping a `Right`. Returns false if the bias of this wrapper is not
          * consistent with the type of the target `Either`.
          * 
          * It is safe to call [[get]] on a biased `Either` for which `conformsToBias` is `true`.
          * 
          * It is safe to call [[xget]] on a biased `Either` for which `conformsToBias` is `false`.
          */
        def conformsToBias : Boolean = opsTypeClass.conformsToBias( target );
      }

      /**
        * Implements the operations of a left-biased `Either`, whose specific behavior -- especially with respect
        * to handling of an empty value resulting from a filter or failed pattern match -- is defined by
        * constructor argument `opsTypeClass`.
        * 
        * Please see [[LeftBias$ LeftBias]]
        */ 
      implicit final class Ops[A,B]( target : Either[A,B] )( implicit opsTypeClass : Either.LeftBias.withEmptyToken.Generic[B] ) extends AbstractOps( target )( opsTypeClass );

      /**
        * A typeclass that defines the implementation of a left-biased `Either`
        * 
        * For more, see [[LeftBias$ LeftBias]]
        */
      trait Generic[+E] extends Either.Bias[E] {
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Right should be created on each
         * invocation.
         */ 
        protected def rightEmpty : Right[Nothing,E]; 

        def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean;

        // monad ops
        def flatMap[A, B>:E, BB>:B ,Z]( target : Either[A,B] )( f : A => Either[Z,BB] ) : Either[Z,BB] = {
          target match {
            case Left( a )  => f( a )
            case Right( _ ) => target.asInstanceOf[Right[Z,B]]
          }
        }
        def map[A, B>:E, Z]( target : Either[A,B] )( f : A => Z ) : Either[Z,B] = {
          target match {
            case Left( a )  => Left( f( a ) )
            case Right( _ ) => target.asInstanceOf[Right[Z,B]]
          }
        }
        def withFilter[A,B>:E]( target : Either[A,B] )( p : A => Boolean ) : Either[A,B] = {
          target match {
            case l @  Left( a ) => if ( p(a) ) l else rightEmpty;
            case     Right( _ ) => target;
          }
        }

        // extra ops
        def exists[A,B>:E]( target : Either[A,B] )( f : A => Boolean ) : Boolean = {
          target match {
            case Left( a )  => f(a);
            case Right( _ ) => false;
          }
        }
        def forall[A,B>:E]( target : Either[A,B] )( f : A => Boolean ) : Boolean = {
          target match {
            case Left( a )  => f(a)
            case Right( _ ) => true;
          }
        }
        def foreach[A,B>:E,U]( target : Either[A,B] )( f : A => U ) : Any = {
          target match {
            case Left( a )  => f(a);
            case Right( _ ) => ();
          }
        }
        def get[A,B>:E]( target : Either[A,B] ) : A = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => throw new NoSuchElementException( NoSuchLeftMessage );
          }
        }
        def getOrElse[A, AA>:A, B>:E]( target : Either[A,B] )( or : =>AA ) : AA = {
          target match {
            case Left( a )  => a;
            case Right( _ ) => or;
          }
        }
        def toOption[A,B>:E]( target : Either[A,B] ) : Option[A] = {
          target match {
            case Left( a )  => Some( a );
            case Right( _ ) => None; 
          }
        }
        def toSeq[A,B>:E]( target : Either[A,B] ) : collection.Seq[A] = {
          target match {
            case Left( a )  => collection.Seq( a );
            case Right( _ ) => collection.Seq.empty[A];
          }
        }
        def xget[A,B>:E]( target : Either[A,B] ) : B = {
          target match {
            case Left( _ )  => throw new NoSuchElementException( NoSuchXRightMessage );
            case Right( b ) => b;
          }
        }
        def xgetOrElse[A,B>:E,BB>:B]( target : Either[A,B] )( or : =>BB ) : BB = {
          target match {
            case Left( _ )  => or;
            case Right( b ) => b;
          }
        }
        def xmap[A,B>:E,Z]( target : Either[A,B] )( f : B => Z ) : Either[A,Z] = {
          target match {
            case Left( _ )  => target.asInstanceOf[Left[A,Z]]
            case Right( b ) => Right( f(b) )
          }
        }
        def replaceIfEmpty[A,B>:E,BB>:B]( target : Either[A,B] )( replacement : =>BB ) : Either[A,BB] = {
          if (isEmpty( target )) Right( replacement ) else target;
        }
        def isLeftBias  : Boolean = true;

        implicit def toOps[A,B>:E]( target : Either[A,B] ) : LeftBias.withEmptyToken.Ops[A,B] = new LeftBias.withEmptyToken.Ops[A,B]( target )( this )
      }
      def apply[E]( token : E ) : withEmptyToken[E] = new withEmptyToken( token );

      /**
        * A factory for a typeclass instance which implements the operations of a left-biased `Either` 
        * that signals empty by throwing some Throwable as specified in a by-name parameter.
        * 
        * Typical use
        * {{{
        *   val LeftBias = Either.LeftBias.withEmptyToken.Throwing( new NoValueAvailableException ); 
        *   import LeftBias._
        * }}}
        * For more, please see [[LeftBias$ LeftBias]].
        */
      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      /**
        * A typeclass instance which implements the operations of a left-biased `Either` 
        * that signals empty by throwing some Throwable as specified in a by-name parameter.
        * 
        * Typical use
        * {{{
        *   val LeftBias = Either.LeftBias.withEmptyToken.Throwing( new NoValueAvailableException ); 
        *   import LeftBias._
        * }}}
        * For more, please see [[LeftBias$ LeftBias]].
        */
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends withEmptyToken.Generic[Nothing] {
        override protected def rightEmpty : Nothing = empty;

        override def empty : Nothing = throw throwableBuilder;

        override def isEmpty[A,B]( target : Either[A,B] ) : Boolean = false; // no state represents empty, empties cannot be formed as an Exception is thrown when it is tried
      }
    }
    /**
      * A typeclass instance which implements the operations of a left-biased `Either`. 
      * An unsuccessful filter or pattern match will result in a Right containing a specified empty token of type `E`.
      * 
      * Typical use
      * {{{
      *   val LeftBias = Either.LeftBias.withEmptyToken(-1); //suitable for Either[A,Int]
      *   import LeftBias._
      * }}}
      * For more, please see [[LeftBias$ LeftBias]].
      */
    final class withEmptyToken[+E] private( override val empty : E ) extends withEmptyToken.Generic[E] {
      override protected val rightEmpty : Right[Nothing,E] = Right(empty);

      override def isEmpty[A>:E,B]( target : Either[A,B] ) : Boolean = (target == rightEmpty);
    }
    /**
      * May be extended by classes, objects, traits, and packages (via package objects).
      * 
      * Within entities that extend this class
      * objects of type `Either` will be left-biased, and have
      * the full-suite of left-biased operations (including operations suitable to for
      * comprehensions) automatically made available.
      * 
      * Empty `Either` values will be represented by Right values containing the token 
      * of type `R` passed to the superclass constructor.
      * 
      * '''If you are using polymorphic error types, be sure to specify the type
      *  argument to Base, as too narrow a type may be inferred from a token
      *  of a derived type.''' 
      *  In English, this means you should extend `Either.LeftBias.Base[Err]( ErrEmpty )`, not just `Either.LeftBias.Base( ErrEmpty )`.
      * 
      * ''Note: Entities that do not need to extend another class can extend the abstract
      * class [[Either.LeftBias.Base]], which allows defining the Empty token in the
      * superclass constructor rather than overriding [[EmptyTokenDefinition]].''
      * 
      * For more information, please see [[LeftBias$ LeftBias]]
      * 
      * {{{
      * class Clown extends Either.LeftBias.Base[Clown.Error]( Clown.Error.Empty ) {
      *   import Clown.{Cash,Laugh,Error};
      * 
      *   // use Either values directly in for comprehensions
      *   def perform : Either[Cash,Error] = {
      *      for {
      *        laugh <- makeFunny
      *        money <- begForCash( laugh ) if laugh.loudness > 2
      *      } yield money
      *   }
      * 
      *   def makeFunny : Either[Laugh,Error] = ???
      *   def begForCash( laugh : Laugh )  : Either[Cash,Error] = ???
      * }
      * object Clown {
      *   final object Error {
      *     case object Empty extends Error;
      *     case object JokeBombed extends Error;
      *     case object NoAudience extends Error;
      *     case object Cheapskates extends Error;
      *   }
      *   sealed trait Error;
      * 
      *   case class Laugh( loudness : Int );
      *   case class Cash( quantity : Int, currencyCode : String );
      * }
      * }}}
      * 
      */ 
    abstract class Base[R]( emptyToken : R ) extends LeftBias[R] {
      override val EmptyTokenDefinition = LeftBias.withEmptyToken[R]( emptyToken )
    }
  }
  /**
    * Left-biasing trait that may be extended by classes, objects, traits, and packages (via package objects).
    * 
    * Within entities that extend this trait
    * objects of type `Either` will be left-biased, and have
    * the full-suite of left-biased operations (including operations suitable to for
    * comprehensions) automatically made available.
    * 
    * By default, transformation of a left-biased `Either` to empty by a failed pattern
    * match or filter operation will provoke a `java.util.NoSuchElementException`. 
    * Override [[EmptyTokenDefinition]] to prevent this and have emptiness
    * represented by an error token of type `R`.
    * 
    * ''Note: Entities that do not need to extend another class can extend the abstract
    * class [[Either.LeftBias.Base]], which allows defining the Empty token in the
    * superclass constructor rather than overriding [[EmptyTokenDefinition]].''
    * 
    * For more information, please see [[LeftBias$ LeftBias]]
    * 
    * {{{
    * class Clown extends Either.LeftBias[Clown.Error] {
    *   import Clown.{Cash,Laugh,Error};
    * 
    *   // override to specify an empty token
    *   override val EmptyTokenDefinition = Either.LeftBias.withEmptyToken(Error.Empty);
    * 
    *   // use Either values directly in for comprehensions
    *   def perform : Either[Cash,Error] = {
    *      for {
    *        laugh <- makeFunny
    *        money <- begForCash( laugh ) if laugh.loudness > 2
    *      } yield money
    *   }
    * 
    *   def makeFunny : Either[Laugh,Error] = ???
    *   def begForCash( laugh : Laugh )  : Either[Cash,Error] = ???
    * }
    * object Clown {
    *   final object Error {
    *     case object Empty extends Error;
    *     case object JokeBombed extends Error;
    *     case object NoAudience extends Error;
    *     case object Cheapskates extends Error;
    *   }
    *   sealed trait Error;
    * 
    *   case class Laugh( loudness : Int );
    *   case class Cash( quantity : Int, currencyCode : String );
    * }
    * }}}
    * 
    */ 
  trait LeftBias[R] { // we use R rather than B here to emphasize to users that it is the right-side type that must be specified here
    /**
      * Override this value to specify an empty token of type `R`. A Right object with this
      * value will be returned by a failed filter or pattern match operation. If this
      * value is not overridden, the default behavior will be to throw a `java.util.NoSuchElementException`
      * on an unmatched pattern or filter. 
      * {{{
      *   // suppose errors on left-biased Either values are represented with Int codes
      *   // with -1 representing "empty"
      *   override val EmptyTokenDefinition = Either.LeftBias.withEmptyToken(-1);
      * }}}
      */ 
    val EmptyTokenDefinition : Either.LeftBias.withEmptyToken.Generic[R] = LeftBias.DefaultThrowingOps;

    implicit def toLeftBiasEtherOps[L]( target : Either[L,R] ) : LeftBias.withEmptyToken.AbstractOps[L,R] = new LeftBias.withEmptyToken.Ops[L,R]( target )( EmptyTokenDefinition );
  }

  private def noSuchElementMessage[A,B]( rightBias : Boolean, mbEither : Option[Either[A,B]] = None ) = {
    val bias = if ( rightBias ) "Right-biased" else "Left-biased";
    val withToken = if ( rightBias ) "RightBias.withEmptyToken" else "LeftBias.withEmptyToken";
    val eitherRep = mbEither.fold(" ")( either => s" '${either}' " );
    s"${bias} Either${eitherRep}filtered to empty or failed to match a pattern. Consider using ${withToken}"
  }

  private val NoSuchLeftMessage = "Can't get a value from a left-biased Either which is in fact a Right.";
  private val NoSuchRightMessage = "Can't get a value from a right-biased Either which is in fact a Left.";
  private val NoSuchXLeftMessage = "This right-biased either is in fact a Right. xget requires a value against its bias."
  private val NoSuchXRightMessage = "This left-biased either is in fact a Left. xget requires a value against its bias."
}
