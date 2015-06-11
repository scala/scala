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
 *  To support multiple projections as generators in for-comprehensions, the Either
 *  type also defines a `flatMap` method.
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
     * Returns the value from this `Left` or throws `java.util.NoSuchElementException`
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
     * Returns the value from this `Right` or throws
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

  trait WithEmpty[+E] {
    def empty : E;
  }
  final object RightBias {

    private[Either] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( true ) ) );

    implicit class Ops[A,B]( val src : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = DefaultThrowingOps.flatMap[A,AA,B,Z]( src )( f );
      def map[Z]( f : B => Z )                         : Either[A,Z]  = DefaultThrowingOps.map( src )( f );
      def withFilter( p : B => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( src )( p );

      // extra ops
      def exists( f : B => Boolean )        : Boolean           = DefaultThrowingOps.exists( src )( f );
      def forall( f : B => Boolean )        : Boolean           = DefaultThrowingOps.forall( src )( f );
      def foreach[U]( f : B => U )          : Any               = DefaultThrowingOps.foreach( src )( f );
      def get                               : B                 = DefaultThrowingOps.get( src );
      def getOrElse[ BB >: B ]( or : =>BB ) : BB                = DefaultThrowingOps.getOrElse[A,B,BB]( src )( or );
      def toOption                          : Option[B]         = DefaultThrowingOps.toOption( src );
      def toSeq                             : collection.Seq[B] = DefaultThrowingOps.toSeq( src );
    }

    object withEmptyToken {
      abstract class AbstractOps[A,B]( src : Either[A,B] )( opsTypeClass : Either.RightBias.withEmptyToken.Generic[A] ) {

        // monad ops
        def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = opsTypeClass.flatMap[A,AA,B,Z]( src )( f );
        def map[Z]( f : B => Z )                         : Either[A,Z]  = opsTypeClass.map( src )( f );
        def withFilter( p : B => Boolean )               : Either[A,B]  = opsTypeClass.withFilter( src )( p );

        // extra ops
        def exists( f : B => Boolean )        : Boolean           = opsTypeClass.exists( src )( f );
        def forall( f : B => Boolean )        : Boolean           = opsTypeClass.forall( src )( f );
        def foreach[U]( f : B => U )          : Any               = opsTypeClass.foreach( src )( f );
        def get                               : B                 = opsTypeClass.get( src );
        def getOrElse[ BB >: B ]( or : =>BB ) : BB                = opsTypeClass.getOrElse[A,B,BB]( src )( or );
        def toOption                          : Option[B]         = opsTypeClass.toOption( src );
        def toSeq                             : collection.Seq[B] = opsTypeClass.toSeq( src );
      }

      implicit final class Ops[A,B]( src : Either[A,B] )( implicit opsTypeClass : Either.RightBias.withEmptyToken.Generic[A] ) extends AbstractOps( src )( opsTypeClass );

      trait Generic[+E] extends Either.WithEmpty[E]{
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Left should be created on each
         * invocation.
         */ 
        protected def leftEmpty : Left[E,Nothing];

        // monad ops
        def flatMap[A>:E,AA>:A,B,Z]( src : Either[A,B] )( f : B => Either[AA,Z] ) : Either[AA,Z] = {
          src match {
            case Left( _ )  => src.asInstanceOf[Left[A,Z]]
            case Right( b ) => f( b )
          }
        }
        def map[A>:E,B,Z]( src : Either[A,B] )( f : B => Z ) : Either[A,Z] = {
          src match {
            case Left( _ )  => src.asInstanceOf[Left[A,Z]]
            case Right( b ) => Right( f( b ) )
          }
        }
        def withFilter[A>:E,B]( src : Either[A,B] )( p : B => Boolean ) : Either[A,B] = {
          src match {
            case      Left( _ ) => src;
            case r @ Right( b ) => if ( p(b) ) r else leftEmpty;
          }
        }

        // extra ops
        def exists[A>:E,B]( src : Either[A,B] )( f : B => Boolean ) : Boolean = {
          src match {
            case Left( _ )  => false;
            case Right( b ) => f( b );
          }
        }
        def forall[A>:E,B]( src : Either[A,B] )( f : B => Boolean ) : Boolean = {
          src match {
            case Left( _ )  => true;
            case Right( b ) => f( b );
          }
        }
        def foreach[A>:E,B,U]( src : Either[A,B] )( f : B => U ) : Any = {
          src match {
            case Left( _ )  => ();
            case Right( b ) => f( b );
          }
        }
        def get[A>:E,B]( src : Either[A,B] ) : B = {
          src match {
            case Left( _ )  => throw new NoSuchElementException( NoSuchRightMessage );
            case Right( b ) => b;
          }
        }
        def getOrElse[A>:E,B,BB>:B]( src : Either[A,B] )( or : =>BB ) : BB = {
          src match {
            case Left( _ )  => or;
            case Right( b ) => b;
          }
        }
        def toOption[A>:E,B]( src : Either[A,B] ) : Option[B] = {
          src match {
            case Left( _ )  => None;
            case Right( b ) => Some( b );
          }
        }
        def toSeq[A>:E,B]( src : Either[A,B] ) : collection.Seq[B] = {
          src match {
            case Left( _ )  => collection.Seq.empty[B];
            case Right( b ) => collection.Seq( b );
          }
        }

        implicit def toOps[A>:E,B]( src : Either[A,B] ) : RightBias.withEmptyToken.Ops[A,B] = new RightBias.withEmptyToken.Ops[A,B]( src )( this )
      }
      def apply[E]( token : E ) : withEmptyToken[E] = new withEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends withEmptyToken.Generic[Nothing] {
        override protected def leftEmpty : Nothing = empty;

        override def empty : Nothing = throw throwableBuilder;
      }
    }
    final class withEmptyToken[+E] private( override val empty : E ) extends withEmptyToken.Generic[E] {
      override protected val leftEmpty : Left[E,Nothing] = Left(empty);
    }
  }
  trait RightBias[A] {
    val EmptyTokenDefinition : Either.RightBias.withEmptyToken.Generic[A] = RightBias.DefaultThrowingOps;

    implicit def toRightBiasEtherOps[B]( src : Either[A,B] ) : RightBias.withEmptyToken.AbstractOps[A,B] = new RightBias.withEmptyToken.Ops[A,B]( src )( EmptyTokenDefinition );
  }
  /**
    *  This object contains utilities for defining an environment in which `Either` instances are "left-biased".
    * 
    *  Left-biased means instances of `Either[A,B]` can be operated upon via methods that render thme 
    *  quite analogous to an `Option[A]`, except that failures are represented by a `Right` containing information about
    *  the problem rathar than by uninformative `None`. In particular,
    *  a left-biased `Either` can be used directly in a for comprehension. Unlike a [[scala.util.Either.LeftProjection]],
    *  a left-biased `Either` supports all features of a for comprehension, including pattern matching, filtering,
    *  and variable assignment.
    * 
    *  To enable left-biasing, you first define a bias, typically via the [[Either.LeftBias.withEmptyToken$ Either.LeftBias.withEmptyToken]]
    *  factory.
    * {{{
    *     // will impart a left-bias suitable to Either[A,String]
    *     val LeftBias = Either.LeftBias.withEmptyToken("EMPTY")
    *     import LeftBias._
    * }}}
    *  The specified "empty token" will define the value in the `Right` object that will be used to
    *  when a filter or pattern-match of a left-side object fails.
    * 
    *  If you don't wish to specify an empty token, you may use the more convenient
    * {{{
    *     import Either.LeftBias._
    * }}}
    *  However, filter operations or pattern match failures that would leave the left-side
    *  empty will result in the raising of a `java.util.NoSuchElementException`.
    * 
    *  For the full list of operations defined on a left-biased Either, see [[LeftBias.Ops]].
    * 
    *  '''A Detailed example:'''
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
    *     def loadURL( url : String ) : Option[Seq[Byte]] = ???
    *     def parseRecord( bytes : Seq[Byte] ) : Option[Record] = ???
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
    *  for things that can go wrong. (We'll let our error codes be `Int` for this example, but should probably 
    *  more descriptive kinds of object in real life.) Unconventionally (since we are considering left-biased
    *  `Either` here), we'll let good values arrive as `Left` values and embed our error codes in `Right` objects.
    * {{{
    *     val ErrIO              = 1
    *     val ErrNoParse         = 2
    *     val ErrBadYear         = 3
    *     val ErrProcessorFailed = 4
    * 
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String ) : Either[Seq[Byte],Int] = ???
    *     def parseRecord( bytes : Seq[Byte] ) : Either[Record,Int] = ???
    *     def process( opCode : Int, data : Seq[Byte] ) : Either[Result,Int] = ???
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
    *   pattern-matching (required to extract `opCode`, `data`, and `year`) or filtering (which we perform based on `year`).
    * 
    *   Instead, we can left-bias the `Either`:
    * {{{
    *     val ErrIO              = 1
    *     val ErrNoParse         = 2
    *     val ErrBadYear         = 3
    *     val ErrProcessorFailed = 4
    * 
    *     // left-bias Either 
    *     val LeftBias = Either.LeftBias.withEmptyToken( ErrBadYear ) 
    *     import LeftBias._
    * 
    *     case class Record( opCode : Int, data : Seq[Byte], year : Int )
    *     trait Result
    * 
    *     def loadURL( url : String ) : Either[Seq[Byte],Int] = ???
    *     def parseRecord( bytes : Seq[Byte] ) : Either[Record,Int] = ???
    *     def process( opCode : Int, data : Seq[Byte] ) : Either[Result,Int] = ???
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
    *   An "empty" `Either` here could only result from the failure of the year filter,
    *   so we set `ErrBadYear` to be the token supplied when the filtering fail. Note that
    *   once the left-biased operations have been imported, a sequence `Either` yielding
    *   values can be used directly in the for comprehension, without extractions of a projection
    *   with `.left`.
    * 
    *   The scope of the left-biasing is the scope of the import. In our example above, our choice
    *   of error token was very specific to this definition. In fact, it might have been better
    *   to narrow the scope of the bias to within the `processURL(...)` function.
    * {{{
    *     // same definitions as above
    *     // except the import statement sits within the method
    *     def processURL( url : String ) : Either[Result,Int] = {
    *       import LeftBias._
    *       for {
    *         rawBytes                     <- loadURL( url )
    *         Record( opCode, data, year ) <- parseRecord( rawBytes )
    *         result                       <- process( opCode, data ) if year >= 2011
    *       } yield result
    *     }
    * }}}
    *   Other times, we might define a convention suitable for use within an entire
    *   package, in which case we can extend the trait [[Either.LeftBias! Either.LeftBias]]:
    * {{{
    *     package util {
    *       object Err {
    *         case object Empty extends Err( -1, "A series of operations yielded no suitable value." );
    *         case object Explosion extends Err( Int.MaxValue, "Boom!");
    *       }
    *       class Err( val code : Int, val description : String )
    *     }
    * 
    *     // `Either` will be left-biased in all traits, classes, and objects 
    *     // belonging package `mypkg`
    *     package object mypkg extends Either.LeftBias[util.Err] {
    *       override val EmptyTokenDefinition = Either.LeftBias.withEmptyToken(util.Err.Empty);
    *     }
    * }}}
    */ 
  final object LeftBias {

    private[Either] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( false ) ) );

    /**
      * A value class implementing the operations of a left-biased `Either`,
      * which throws a `java.util.NoSuchElementException` if a filter operation
      * or pattern match results in empty.
      * 
      * Typical use
      * {{{
      *   import Either.LeftBias._
      * }}}
      * For more, please see [[LeftBias$ LeftBias]].
      */
    implicit class Ops[A,B]( val src : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = DefaultThrowingOps.flatMap[A,B,BB,Z]( src )( f );
      def map[Z]( f : A => Z )                         : Either[Z,B]  = DefaultThrowingOps.map( src )( f );
      def withFilter( p : A => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( src )( p );

      // extra ops
      def exists( f : A => Boolean )       : Boolean           = DefaultThrowingOps.exists( src )( f );
      def forall( f : A => Boolean )       : Boolean           = DefaultThrowingOps.forall( src )( f );
      def foreach[U]( f : A => U )         : Any               = DefaultThrowingOps.foreach( src )( f );
      def get                              : A                 = DefaultThrowingOps.get( src );
      def getOrElse[AA >: A ]( or : =>AA ) : AA                = DefaultThrowingOps.getOrElse[A,AA,B]( src )( or );
      def toOption                         : Option[A]         = DefaultThrowingOps.toOption( src );
      def toSeq                            : collection.Seq[A] = DefaultThrowingOps.toSeq( src );
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
        *   implicit class LeftBiasOps( src : Either[A,Throwable] ) extends Either.LeftBias.withEmptyToken.AbstractOps( src )( LeftBias ) {
        *      def raise : Unit = {
        *        src match {
        *          case Left(_) => ();
        *          case Right( t ) => throw t;
        *        }
        *      }
        *   }
        * }}}
        */ 
      abstract class AbstractOps[A,B]( src : Either[A,B] )( opsTypeClass : Either.LeftBias.withEmptyToken.Generic[B] ) {

        // monad ops
        def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = opsTypeClass.flatMap[A,B,BB,Z]( src )( f );
        def map[Z]( f : A => Z )                         : Either[Z,B]  = opsTypeClass.map( src )( f );
        def withFilter( p : A => Boolean )               : Either[A,B]  = opsTypeClass.withFilter( src )( p );

        // extra ops
        def exists( f : A => Boolean )       : Boolean           = opsTypeClass.exists( src )( f );
        def forall( f : A => Boolean )       : Boolean           = opsTypeClass.forall( src )( f );
        def foreach[U]( f : A => U )         : Any               = opsTypeClass.foreach( src )( f );
        def get                              : A                 = opsTypeClass.get( src );
        def getOrElse[AA >: A ]( or : =>AA ) : AA                = opsTypeClass.getOrElse[A,AA,B]( src )( or );
        def toOption                         : Option[A]         = opsTypeClass.toOption( src );
        def toSeq                            : collection.Seq[A] = opsTypeClass.toSeq( src );
      }

      /**
        * Implements the operations of a left-biased `Either`, whose specific behavior -- especially with respect
        * to handling of an empty value resulting from a filter or failed pattern match -- is defined by
        * constructor argument `opsTypeClass`.
        */ 
      implicit final class Ops[A,B]( src : Either[A,B] )( implicit opsTypeClass : Either.LeftBias.withEmptyToken.Generic[B] ) extends AbstractOps( src )( opsTypeClass );

      trait Generic[+E] extends Either.WithEmpty[E]{
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Right should be created on each
         * invocation.
         */ 
        protected def rightEmpty : Right[Nothing,E]; 

        // monad ops
        def flatMap[A, B>:E, BB>:B ,Z]( src : Either[A,B] )( f : A => Either[Z,BB] ) : Either[Z,BB] = {
          src match {
            case Left( a )  => f( a )
            case Right( _ ) => src.asInstanceOf[Right[Z,B]]
          }
        }
        def map[A, B>:E, Z]( src : Either[A,B] )( f : A => Z ) : Either[Z,B] = {
          src match {
            case Left( a )  => Left( f( a ) )
            case Right( _ ) => src.asInstanceOf[Right[Z,B]]
          }
        }
        def withFilter[A,B>:E]( src : Either[A,B] )( p : A => Boolean ) : Either[A,B] = {
          src match {
            case l @  Left( a ) => if ( p(a) ) l else rightEmpty;
            case     Right( _ ) => src;
          }
        }

        // extra ops
        def exists[A,B>:E]( src : Either[A,B] )( f : A => Boolean ) : Boolean = {
          src match {
            case Left( a )  => f(a);
            case Right( _ ) => false;
          }
        }
        def forall[A,B>:E]( src : Either[A,B] )( f : A => Boolean ) : Boolean = {
          src match {
            case Left( a )  => f(a)
            case Right( _ ) => true;
          }
        }
        def foreach[A,B>:E,U]( src : Either[A,B] )( f : A => U ) : Any = {
          src match {
            case Left( a )  => f(a);
            case Right( _ ) => ();
          }
        }
        def get[A,B>:E]( src : Either[A,B] ) : A = {
          src match {
            case Left( a )  => a;
            case Right( _ ) => throw new NoSuchElementException( NoSuchLeftMessage );
          }
        }
        def getOrElse[A, AA>:A, B>:E]( src : Either[A,B] )( or : =>AA ) : AA = {
          src match {
            case Left( a )  => a;
            case Right( _ ) => or;
          }
        }
        def toOption[A,B>:E]( src : Either[A,B] ) : Option[A] = {
          src match {
            case Left( a )  => Some( a );
            case Right( _ ) => None; 
          }
        }
        def toSeq[A,B>:E]( src : Either[A,B] ) : collection.Seq[A] = {
          src match {
            case Left( a )  => collection.Seq( a );
            case Right( _ ) => collection.Seq.empty[A];
          }
        }

        implicit def toOps[A,B>:E]( src : Either[A,B] ) : LeftBias.withEmptyToken.Ops[A,B] = new LeftBias.withEmptyToken.Ops[A,B]( src )( this )
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
    }
  }
  /**
    * May be extends by classes, objects, traits, and packages (via package objects).
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

    implicit def toLeftBiasEtherOps[A]( src : Either[A,R] ) : LeftBias.withEmptyToken.AbstractOps[A,R] = new LeftBias.withEmptyToken.Ops[A,R]( src )( EmptyTokenDefinition );
  }

  private def noSuchElementMessage[A,B]( rightBias : Boolean, mbEither : Option[Either[A,B]] = None ) = {
    val bias = if ( rightBias ) "Right-biased" else "Left-biased";
    val withToken = if ( rightBias ) "RightBias.withEmptyToken" else "LeftBias.withEmptyToken";
    val eitherRep = mbEither.fold(" ")( either => s" '${either}' " );
    s"${bias} Either${eitherRep}filtered to empty or failed to match a pattern. Consider using ${withToken}"
  }

  private val NoSuchLeftMessage = "Can't get a value from a left-biased Either which is in fact a Right.";
  private val NoSuchRightMessage = "Can't get a value from a right-biased Either which is in fact a Left.";
}
