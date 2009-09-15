/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** <p>
 *   The <code>Either</code> type represents a value of one of two possible
 *   types (a disjoint union). The data constructors <code>Left</code> and
 *   <code>Right</code> represent the two possible values.
 *   The <code>Either</code> type is often used as an alternative to
 *   <code>scala.Option</code> where <code>Left</code> represents failure
 *   (by convention) and <code>Right</code> is akin to <code>Some</code>.
 *  </p>
 *
 *  @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 *  @version 1.0, 11/10/2008
 *  @since 2.7
 */
sealed abstract class Either[+A, +B] {
  /**
   * Projects this <code>Either</code> as a <code>Left</code>.
   */
  def left = Either.LeftProjection(this)

  /**
   * Projects this <code>Either</code> as a <code>Right</code>.
   */
  def right = Either.RightProjection(this)

  /**
   * Deconstruction of the <code>Either</code> type (in contrast to pattern matching).
   */
  def fold[X](fa: A => X, fb: B => X) = this match {
    case Left(a) => fa(a)
    case Right(b) => fb(b)
  }

  /**
   * If this is a <code>Left</code>, then return the left value in <code>Right</code> or vice versa.
   */
  def swap = this match {
    case Left(a) => Right(a)
    case Right(b) => Left(b)
  }

  /**
   * Returns <code>true</code> if this is a <code>Left</code>, <code>false</code> otherwise.
   */
  def isLeft = false  // Default here, overriden in Left

  /**
   * Returns <code>true</code> if this is a <code>Right</code>, <code>false</code> otherwise.
   */
  def isRight = false // Default here, overriden in Right.
}

/**
 * The left side of the disjoint union, as opposed to the <code>Right</code> side.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 * @version 1.0, 11/10/2008
 */
final case class Left[+A, +B](a: A) extends Either[A, B] { override def isLeft = true }

/**
 * The right side of the disjoint union, as opposed to the <code>Left</code> side.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 * @version 1.0, 11/10/2008
 */
final case class Right[+A, +B](b: B) extends Either[A, B] { override def isRight = true }

object Either {

  /**
   * Returns the <code>Left</code> values in the given <code>Iterable</code> of <code>Either</code>s.
   */
  @deprecated("use `for (Left(a) <- es) yield a'")
  def lefts[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[List[A]](Nil)((e, as) => e match {
      case Left(a) => a :: as
      case Right(_) => as
    })

  /**
   * Returns the <code>Right</code> values in the given<code>Iterable</code> of  <code>Either</code>s.
   */
  @deprecated("use `for (Right(a) <- es) yield a'")
  def rights[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[List[B]](Nil)((e, bs) => e match {
      case Left(_) => bs
      case Right(b) => b :: bs
    })

  /** Transforms an Iterable of Eithers into a pair of lists.
   *
   *  @param xs the iterable of Eithers to separate
   *  @return a pair of lists.
   */
  @deprecated("use `for ((Left(l), Right(r)) <- es partition isLeft) yield (l, r)'")
  def separate[A,B](es: Iterable[Either[A,B]]): (List[A], List[B]) =
      es.foldRight[(List[A], List[B])]((Nil, Nil)) {
      case (Left(a), (lefts, rights)) => (a :: lefts, rights)
      case (Right(b), (lefts, rights)) => (lefts, b :: rights)
    }

  /**
   * Projects an <code>Either</code> into a <code>Left</code>.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
   * @version 1.0, 11/10/2008
   */
  final case class LeftProjection[+A, +B](e: Either[A, B]) {
    /**
     * Returns the value from this <code>Left</code> or throws <code>Predef.NoSuchElementException</code>
     * if this is a <code>Right</code>.
     *
     * @throws Predef.NoSuchElementException if the option is empty.
     */
    def get = e match {
      case Left(a) => a
      case Right(_) =>  throw new NoSuchElementException("Either.left.value on Right")
    }

    /**
     * Executes the given side-effect if this is a <code>Left</code>.
     *
     * @param e The side-effect to execute.
     */
    def foreach[U](f: A => U) = e match {
      case Left(a) => f(a)
      case Right(_) => {}
    }

    /**
     * Returns the value from this <code>Left</code> or the given argument if this is a
     * <code>Right</code>.
     */
    def getOrElse[AA >: A](or: => AA) = e match {
      case Left(a) => a
      case Right(_) => or
    }

    /**
     * Returns <code>true</code> if <code>Right</code> or returns the result of the application of
     * the given function to the <code>Left</code> value.
     */
    def forall(f: A => Boolean) = e match {
      case Left(a) => f(a)
      case Right(_) => true
    }

    /**
     * Returns <code>false</code> if <code>Right</code> or returns the result of the application of
     * the given function to the <code>Left</code> value.
     */
    def exists(f: A => Boolean) = e match {
      case Left(a) => f(a)
      case Right(_) => false
    }

    /**
     * Binds the given function across <code>Left</code>.
     *
     * @param The function to bind across <code>Left</code>.
     */
    def flatMap[BB >: B, X](f: A => Either[X, BB]) = e match {
      case Left(a) => f(a)
      case Right(b) => Right(b)
    }

    /**
     * Maps the function argument through <code>Left</code>.
     */
    def map[X](f: A => X) = e match {
      case Left(a) => Left(f(a))
      case Right(b) => Right(b)
    }

    /**
     * Returns <code>None</code> if this is a <code>Right</code> or if the given predicate
     * <code>p</code> does not hold for the left value, otherwise, returns a <code>Left</code>.
     */
    def filter[Y](p: A => Boolean): Option[Either[A, Y]] = e match {
      case Left(a) => if(p(a)) Some(Left(a)) else None
      case Right(b) => None
    }

    /**
     * Returns a <code>Seq</code> containing the <code>Left</code> value if it exists or an empty
     * <code>Seq</code> if this is a <code>Right</code>.
     */
    def toSeq = e match {
      case Left(a) => Seq(a)
      case Right(_) => Seq.empty
    }

    /**
     * Returns a <code>Some</code> containing the <code>Left</code> value if it exists or a
     * <code>None</code> if this is a <code>Right</code>.
     */
    def toOption = e match {
      case Left(a) => Some(a)
      case Right(_) => None
    }
  }

  /**
   * Projects an <code>Either</code> into a <code>Right</code>.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
   * @version 1.0, 11/10/2008
   */
  final case class RightProjection[+A, +B](e: Either[A, B]) {
    /**
     * Returns the value from this <code>Right</code> or throws
     * <code>Predef.NoSuchElementException</code> if this is a <code>Left</code>.
     *
     * @throws Predef.NoSuchElementException if the projection is <code>Left</code>.
     */
    def get = e match {
      case Left(_) =>  throw new NoSuchElementException("Either.right.value on Left")
      case Right(a) => a
    }

    /**
     * Executes the given side-effect if this is a <code>Right</code>.
     *
     * @param e The side-effect to execute.
     */
    def foreach[U](f: B => U) = e match {
      case Left(_) => {}
      case Right(b) => f(b)
    }

    /**
     * Returns the value from this <code>Right</code> or the given argument if this is a
     * <code>Left</code>.
     */
    def getOrElse[BB >: B](or: => BB) = e match {
      case Left(_) => or
      case Right(b) => b
    }

    /**
     * Returns <code>true</code> if <code>Left</code> or returns the result of the application of
     * the given function to the <code>Right</code> value.
     */
    def forall(f: B => Boolean) = e match {
      case Left(_) => true
      case Right(b) => f(b)
    }

    /**
     * Returns <code>false</code> if <code>Left</code> or returns the result of the application of
     * the given function to the <code>Right</code> value.
     */
    def exists(f: B => Boolean) = e match {
      case Left(_) => false
      case Right(b) => f(b)
    }

    /**
     * Binds the given function across <code>Right</code>.
     *
     * @param The function to bind across <code>Right</code>.
     */
    def flatMap[AA >: A, Y](f: B => Either[AA, Y]) = e match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }

    /**
     * Maps the function argument through <code>Right</code>.
     */
    def map[Y](f: B => Y) = e match {
      case Left(a) => Left(a)
      case Right(b) => Right(f(b))
    }

    /** Returns <code>None</code> if this is a <code>Left</code> or if the
     *  given predicate <code>p</code> does not hold for the right value,
     *  otherwise, returns a <code>Right</code>.
     */
    def filter[X](p: B => Boolean): Option[Either[X, B]] = e match {
      case Left(_) => None
      case Right(b) => if(p(b)) Some(Right(b)) else None
    }

    /** Returns a <code>Seq</code> containing the <code>Right</code> value if
     *  it exists or an empty <code>Seq</code> if this is a <code>Left</code>.
     */
    def toSeq = e match {
      case Left(_) => Seq.empty
      case Right(b) => Seq(b)
    }

    /** Returns a <code>Some</code> containing the <code>Right</code> value
     *  if it exists or a <code>None</code> if this is a <code>Left</code>.
     */
    def toOption = e match {
      case Left(_) => None
      case Right(b) => Some(b)
    }
  }

  /**
   * Joins an <code>Either</code> through <code>Left</code>.
   */
  def joinLeft[A, B](es: Either[Either[A, B], B]) =
    es.left.flatMap(x => x)

  /**
   * Joins an <code>Either</code> through <code>Right</code>.
   */
  def joinRight[A, B](es: Either[A, Either[A, B]]) =
    es.right.flatMap(x => x)

  /**
   * Takes an <code>Either</code> to its contained value within <code>Left</code> or
   * <code>Right</code>.
   */
  def merge[T](e: Either[T, T]) = e match {
    case Left(t) => t
    case Right(t) => t
  }

  /** If the condition satisfies, return the given A in <code>Left</code>,
   *  otherwise, return the given B in <code>Right</code>.
   */
  def cond[A, B](test: Boolean, right: => B, left: => A) =
    if(test) Right(right) else Left(left)
}
