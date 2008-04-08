/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

/**
 * <p>
 * The <code>Either</code> type represents a value of one of two possible types (a disjoint union).
 * The data constructors; <code>Left</code> and <code>Right</code> represent the two possible
 * values. The <code>Either</code> type is often used as an alternative to
 * <code>scala.Option</code> where <code>Left</code> represents failure (by convention) and
 * <code>Right</code> is akin to <code>Some</code>.
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 * @version 1.0, 06/02/2008
 */
sealed trait Either[+A, +B] {
  /**
   * Returns the value from this <code>Left</code> or fails with the given message if this is a
   * <code>Right</code>.
   */
  def leftE(err: => String) = this match {
    case Left(a) => a
    case Right(_) => error(err)
  }

  /**
   * Returns the value from this <code>Right</code> or fails with the given message if this is a
   * <code>Left</code>.
   */
  def rightE(err: => String) = this match {
    case Left(_) => error(err)
    case Right(b) => b
  }

  /**
   * Returns the value from this <code>Left</code> or fails if this is a <code>Right</code>
   * (undefined).
   */
  lazy val left = leftE("Either.left on Right")

  /**
   * Returns the value from this <code>Right</code> or fails if this is a <code>Left</code>
   * (undefined).
   */
  lazy val right = rightE("Either.right on Left")

  /**
   * If this is a <code>Left</code>, then return the left value in <code>Right</code> or vice versa.
   */
  lazy val unary_~ = this match {
    case Left(a) => Right(a)
    case Right(b) => Left(b)
  }

  /**
   * Returns <code>true</code> if this is a <code>Left</code>, <code>false</code> otherwise.
   */
  lazy val isLeft = this match {
    case Left(_) => true
    case Right(_) => false
  }

  /**
   * Returns <code>true</code> if this is a <code>Right</code>, <code>false</code> otherwise.
   */
  lazy val isRight = this match {
    case Left(_) => false
    case Right(_) => true
  }

  /**
   * Deconstruction of the <code>Either</code> type using continuation passing (in contrast to
   * pattern matching).
   */
  def either[X](ax: A => X, bx: B => X) = this match {
    case Left(a) => ax(a)
    case Right(b) => bx(b)
  }

  /**
   * Executes the given side-effect if this is a <code>Left</code>.
   *
   * @param e The side-effect to execute.
   */
  def ifLeft(f: A => Unit) = this match {
    case Left(a) => f(a)
    case Right(_) => {}
  }

  /**
   * Executes the given side-effect if this is a <code>Right</code>.
   *
   * @param e The side-effect to execute.
   */
  def ifRight(f: B => Unit) = this match {
    case Left(_) => {}
    case Right(b) => f(b)
  }

  /**
   * An alias for <code>ifRight</code>.
   *
   * <em>This function allows <code>Either</code> to be used in a for-comprehension.</em>
   */
  def foreach(f: B => Unit) = ifRight(f)

  /**
   * Returns the first argument if <code>Left</code>, otherwise returns the second argument.
   */
  def ?[X](left: => X, right: => X) = this match {
    case Left(_) => left
    case Right(_) => right
  }

  /**
   * Returns the value from this <code>Left</code> or the result of the given argument applied to
   * the value of this <code>Right</code> value.
   *
   * @param f The function that is applied to the <code>Right</code> value.
   */
  def left[AA >: A](f: B => AA) = this match {
    case Left(a) => a
    case Right(b) => f(b)
  }

  /**
   * Returns the value from this <code>Right</code> or the result of the given argument applied to
   * the value of this <code>Left</code> value.
   *
   * @param f The function that is applied to the <code>Left</code> value.
   */
  def right[BB >: B](f: A => BB) = this match {
    case Left(a) => f(a)
    case Right(b) => b
  }

  /**
   * Returns the value from this <code>Left</code> or the given argument if this is a
   * <code>Right</code>.
   */
  def leftOr[AA >: A](or: => AA) = this match {
    case Left(a) => a
    case Right(_) => or
  }

  /**
   * Returns the value from this <code>Right</code> or the given argument if this is a
   * <code>Left</code>.
   */
  def rightOr[BB >: B](or: => BB) = this match {
    case Left(_) => or
    case Right(b) => b
  }

  /**
   * Binds the given function across <code>Right</code>.
   *
   * @param The function to bind across <code>Right</code>.
   */
  def flatMap[AA >: A, Y](f: B => Either[AA, Y]) = this match {
    case Left(a) => Left(a)
    case Right(b) => f(b)
  }

  /**
   * An alias for <code>flatMap</code>.
   */
  def >>=[AA >: A, Y](f: B => Either[AA, Y]) = flatMap(f)

  /**
   * Binds the given constant across <code>Right</code>. This is the same as calling
   * <code>flatMap</code> where the argument to the given function is ignored.
   *
   * @param The constant to bind across <code>Right</code>.
   */
  def >>[AA >: A, Y](e: => Either[AA, Y]) = flatMap(b => e)

  /**
   * Binds the given function across <code>Left</code>.
   *
   * @param The function to bind across <code>Left</code>.
   */
  def flatMapLeft[BB >: B, X](f: A => Either[X, BB]) = this match {
    case Left(a) => f(a)
    case Right(b) => Right(b)
  }

  /**
   * An alias for <code>flatMapLeft</code>.
   */
  def <<=[BB >: B, X](f: A => Either[X, BB]) = flatMapLeft(f)

  /**
   * Binds the given constant across <code>Left</code>. This is the same as calling
   * <code>flatMapLeft</code> where the argument to the given function is ignored.
   *
   * @param The constant to bind across <code>Left</code>.
   */
  def <<[BB >: B, X](e: => Either[X, BB]) = flatMapLeft[BB, X](a => e)

  /**
   * Maps the first function argument through <code>Left</code> and the second function argument
   * through <code>Right</code>.
   */
  def <|>[X, Y](ax: A => X, by: B => Y) = this match {
    case Left(a) => Left(ax(a))
    case Right(b) => Right(by(b))
  }

  /**
   * Maps the function argument through <code>Right</code>.
   *
   * <em>This function allows <code>Either</code> to be used in a for-comprehension.</em>
   */
  def map[Y](f: B => Y): Either[A, Y] = this match {
    case Left(a) => Left(a)
    case Right(b) => Right(f(b))
  }

  /**
   * An alias for <code>map</code>.
   */
  def |>[Y](f: B => Y) = map(f)

  /**
   * Maps the function argument twice through <code>Right</code>.
   */
  def map2[AA >: A, C, Y](e: Either[AA, C], f: (B, C) => Y) =
    flatMap(b => e map (f(b, _)))

  /**
   * An alias for <code>map2</code>.
   */
  def ||>[AA >: A, C, Y](e: Either[AA, C], f: (B, C) => Y) = map2(e, f)

  /**
   * Maps the function argument three times through <code>Right</code>.
   */
  def map3[AA >: A, C, D, Y](ec: Either[AA, C], ed: Either[AA, D], f: (B, C, D) => Y) =
    flatMap(b => ec flatMap (c => ed map (f(b, c, _))))

  /**
   * An alias for <code>map3</code>.
   */
  def |||>[AA >: A, C, D, Y](ec: Either[AA, C], ed: Either[AA, D], f: (B, C, D) => Y) =
    map3(ec, ed, f)

  /**
   * Maps the function argument four times through <code>Right</code>.
   */
  def map4[AA >: A, C, D, E, Y](ec: Either[AA, C], ed: Either[AA, D], ee: Either[AA, E], f: (B, C, D, E) => Y) =
    flatMap(b => ec flatMap (c => ed flatMap (d => ee map (f(b, c, d, _)))))

  /**
   * An alias for <code>map4</code>.
   */
  def ||||>[AA >: A, C, D, E, Y](ec: Either[AA, C], ed: Either[AA, D], ee: Either[AA, E], f: (B, C, D, E) => Y) =
    map4(ec, ed, ee, f)

  /**
   * Maps the function argument five times through <code>Right</code>.
   */
  def map5[AA >: A, C, D, E, F, Y](ec: Either[AA, C], ed: Either[AA, D], ee: Either[AA, E], ef: Either[AA, F], f: (B, C, D, E, F) => Y) =
    flatMap(b => ec flatMap (c => ed flatMap (d => ee flatMap (e => ef map (f(b, c, d, e, _))))))

  /**
   * An alias for <code>map5</code>.
   */
  def |||||>[AA >: A, C, D, E, F, Y](ec: Either[AA, C], ed: Either[AA, D], ee: Either[AA, E], ef: Either[AA, F], f: (B, C, D, E, F) => Y) =
    map5(ec, ed, ee, ef, f)

  /**
   * Maps the function argument through <code>Left</code>.
   */
  def mapLeft[Y](f: A => Y) = this match {
    case Left(a) => Left(f(a))
    case Right(b) => Right(b)
  }

  /**
   * An alias for <code>mapLeft</code>.
   */
  def <|[X](f: A => X) = mapLeft(f)


  /**
   * Maps the function argument twice through <code>Left</code>.
   */
  def mapLeft2[BB >: B, C, X](e: Either[C, BB], f: (A, C) => X) =
    flatMapLeft(a => e mapLeft (f(a, _)))

  /**
   * An alias for <code>mapLeft2</code>.
   */
  def <||[BB >: B, C, X](e: Either[C, BB], f: (A, C) => X) =
    mapLeft2(e, f)

  /**
   * Maps the function argument three times through <code>Left</code>.
   */
  def mapLeft3[BB >: B, C, D, X](ec: Either[C, BB], ed: Either[D, BB], f: (A, C, D) => X) =
    flatMapLeft(a => ec flatMapLeft (c => ed mapLeft (f(a, c, _))))

  /**
   * An alias for <code>mapLeft3</code>.
   */
  def <|||[BB >: B, C, D, X](ec: Either[C, BB], ed: Either[D, BB], f: (A, C, D) => X) =
    mapLeft3(ec, ed, f)

  /**
   * Maps the function argument four times through <code>Left</code>.
   */
  def mapLeft4[BB >: B, C, D, E, X](ec: Either[C, BB], ed: Either[D, BB], ee: Either[E, BB], f: (A, C, D, E) => X) =
    flatMapLeft(a => ec flatMapLeft (c => ed flatMapLeft (d => ee mapLeft (f(a, c, d, _)))))

  /**
   * An alias for <code>mapLeft4</code>.
   */
  def <||||[BB >: B, C, D, E, X](ec: Either[C, BB], ed: Either[D, BB], ee: Either[E, BB], f: (A, C, D, E) => X) =
    mapLeft4(ec, ed, ee, f)

  /**
   * Maps the function argument five times through <code>Left</code>.
   */
  def mapLeft5[BB >: B, C, D, E, F, X](ec: Either[C, BB], ed: Either[D, BB], ee: Either[E, BB], ef: Either[F, BB], f: (A, C, D, E, F) => X) =
    flatMapLeft(a => ec flatMapLeft (c => ed flatMapLeft (d => ee flatMapLeft (e => ef mapLeft (f(a, c, d, e, _))))))

  /**
   * An alias for <code>mapLeft5</code>.
   */
  def <|||||[BB >: B, C, D, E, F, X](ec: Either[C, BB], ed: Either[D, BB], ee: Either[E, BB], ef: Either[F, BB], f: (A, C, D, E, F) => X) =
    mapLeft5(ec, ed, ee, ef, f)

  /**
   * Returns <code>None</code> is this is a <code>Left</code> or if the given predicate
   * <code>p</code> does not hold for the right value, otherwise, returns a <code>Right</code>
   * (identity).
   *
   * <em>This function allows <code>Either</code> to be used in a for-comprehension.</em>
   */
  def filter[X](p: B => Boolean): Option[Either[X, B]] = this match {
    case Left(_) => None
    case Right(b) => if(p(b)) Some(Right(b)) else None
  }

  /**
   * Filters through a <code>Right</code> value. If this is a <code>Left</code> or the given
   * predicate <code>p</code> does not hold on the right value, then return the given argument
   * <code>left</code> in <code>Left</code>, otherwise, return <code>Right</code> (identity).
   */
  def filter[X](p: B => Boolean, left: => X): Either[X, B] = this match {
    case Left(_) => Left(left)
    case Right(b) => if(p(b)) Right(b) else Left(left)
  }

  /**
   * Filters through a <code>Left</code> value. If this is a <code>Right</code> or the given
   * predicate <code>p</code> does not hold on the left value, then return the given argument
   * <code>right</code> in <code>Right</code>, otherwise, return <code>Left</code> (identity).
   */
  def filterLeft[Y](p: A => Boolean, right: => Y) = this match {
    case Left(a) => if(p(a)) Left(a) else Right(right)
    case Right(_) => Right(right)
  }

  /**
   * If this is a <code>Left</code> then return <code>Left</code> (identity), otherwise if the
   * given predicate <code>p</code> holds, then return a <code>Left</code> with the result of
   * applying the given function <code>f</code>, otherwise return a <code>Right</code> (identity).
   *
   * @param p The predicate to test if <code>Right</code>.
   * @param f The function to apply if the predicate holds.
   */
  def mapIf[AA >: A](p: B => Boolean, f: B => AA) =
    flatMap(b => if (p(b)) Left(f(b)) else Right(b))

  /**
   * If this is a <code>Right</code> then return <code>Right</code> (identity), otherwise if the
   * given predicate <code>p</code> holds, then return a <code>Right</code> with the result of
   * applying the given function <code>f</code>, otherwise return a <code>Left</code> (identity).
   *
   * @param p The predicate to test if <code>Left</code>.
   * @param f The function to apply if the predicate holds.
   */
  def mapIfLeft[BB >: B](p: A => Boolean, f: A => BB) =
    flatMapLeft(a => if (p(a)) Right(f(a)) else Left(a))

  /**
   * Function application on <code>Right</code>.
   *
   * @param e The Either of the function to apply on the right.
   */
  def ap[AA >: A, Y](e: Either[AA, B => Y]) =
    e.flatMap(f => flatMap(b => Right(f(b))))

  /**
   * Function application on <code>Left</code>.
   *
   * @param e The Either of the function to apply on the left.
   */
  def apLeft[BB >: B, X](e: Either[A => X, BB]) =
    e.flatMapLeft(f => flatMapLeft(a => Left(f(a))))

  /**
   * Returns a <code>Seq</code> containing the <code>Right</code> value if it exists or an empty
   * <code>Seq</code> if this is a <code>Left</code>.
   */
  lazy val seq = this match {
    case Left(_) => Seq.empty
    case Right(b) => Seq.singleton(b)
  }

  /**
   * Returns a <code>Seq</code> containing the <code>Left</code> value if it exists or an empty
   * <code>Seq</code> if this is a <code>Right</code>.
   */
  lazy val seqLeft = this match {
    case Left(a) => Seq.singleton(a)
    case Right(_) => Seq.empty
  }

  /**
   * Returns a <code>Some</code> containing the <code>Right</code> value if it exists or a
   * <code>None</code> if this is a <code>Left</code>.
   */
  lazy val option = this match {
    case Left(_) => None
    case Right(b) => Some(b)
  }

  /**
   * Returns a <code>Some</code> containing the <code>Left</code> value if it exists or a
   * <code>None</code> if this is a <code>Right</code>.
   */
  lazy val optionLeft =  this match {
    case Left(a) => Some(a)
    case Right(_) => None
  }
}

import Function.untupled

object Either {
  /**
   * Constructs a <code>Left</code> value with an upcast to <code>Either</code>.
   */
  def left[A, B](a: A): Either[A, B] = Left(a)

  /**
   * Constructs a <code>Right</code> value with an upcast to <code>Either</code>.
   */
  def right[A, B](b: B): Either[A, B] = Right(b)

  /**
   * Joins an <code>Either</code> through <code>Right</code>.
   */
  def join[A, B](es: Either[A, Either[A, B]]) =
    es flatMap(x => x)

  /**
   * Joins an <code>Either</code> through <code>Left</code>.
   */
  def joinLeft[A, B](es: Either[Either[A, B], B]) =
    es flatMapLeft(x => x)

  /**
   * Returns the <code>Left</code> values in the given <code>Either</code>s.
   */
  def lefts[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[List[A]](Nil)((e, as) => e match {
      case Left(a) => a :: as
      case Right(_) => as
    })

  /**
   * Returns the <code>Right</code> values in the given <code>Either</code>s.
   */
  def rights[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[List[B]](Nil)((e, bs) => e match {
      case Left(_) => bs
      case Right(b) => b :: bs
    })

  /**
   * Returns the <code>Left</code> and <code>Right</code> values in the given <code>Either</code>s.
   */
  def leftsRights[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[(List[A], List[B])]((Nil, Nil))(untupled(_ match {
      case (Left(a), (lefts, rights)) => (a :: lefts, rights)
      case (Right(b), (lefts, rights)) => (lefts, b :: rights)
    }))

  /**
   * An alternative way to handle an exception that may be thrown in the evaluation of the given
   * argument.
   */
  def throws[A](a: => A) =
    try {
      Right(a)
    } catch {
      case e => Left(e)
    }

  /**
   * Throw the exception if left, otherwise, the right value.
   */
  def throwIt[A](e: Either[Throwable, A]) = e match {
    case Left(t) => throw t
    case Right(a) => a
  }

  /**
   * Takes an <code>Either</code> to its contained value within <code>Left</code> or
   * <code>Right</code>.
   */
  def reduce[A](e: Either[A, A]) = e match {
    case Left(a) => a
    case Right(a) => a
  }

  /**
   * If the condition satisfies, return the given A in <code>Left</code>, otherwise, return the
   * given B in <code>Right</code>.
   */
  def iif[A, B](cond: Boolean)(left: => A, right: => B) =
    if(cond) Right(right) else Left(left)
}

/**
 * The left side of the disjoint union, as opposed to the <code>Right</code> side.
 */
final case class Left[+A, +B](a: A) extends Either[A, B]

/**
 * The right side of the disjoint union, as opposed to the <code>Left</code> side.
 */
final case class Right[+A, +B](b: B) extends Either[A, B]
