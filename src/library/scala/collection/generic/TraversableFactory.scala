/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package generic

import annotation.bridge

/** A template for companion objects of `Traversable` and subclasses thereof.
 *  This class provides a set of operations to create `$Coll` objects.
 *  It is typically inherited by companion objects of subclasses of `Traversable`.
 *
 *  @since 2.8
 *
 *  @define coll collection
 *  @define Coll Traversable
 *  @define factoryInfo
 *    This object provides a set of operations to create `$Coll` values.
 *    @author Martin Odersky
 *    @version 2.8
 *  @define canBuildFromInfo
 *    The standard `CanBuildFrom` instance for $Coll objects.
 *    @see CanBuildFrom
 *  @define genericCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for $Coll objects.
 *    The created value is an instance of class `GenericCanBuildFrom`,
 *    which forwards calls to create a new builder to the
 *    `genericBuilder` method of the requesting collection.
 *    @see CanBuildFrom
 *    @see GenericCanBuildFrom
 */
trait TraversableFactory[CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC]]
  extends GenTraversableFactory[CC] with GenericSeqCompanion[CC] {

  @bridge
  override def concat[A](xss: Traversable[A]*): CC[A] = super.concat(xss: _*)

  @bridge
  override def fill[A](n: Int)(elem: => A): CC[A] = super.fill(n)(elem)

  @bridge
  override def fill[A](n1: Int, n2: Int)(elem: => A): CC[CC[A]] = super.fill(n1, n2)(elem)

  @bridge
  override def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]]] = super.fill(n1, n2, n3)(elem)

  @bridge
  override def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]]] = super.fill(n1, n2, n3, n4)(elem)

  @bridge
  override def fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]]] = super.fill(n1, n2, n3, n4, n5)(elem)

  @bridge
  override def tabulate[A](n: Int)(f: Int => A): CC[A] = super.tabulate(n)(f)

  @bridge
  override def tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A]] = super.tabulate(n1, n2)(f)

  @bridge
  override def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]]] = super.tabulate(n1, n2, n3)(f)

  @bridge
  override def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]]] = super.tabulate(n1, n2, n3, n4)(f)

  @bridge
  override def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]]] = super.tabulate(n1, n2, n3, n4, n5)(f)

  @bridge
  override def range[T: Integral](start: T, end: T): CC[T] = super.range(start, end)

  @bridge
  override def range[T: Integral](start: T, end: T, step: T): CC[T] = super.range(start, end, step)

  @bridge
  override def iterate[A](start: A, len: Int)(f: A => A): CC[A] = super.iterate(start, len)(f)
}

