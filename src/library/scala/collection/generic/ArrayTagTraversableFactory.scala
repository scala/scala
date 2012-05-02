/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

import language.higherKinds

/** A template for companion objects of `ClassTagTraversable` and
 *  subclasses thereof.
 *
 *  @define coll collection
 *  @define Coll `Traversable`
 *  @define genericCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for $Coll objects.
 *    @author Aleksandar Prokopec
 *    @since 2.8
 */
abstract class ArrayTagTraversableFactory[CC[X] <: Traversable[X] with GenericArrayTagTraversableTemplate[X, CC]]
              extends GenericArrayTagCompanion[CC] {

  class GenericCanBuildFrom[A](implicit tag: ArrayTag[A]) extends CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = from.genericArrayTagBuilder[A]
    def apply = newBuilder[A]
  }
}
