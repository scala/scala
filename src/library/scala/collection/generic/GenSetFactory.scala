/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package generic

import mutable.Builder
import scala.language.higherKinds

/** A template for companion objects of `Set` and subclasses thereof.
 *
 *  @define coll set
 *  @define Coll `Set`
 *  @define factoryInfo
 *    This object provides a set of operations needed to create `$Coll` values.
 *    @author Martin Odersky
 *    @version 2.8
 *    @since 2.8
 *  @define canBuildFromInfo
 *    The standard `CanBuildFrom` instance for `$Coll` objects.
 *    @see CanBuildFrom
 *  @define setCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for `$Coll` objects.
 *    @see CanBuildFrom
 *    @see GenericCanBuildFrom
 */
abstract class GenSetFactory[CC[X] <: GenSet[X] with GenSetLike[X, CC[X]]]
  extends GenericCompanion[CC] {

  def newBuilder[A]: Builder[A, CC[A]]

  /** $setCanBuildFromInfo
   */
  def setCanBuildFrom[A] = new CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = from match {
      // When building from an existing Set, try to preserve its type:
      case from: Set[_] => from.genericBuilder.asInstanceOf[Builder[A, CC[A]]]
      case _ => newBuilder[A]
    }
    def apply() = newBuilder[A]
  }
}
