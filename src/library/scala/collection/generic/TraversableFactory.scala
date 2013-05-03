/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package generic

import scala.language.higherKinds

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
  extends GenTraversableFactory[CC] with GenericSeqCompanion[CC]

