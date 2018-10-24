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

