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
import scala.reflect.ClassTag

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
abstract class ClassTagTraversableFactory[CC[X] <: Traversable[X] with GenericClassTagTraversableTemplate[X, CC]]
              extends GenericClassTagCompanion[CC] {

  class GenericCanBuildFrom[A](implicit tag: ClassTag[A]) extends CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = from.genericClassTagBuilder[A]
    def apply = newBuilder[A]
  }
}
