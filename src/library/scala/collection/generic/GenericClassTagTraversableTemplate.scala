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

import mutable.Builder
import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds
import scala.reflect.ClassTag

/** This trait represents collections classes which require class
 *  tags for their element types.
 *
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
trait GenericClassTagTraversableTemplate[+A, +CC[X] <: Traversable[X]] extends HasNewBuilder[A, CC[A] @uncheckedVariance] {
  implicit protected[this] val tag: ClassTag[A]
  def classTagCompanion: GenericClassTagCompanion[CC]
  def genericClassTagBuilder[B](implicit tag: ClassTag[B]): Builder[B, CC[B]] = classTagCompanion.newBuilder[B]
  @deprecated("use classTagCompanion instead", "2.10.0")
  def classManifestCompanion: GenericClassManifestCompanion[CC] = classTagCompanion
  @deprecated("use genericClassTagBuilder instead", "2.10.0")
  def genericClassManifestBuilder[B](implicit manifest: ClassManifest[B]): Builder[B, CC[B]] = genericClassTagBuilder[B](manifest)
}
