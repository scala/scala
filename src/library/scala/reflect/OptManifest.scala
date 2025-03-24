/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect

/** A `OptManifest[T]` is an optional [[scala.reflect.Manifest]].
 *
 *  It is either a `Manifest` or the value `NoManifest`.
 */
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("This notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
trait OptManifest[+T] extends Serializable
