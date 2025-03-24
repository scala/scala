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
package api

/** A mirror-aware factory for trees.
 *
 * This class is used internally by Scala Reflection, and is not recommended for use in client code.
 *
 * @group ReflectionAPI
 */
abstract class TreeCreator extends Serializable {
  def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Tree
}
