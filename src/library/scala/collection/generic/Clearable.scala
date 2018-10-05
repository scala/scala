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

/** This trait forms part of collections that can be cleared
 *  with a clear() call.
 *
 *  @author Paul Phillips
 *  @since  2.10
 *  @define coll clearable collection
 *  @define Coll `Clearable`
 */
trait Clearable {
  /** Clears the $coll's contents. After this operation, the
   *  $coll is empty.
   */
  def clear(): Unit
}
