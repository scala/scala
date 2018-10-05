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
import scala.language.higherKinds

/** A template for companion objects of `mutable.Map` and subclasses thereof.
 *    @author Martin Odersky
 *    @since 2.8
 */
abstract class MutableMapFactory[CC[A, B] <: mutable.Map[A, B] with mutable.MapLike[A, B, CC[A, B]]]
  extends MapFactory[CC] {

  /** The default builder for $Coll objects.
   *  @tparam A      the type of the keys
   *  @tparam B      the type of the associated values
   */
  override def newBuilder[A, B]: Builder[(A, B), CC[A, B]] = empty[A, B]
}
