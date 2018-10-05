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

/** A template for companion objects of `Map` and subclasses thereof.
 *
 *  @define coll map
 *  @define Coll Map
 *  @define factoryInfo
 *    This object provides a set of operations needed to create `$Coll` values.
 *    @author Martin Odersky
 *    @since 2.8
 *  @define canBuildFromInfo
 *    The standard `CanBuildFrom` instance for `$Coll` objects.
 *    @see CanBuildFrom
 *  @define mapCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for `$Coll` objects.
 *    The created value is an instance of class `MapCanBuildFrom`.
 *    @see CanBuildFrom
 *    @see GenericCanBuildFrom
 */
abstract class MapFactory[CC[A, B] <: Map[A, B] with MapLike[A, B, CC[A, B]]] extends GenMapFactory[CC] {

  def empty[A, B]: CC[A, B]

}
