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

/** `Null` is - together with [[scala.Nothing]] - at the bottom of the Scala type hierarchy.
 *
 *  `Null` is a subtype of all reference types; its only instance is the `null` reference.
 *  Since `Null` is not a subtype of value types, `null` is not a member of any such type.  For instance,
 *  it is not possible to assign `null` to a variable of type [[scala.Int]].
 */
sealed trait Null
