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

/** `Null` is - together with [[scala.Nothing]] - at the bottom of the Scala type hierarchy.
  *
  * `Null` is the type of the `null` literal. It is a subtype of every type
  * except those of value classes. Value classes are subclasses of [[AnyVal]], which includes
  * primitive types such as [[Int]], [[Boolean]], and user-defined value classes.
  *
  * Since `Null` is not a subtype of value types, `null` is not a member of any such type.
  * For instance, it is not possible to assign `null` to a variable of type [[scala.Int]].
  */
sealed trait Null
