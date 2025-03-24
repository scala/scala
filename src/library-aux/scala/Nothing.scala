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

/** `Nothing` is - together with [[scala.Null]] - at the bottom of Scala's type hierarchy.
 *
 *  `Nothing` is a subtype of every other type (including [[scala.Null]]); there exist
 *  ''no instances'' of this type.  Although type `Nothing` is uninhabited, it is
 *  nevertheless useful in several ways.  For instance, the Scala library defines a value
 *  [[scala.collection.immutable.Nil]] of type `List[Nothing]`. Because lists are covariant in Scala,
 *  this makes [[scala.collection.immutable.Nil]] an instance of `List[T]`, for any element of type `T`.
 *
 *  Another usage for Nothing is the return type for methods which never return normally.
 *  One example is method error in [[scala.sys]], which always throws an exception.
 */
sealed trait Nothing

