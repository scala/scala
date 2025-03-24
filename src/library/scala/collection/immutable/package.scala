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

package scala.collection


package object immutable {
  type StringOps = scala.collection.StringOps
  val StringOps = scala.collection.StringOps
  type StringView = scala.collection.StringView
  val StringView = scala.collection.StringView

  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[+X] = Iterable[X]
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable

  @deprecated("Use Map instead of DefaultMap", "2.13.0")
  type DefaultMap[K, +V] = scala.collection.immutable.Map[K, V]
}
