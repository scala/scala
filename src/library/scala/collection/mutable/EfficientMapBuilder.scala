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
package scala.collection.mutable

private[collection] trait EfficientMapBuilder[K, V] extends ((K, V, (K, V)) => Unit) {
  @inline final def apply(k: K, v: V, kvOrNull: (K, V)): Unit = addOne(k, v, kvOrNull)
  private[collection] def addOne(k: K, v: V, kvOrNull: (K, V)): Unit
}