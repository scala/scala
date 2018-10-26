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

package scala.reflect.internal
package util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.reflect.NameTransformer

class FreshNameCreator(creatorPrefix: String = "") {
  protected val counters = new ConcurrentHashMap[String, AtomicLong]()

  /**
   * Create a fresh name with the given prefix. It is guaranteed
   * that the returned name has never been returned by a previous
   * call to this function (provided the prefix does not end in a digit).
   */
  def newName(prefix: String): String = {
    val safePrefix = NameTransformer.encode(prefix)
    val counter = counters.computeIfAbsent(safePrefix, (s: String) => new AtomicLong(0))
    val idx = counter.incrementAndGet()
    creatorPrefix + safePrefix + idx
  }

}
