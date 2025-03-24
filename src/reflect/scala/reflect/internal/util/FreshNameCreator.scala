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
    val idx = allocateCounter(safePrefix).incrementAndGet()
    assemble(safePrefix, idx)
  }

  /** Low level API for clients that want to perform multiple fresh names from the same prefix. */
  def newNameFactory(prefix: String): NameFactory = {
    val safePrefix = NameTransformer.encode(prefix)
    val counter = allocateCounter(safePrefix)
    new NameFactory(safePrefix, counter)
  }

  private def assemble(safePrefix: String, idx: Long) = {
    val result = new java.lang.StringBuilder(creatorPrefix.length + safePrefix.length + decimalLength(idx))
    result.append(creatorPrefix)
    result.append(safePrefix)
    result.append(idx)
    creatorPrefix + safePrefix + idx
  }

  private def allocateCounter(safePrefix: String): AtomicLong = {
    counters.computeIfAbsent(safePrefix, (s: String) => new AtomicLong(0))
  }

  final class NameFactory(safePrefix: String, counter: AtomicLong) {
    def index(): Long = counter.incrementAndGet()
    def newNameAtIndex(index: Long): String = assemble(safePrefix, index)
    def newName(): String = newNameAtIndex(index())
  }

  def decimalLength(i: Long): Int = {
    require(i >= 0, i)
    var ceiling = 10
    var numDigits = 1
    val MaxValueLength = 19 // = Long.MaxValue.toString.length
    while (numDigits <= MaxValueLength) {
      if (i < ceiling) return numDigits
      numDigits += 1
      ceiling *= 10
    }
    MaxValueLength
  }
}
