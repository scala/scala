/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
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
