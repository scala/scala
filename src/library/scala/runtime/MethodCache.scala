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
package runtime


import java.lang.reflect.{ Method => JMethod }
import java.lang.{ Class => JClass }

import scala.annotation.tailrec

/** An element of a polymorphic object cache.
 *  This class is referred to by the `CleanUp` phase. Each `PolyMethodCache` chain
 *  must only relate to one method as `PolyMethodCache` does not identify
 *  the method name and argument types. In practice, one variable will be
 *  generated per call point, and will uniquely relate to the method called
 *  at that point, making the method name and argument types irrelevant. */
/* TODO: if performance is acceptable, PolyMethodCache should be made generic on the method type */
private[scala] sealed abstract class MethodCache {
  /** Searches for a cached method in the `MethodCache` chain that
   *  is compatible with receiver class `forReceiver`. If none is cached,
   *  `null` is returned. If `null` is returned, find's caller should look-
   *  up the right method using whichever means it prefers, and add it to
   *  the cache for later use. */
  def find(forReceiver: JClass[_]): JMethod
  def add(forReceiver: JClass[_], forMethod: JMethod): MethodCache
}

private[scala] final class EmptyMethodCache extends MethodCache {

  def find(forReceiver: JClass[_]): JMethod = null

  def add(forReceiver: JClass[_], forMethod: JMethod): MethodCache =
    new PolyMethodCache(this, forReceiver, forMethod, 1)

}

private[scala] final class MegaMethodCache(
  private[this] val forName: String,
  private[this] val forParameterTypes: Array[JClass[_]]
) extends MethodCache {

  def find(forReceiver: JClass[_]): JMethod =
    forReceiver.getMethod(forName, forParameterTypes:_*)

  def add(forReceiver: JClass[_], forMethod: JMethod): MethodCache = this

}

private[scala] final class PolyMethodCache(
  private[this] val next: MethodCache,
  private[this] val receiver: JClass[_],
  private[this] val method: JMethod,
  private[this] val complexity: Int
) extends MethodCache {

  /** To achieve tail recursion this must be a separate method
   *  from `find`, because the type of next is not `PolyMethodCache`.
   */
  @tailrec private def findInternal(forReceiver: JClass[_]): JMethod =
    if (forReceiver eq receiver) method
    else next match {
      case x: PolyMethodCache => x findInternal forReceiver
      case _                  => next find forReceiver
    }

  def find(forReceiver: JClass[_]): JMethod = findInternal(forReceiver)

  // TODO: come up with a more realistic number
  final private val MaxComplexity = 160

  def add(forReceiver: JClass[_], forMethod: JMethod): MethodCache =
    if (complexity < MaxComplexity)
      new PolyMethodCache(this, forReceiver, forMethod, complexity + 1)
    else
      new MegaMethodCache(forMethod.getName, forMethod.getParameterTypes)
}
