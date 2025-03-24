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

package scala.reflect
package runtime

private[reflect] trait Gil {
  self: SymbolTable =>

  // fixme... please...
  // there are the following avenues of optimization we discussed with Roland:
  // 1) replace PackageScope locks with ConcurrentHashMap, because PackageScope materializers seem to be idempotent
  // 2) unlock unpickling completers by verifying that they are idempotent or moving non-idempotent parts
  // 3) remove the necessity in global state for isSubType
  private lazy val gil = new java.util.concurrent.locks.ReentrantLock

  @inline final def gilSynchronized[T](body: => T): T = {
    if (isCompilerUniverse) body
    else {
      try {
        gil.lock()
        body
      } finally {
        gil.unlock()
      }
    }
  }
}
