/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect
package common

import scala.collection.{ mutable, immutable }

/** A cache for some entity whose validity depends on a monotonically
 *  increasing sequence number.
 */
abstract class SequencedCache[T >: Null] {
  def zero: Int
  def sequenceId: Int
  def calculate(): T

  /** If sequence numbers differ, this condition is consulted before
   *  updating the cached value.
   */
  def isCacheValid: Boolean

  /** Public so accesses can be inlined. */
  @inline var cachedId: Int = 0
  @inline var cachedValue: T = _

  /** Puts cache back in uninitialized state. */
  @inline final def clear() = {
    cachedId = zero
    cachedValue = null
  }
  /** Resets the sequence id without touching the cached value. */
  @inline final def reset() = {
    cachedId = zero
  }

  final def get(): T = {
    if (cachedValue == null) {
      cachedValue = calculate()
      cachedId = sequenceId
    }
    else if (cachedId != sequenceId) {
      if (!isCacheValid)
        cachedValue = calculate()

      cachedId = sequenceId
    }
    cachedValue
  }
}

trait Caches {
  self: SymbolTable =>

  final def isValid(period: Period): Boolean =
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) infoTransformers.nextFrom(pid).pid >= phase.id
      else infoTransformers.nextFrom(phase.id).pid >= pid
    }

  final def isValidForBaseClasses(period: Period): Boolean = {
    def noChangeInBaseClasses(it: InfoTransformer, limit: Phase#Id): Boolean = (
      it.pid >= limit ||
      !it.changesBaseClasses && noChangeInBaseClasses(it.next, limit)
    );
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) noChangeInBaseClasses(infoTransformers.nextFrom(pid), phase.id)
      else noChangeInBaseClasses(infoTransformers.nextFrom(phase.id), pid)
    }
  }

  abstract class PeriodCache[T >: Null] extends SequencedCache[T] {
    final val zero = NoPeriod
    @inline final def sequenceId = currentPeriod
  }

  abstract class ListOfTypesCache extends PeriodCache[List[Type]] {
    @inline final def isCacheValid = isValidForBaseClasses(cachedId)
  }
  abstract class ListOfSymbolsCache extends PeriodCache[List[Symbol]] {
    @inline final def isCacheValid = isValidForBaseClasses(cachedId)
  }
  abstract class BaseTypeSeqCache extends PeriodCache[BaseTypeSeq] {
    @inline final def isCacheValid = isValidForBaseClasses(cachedId)
  }
  abstract class TypeCache extends PeriodCache[Type] {
    @inline final def isCacheValid = isValid(cachedId)
  }
  abstract class TypeCacheForRunId extends SequencedCache[Type] {
    final val zero = NoRunId
    @inline final def sequenceId = currentRunId
    @inline final override def isCacheValid = false
  }
  object TypeCache {
    def apply(body: => Type): TypeCache = new TypeCache {
      @inline final def calculate() = body
    }
  }
}

