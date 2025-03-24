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
package reflect
package runtime

import scala.collection.mutable
import java.lang.ref.{WeakReference => jWeakRef}
import scala.ref.{WeakReference => sWeakRef}
import scala.reflect.internal.Depth

/** This trait overrides methods in reflect.internal, bracketing
 *  them in synchronized { ... } to make them thread-safe
 */
private[reflect] trait SynchronizedTypes extends internal.Types { self: SymbolTable =>

  // No sharing of map objects:
  override protected def commonOwnerMap = new CommonOwnerMap

  // we can keep this lock fine-grained, because super.unique just updates the cache
  // and, in particular, doesn't call any reflection APIs which makes deadlocks impossible
  private lazy val uniqueLock = new Object
  private[this] val uniques = mutable.WeakHashMap[Type, jWeakRef[Type]]()
  override def unique[T <: Type](tp: T): T = uniqueLock.synchronized {
    // we need to have weak uniques for runtime reflection
    // because unlike the normal compiler universe, reflective universe isn't organized in runs
    // therefore perRunCaches can grow infinitely large
    //
    // despite that toolbox universes are decorated, toolboxes are compilers,
    // i.e. they have their caches cleaned up automatically on per-run basis,
    // therefore they should use vanilla uniques, which are faster
    if (!isCompilerUniverse) {
      val inCache = uniques get tp
      val result = if (inCache.isDefined) inCache.get.get else null
      if (result ne null) result.asInstanceOf[T]
      else {
        uniques(tp) = new jWeakRef(tp)
        tp
      }
    } else {
      super.unique(tp)
    }
  }

  private lazy val _skolemizationLevel = mkThreadLocalStorage(0)
  override def skolemizationLevel = _skolemizationLevel.get
  override def skolemizationLevel_=(value: Int) = _skolemizationLevel.set(value)

  private lazy val _undoLog = mkThreadLocalStorage(new UndoLog)
  override def undoLog = _undoLog.get

  private lazy val _intersectionWitness = mkThreadLocalStorage(perRunCaches.newWeakMap[List[Type], sWeakRef[Type]]())
  override def intersectionWitness = _intersectionWitness.get

  private lazy val _subsametypeRecursions = mkThreadLocalStorage(0)
  override def subsametypeRecursions = _subsametypeRecursions.get
  override def subsametypeRecursions_=(value: Int) = _subsametypeRecursions.set(value)

  private lazy val _pendingSubTypes = mkThreadLocalStorage(new mutable.HashSet[SubTypePair])
  override def pendingSubTypes = _pendingSubTypes.get

  private lazy val _basetypeRecursions = mkThreadLocalStorage(0)
  override def basetypeRecursions = _basetypeRecursions.get
  override def basetypeRecursions_=(value: Int) = _basetypeRecursions.set(value)

  private lazy val _pendingBaseTypes = mkThreadLocalStorage(new mutable.HashSet[Type])
  override def pendingBaseTypes = _pendingBaseTypes.get

  private lazy val _lubResults = mkThreadLocalStorage(new mutable.HashMap[(Depth, List[Type]), Type])
  override def lubResults = _lubResults.get

  private lazy val _glbResults = mkThreadLocalStorage(new mutable.HashMap[(Depth, List[Type]), Type])
  override def glbResults = _glbResults.get

  private lazy val _indent = mkThreadLocalStorage("")
  override def indent = _indent.get
  override def indent_=(value: String) = _indent.set(value)

  private lazy val _toStringRecursions = mkThreadLocalStorage(0)
  override def toStringRecursions = _toStringRecursions.get
  override def toStringRecursions_=(value: Int) = _toStringRecursions.set(value)

  private lazy val _toStringSubjects = mkThreadLocalStorage(new mutable.HashSet[Type])
  override def toStringSubjects = _toStringSubjects.get

  /* The idea of caches is as follows.
   * When in reflexive mode, a cache is either null, or one sentinel
   * value representing undefined or the final defined
   * value. Hence, we can ask in non-synchronized mode whether the cache field
   * is non null and different from the sentinel (if a sentinel exists).
   * If that's true, the cache value is current.
   * Otherwise we arrive in one of the defined... methods listed below
   * which go through all steps in synchronized mode.
   */

  override protected def defineUnderlyingOfSingleType(tpe: SingleType) =
    gilSynchronized { super.defineUnderlyingOfSingleType(tpe) }

  override protected def defineBaseTypeSeqOfCompoundType(tpe: CompoundType) =
    gilSynchronized { super.defineBaseTypeSeqOfCompoundType(tpe) }

  override protected def defineBaseClassesOfCompoundType(tpe: CompoundType) =
    gilSynchronized { super.defineBaseClassesOfCompoundType(tpe) }

  override protected def defineParentsOfTypeRef(tpe: TypeRef) =
    gilSynchronized { super.defineParentsOfTypeRef(tpe) }

  override protected def defineBaseTypeSeqOfTypeRef(tpe: TypeRef) =
    gilSynchronized { super.defineBaseTypeSeqOfTypeRef(tpe) }

  override protected def defineNormalized(tr: TypeRef): Unit = {
    gilSynchronized { super.defineNormalized(tr) }
  }
}
