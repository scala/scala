package scala
package reflect
package runtime

import scala.collection.mutable.WeakHashMap
import java.lang.ref.WeakReference
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
  private val uniques = WeakHashMap[Type, WeakReference[Type]]()
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
        uniques(tp) = new WeakReference(tp)
        tp
      }
    } else {
      super.unique(tp)
    }
  }

  class SynchronizedUndoLog extends UndoLog {
    final override def lock(): Unit = gil.lock()
    final override def unlock(): Unit = gil.unlock()
  }

  override protected def newUndoLog = new SynchronizedUndoLog

  override protected def baseTypeOfNonClassTypeRef(tpe: NonClassTypeRef, clazz: Symbol) =
    gilSynchronized { super.baseTypeOfNonClassTypeRef(tpe, clazz) }

  override def isSameType(tp1: Type, tp2: Type): Boolean =
    gilSynchronized { super.isSameType(tp1, tp2) }

  override def isDifferentType(tp1: Type, tp2: Type): Boolean =
    gilSynchronized { super.isDifferentType(tp1, tp2) }

  override def isSubType(tp1: Type, tp2: Type, depth: Depth): Boolean =
    gilSynchronized { super.isSubType(tp1, tp2, depth) }

  override def glb(ts: List[Type]): Type =
    gilSynchronized { super.glb(ts) }

  override def lub(ts: List[Type]): Type =
    gilSynchronized { super.lub(ts) }

  override protected def explain[T](op: String, p: (Type, T) => Boolean, tp1: Type, arg2: T): Boolean =
    gilSynchronized { super.explain(op, p, tp1, arg2) }

  override protected def typeToString(tpe: Type): String =
    gilSynchronized(super.typeToString(tpe))

  /* The idea of caches is as follows.
   * When in reflexive mode, a cache is either null, or one sentinal
   * value representing undefined or the final defined
   * value. Hence, we can ask in non-synchronized ode whether the cache field
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

}
