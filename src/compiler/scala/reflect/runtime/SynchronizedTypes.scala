package scala.reflect
package runtime

/** This trait overrides methods in reflect.internal, bracketing
 *  them in synchronized { ... } to make them thread-safe
 */
trait SynchronizedTypes extends internal.Types { self: SymbolTable =>

  // No sharing of map objects:
  override protected def commonOwnerMap = new CommonOwnerMap

  private object uniqueLock

  override def unique[T <: Type](tp: T): T = uniqueLock.synchronized { super.unique(tp) }

  class SynchronizedUndoLog extends UndoLog {

    override def clear() =
      synchronized { super.clear() }

    override def undo[T](block: => T): T =
      synchronized { super.undo(block) }

    override def undoUnless(block: => Boolean): Boolean =
      synchronized { super.undoUnless(block) }
  }

  override protected def newUndoLog = new SynchronizedUndoLog

  override protected def baseTypeOfNonClassTypeRef(tpe: NonClassTypeRef, clazz: Symbol) =
    synchronized { super.baseTypeOfNonClassTypeRef(tpe, clazz) }

  private object subsametypeLock

  override def isSameType(tp1: Type, tp2: Type): Boolean =
    subsametypeLock.synchronized { super.isSameType(tp1, tp2) }

  override def isDifferentType(tp1: Type, tp2: Type): Boolean =
    subsametypeLock.synchronized { super.isDifferentType(tp1, tp2) }

  override def isSubType(tp1: Type, tp2: Type, depth: Int): Boolean =
    subsametypeLock.synchronized { super.isSubType(tp1, tp2, depth) }

  private object lubglbLock

  override def glb(ts: List[Type]): Type =
    lubglbLock.synchronized { super.glb(ts) }

  override def lub(ts: List[Type]): Type =
    lubglbLock.synchronized { super.lub(ts) }

  private object indentLock

  override protected def explain[T](op: String, p: (Type, T) => Boolean, tp1: Type, arg2: T): Boolean = {
    indentLock.synchronized { super.explain(op, p, tp1, arg2) }
  }

  private object toStringLock

  override protected def typeToString(tpe: Type): String =
    toStringLock.synchronized(super.typeToString(tpe))

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
    tpe.synchronized { super.defineUnderlyingOfSingleType(tpe) }

  override protected def defineBaseTypeSeqOfCompoundType(tpe: CompoundType) =
    tpe.synchronized { super.defineBaseTypeSeqOfCompoundType(tpe) }

  override protected def defineBaseClassesOfCompoundType(tpe: CompoundType) =
    tpe.synchronized { super.defineBaseClassesOfCompoundType(tpe) }

  override protected def defineParentsOfTypeRef(tpe: TypeRef) =
    tpe.synchronized { super.defineParentsOfTypeRef(tpe) }

  override protected def defineBaseTypeSeqOfTypeRef(tpe: TypeRef) =
    tpe.synchronized { super.defineBaseTypeSeqOfTypeRef(tpe) }

}
