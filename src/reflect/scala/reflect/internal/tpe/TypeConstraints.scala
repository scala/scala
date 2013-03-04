package scala.reflect
package internal
package tpe

import scala.collection.generic
import generic.Clearable
import Flags._


private[internal] trait TypeConstraints {
  self: SymbolTable =>
  import definitions._

  class AbsUndoLog extends Clearable {
    private type UndoPairs = List[(TypeVar, TypeConstraint)]
    //OPT this method is public so we can do `manual inlining`
    var log: UndoPairs = List()

    /*
     * These two methods provide explicit locking mechanism that is overridden in SynchronizedUndoLog.
     *
     * The idea behind explicit locking mechanism is that all public methods that access mutable state
     * will have to obtain the lock for their entire execution so both reads and writes can be kept in
     * right order. Originally, that was achieved by overriding those public methods in
     * `SynchronizedUndoLog` which was fine but expensive. The reason is that those public methods take
     * thunk as argument and if we keep them non-final there's no way to make them inlined so thunks
     * can go away.
     *
     * By using explicit locking we can achieve inlining.
     *
     * NOTE: They are made public for now so we can apply 'manual inlining' (copy&pasting into hot
     * places implementation of `undo` or `undoUnless`). This should be changed back to protected
     * once inliner is fixed.
     */
    def lock(): Unit = ()
    def unlock(): Unit = ()

    // register with the auto-clearing cache manager
    perRunCaches.recordCache(this)

    /** Undo all changes to constraints to type variables upto `limit`. */
    //OPT this method is public so we can do `manual inlining`
    def undoTo(limit: UndoPairs) {
      assertCorrectThread()
      while ((log ne limit) && log.nonEmpty) {
        val (tv, constr) = log.head
        tv.constr = constr
        log = log.tail
      }
    }

    /** No sync necessary, because record should only
     *  be called from within a undo or undoUnless block,
     *  which is already synchronized.
     */
    private[reflect] def record(tv: TypeVar) = {
      log ::= ((tv, tv.constr.cloneInternal))
    }

    def clear() {
      lock()
      try {
        if (settings.debug.value)
          self.log("Clearing " + log.size + " entries from the undoLog.")
        log = Nil
      } finally unlock()
    }
    def size = {
      lock()
      try log.size finally unlock()
    }

    // `block` should not affect constraints on typevars
    def undo[T](block: => T): T = {
      lock()
      try {
        val before = log

        try block
        finally undoTo(before)
      } finally unlock()
    }

    // if `block` evaluates to false, it should not affect constraints on typevars
    def undoUnless(block: => Boolean): Boolean = {
      lock()
      try {
        val before = log
        var result = false

        try result = block
        finally if (!result) undoTo(before)

        result
      } finally unlock()
    }
  }

  /** Solve constraint collected in types `tvars`.
   *
   *  @param tvars      All type variables to be instantiated.
   *  @param tparams    The type parameters corresponding to `tvars`
   *  @param variances  The variances of type parameters; need to reverse
   *                    solution direction for all contravariant variables.
   *  @param upper      When `true` search for max solution else min.
   */
  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Int], upper: Boolean): Boolean =
     solve(tvars, tparams, variances, upper, AnyDepth)

  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Int], upper: Boolean, depth: Int): Boolean = {

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Int) {
      if (tvar.constr.inst == NoType) {
        val up = if (variance != CONTRAVARIANT) upper else !upper
        tvar.constr.inst = null
        val bound: Type = if (up) tparam.info.bounds.hi else tparam.info.bounds.lo
        //Console.println("solveOne0(tv, tp, v, b)="+(tvar, tparam, variance, bound))
        var cyclic = bound contains tparam
        foreach3(tvars, tparams, variances)((tvar2, tparam2, variance2) => {
          val ok = (tparam2 != tparam) && (
               (bound contains tparam2)
            ||  up && (tparam2.info.bounds.lo =:= tparam.tpeHK)
            || !up && (tparam2.info.bounds.hi =:= tparam.tpeHK)
          )
          if (ok) {
            if (tvar2.constr.inst eq null) cyclic = true
            solveOne(tvar2, tparam2, variance2)
          }
        })
        if (!cyclic) {
          if (up) {
            if (bound.typeSymbol != AnyClass) {
              log(s"$tvar addHiBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addHiBound bound.instantiateTypeParams(tparams, tvars)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.lo.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  log(s"$tvar addHiBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addHiBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          } else {
            if (bound.typeSymbol != NothingClass && bound.typeSymbol != tparam) {
              log(s"$tvar addLoBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addLoBound bound.instantiateTypeParams(tparams, tvars)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.hi.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  log(s"$tvar addLoBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addLoBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          }
        }
        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar

        //println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen)))
        val newInst = (
          if (up) {
            if (depth != AnyDepth) glb(tvar.constr.hiBounds, depth) else glb(tvar.constr.hiBounds)
          } else {
            if (depth != AnyDepth) lub(tvar.constr.loBounds, depth) else lub(tvar.constr.loBounds)
          }
        )
        log(s"$tvar setInst $newInst")
        tvar setInst newInst
        //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen))+" = "+tvar.constr.inst)//@MDEBUG
      }
    }

    // println("solving "+tvars+"/"+tparams+"/"+(tparams map (_.info)))
    foreach3(tvars, tparams, variances)(solveOne)
    tvars forall (tvar => tvar.constr.isWithinBounds(tvar.constr.inst))
  }
}
