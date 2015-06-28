package scala
package reflect
package internal
package tpe

import scala.collection.{ generic }
import generic.Clearable

private[internal] trait TypeConstraints {
  self: SymbolTable =>
  import definitions._

  /** A log of type variable with their original constraints. Used in order
    *  to undo constraints in the case of isSubType/isSameType failure.
    */
  private lazy val _undoLog = new UndoLog
  def undoLog = _undoLog

  import TypeConstraints.UndoPair
  class UndoLog extends Clearable {
    type UndoPairs = List[UndoPair[TypeVar, TypeConstraint]]
    //OPT this method is public so we can do `manual inlining`
    var log: UndoPairs = List()

    // register with the auto-clearing cache manager
    perRunCaches.recordCache(this)

    /** Undo all changes to constraints to type variables upto `limit`. */
    //OPT this method is public so we can do `manual inlining`
    def undoTo(limit: UndoPairs) {
      assertCorrectThread()
      while ((log ne limit) && log.nonEmpty) {
        val UndoPair(tv, constr) = log.head
        tv.constr = constr
        log = log.tail
      }
    }

    /** No sync necessary, because record should only
      *  be called from within an undo or undoUnless block,
      *  which is already synchronized.
      */
    private[reflect] def record(tv: TypeVar) = {
      log ::= UndoPair(tv, tv.constr.cloneInternal)
    }

    def clear() {
      if (settings.debug)
        self.log("Clearing " + log.size + " entries from the undoLog.")
      log = Nil
    }

    // `block` should not affect constraints on typevars
    def undo[T](block: => T): T = {
      val before = log
      try block
      finally undoTo(before)
    }
  }

  /** @PP: Unable to see why these apparently constant types should need vals
    *  in every TypeConstraint, I lifted them out.
    */
  private lazy val numericLoBound = IntTpe
  private lazy val numericHiBound = intersectionType(List(ByteTpe, CharTpe), ScalaPackageClass)

  /** A class expressing upper and lower bounds constraints of type variables,
    * as well as their instantiations.
    */
  class TypeConstraint(lo0: List[Type], hi0: List[Type], numlo0: Type, numhi0: Type, avoidWidening0: Boolean = false) {
    def this(lo0: List[Type], hi0: List[Type]) = this(lo0, hi0, NoType, NoType)
    def this(bounds: TypeBounds) = this(List(bounds.lo), List(bounds.hi))
    def this() = this(List(), List())

    /*  Syncnote: Type constraints are assumed to be used from only one
     *  thread. They are not exposed in api.Types and are used only locally
     *  in operations that are exposed from types. Hence, no syncing of any
     *  variables should be necessary.
     */

    /** Guard these lists against AnyClass and NothingClass appearing,
      *  else loBounds.isEmpty will have different results for an empty
      *  constraint and one with Nothing as a lower bound.  [Actually
      *  guarding addLoBound/addHiBound somehow broke raw types so it
      *  only guards against being created with them.]
      */
    private var lobounds = lo0 filterNot typeIsNothing
    private var hibounds = hi0 filterNot typeIsAny
    private var numlo = numlo0
    private var numhi = numhi0
    private var avoidWidening = avoidWidening0

    def loBounds: List[Type] = if (numlo == NoType) lobounds else numlo :: lobounds
    def hiBounds: List[Type] = if (numhi == NoType) hibounds else numhi :: hibounds
    def avoidWiden: Boolean = avoidWidening

    def addLoBound(tp: Type, isNumericBound: Boolean = false) {
      // For some reason which is still a bit fuzzy, we must let Nothing through as
      // a lower bound despite the fact that Nothing is always a lower bound.  My current
      // supposition is that the side-effecting type constraint accumulation mechanism
      // depends on these subtype tests being performed to make forward progress when
      // there are mutually recursive type vars.
      // See pos/t6367 and pos/t6499 for the competing test cases.
      val mustConsider = tp.typeSymbol match {
        case NothingClass => true
        case _            => !(lobounds contains tp)
      }
      if (mustConsider) {
        if (isNumericBound && isNumericValueType(tp)) {
          if (numlo == NoType || isNumericSubType(numlo, tp))
            numlo = tp
          else if (!isNumericSubType(tp, numlo))
            numlo = numericLoBound
        }
        else lobounds ::= tp
      }
    }

    def checkWidening(tp: Type) {
      if(tp.isStable) avoidWidening = true
      else tp match {
        case HasTypeMember(_, _) => avoidWidening = true
        case _ =>
      }
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false) {
      // My current test case only demonstrates the need to let Nothing through as
      // a lower bound, but I suspect the situation is symmetrical.
      val mustConsider = tp.typeSymbol match {
        case AnyClass => true
        case _        => !(hibounds contains tp)
      }
      if (mustConsider) {
        checkWidening(tp)
        if (isNumericBound && isNumericValueType(tp)) {
          if (numhi == NoType || isNumericSubType(tp, numhi))
            numhi = tp
          else if (!isNumericSubType(numhi, tp))
            numhi = numericHiBound
        }
        else hibounds ::= tp
      }
    }

    def instWithinBounds = instValid && isWithinBounds(inst)

    def isWithinBounds(tp: Type): Boolean = (
         lobounds.forall(_ <:< tp)
      && hibounds.forall(tp <:< _)
      && (numlo == NoType || (numlo weak_<:< tp))
      && (numhi == NoType || (tp weak_<:< numhi))
    )

    var inst: Type = NoType // @M reduce visibility?

    def instValid = (inst ne null) && (inst ne NoType)

    def cloneInternal = {
      val tc = new TypeConstraint(lobounds, hibounds, numlo, numhi, avoidWidening)
      tc.inst = inst
      tc
    }

    override def toString = {
      val boundsStr = {
        val lo = loBounds filterNot typeIsNothing match {
          case Nil       => ""
          case tp :: Nil => " >: " + tp
          case tps       => tps.mkString(" >: (", ", ", ")")
        }
        val hi = hiBounds filterNot typeIsAny match {
          case Nil       => ""
          case tp :: Nil => " <: " + tp
          case tps       => tps.mkString(" <: (", ", ", ")")
        }
        lo + hi
      }
      if (inst eq NoType) boundsStr
      else boundsStr + " _= " + inst.safeToString
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
  def solve(tvars: List[TypeVar], tparams: List[Symbol], variances: List[Variance], upper: Boolean, depth: Depth): Boolean = {

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Variance) {
      if (tvar.constr.inst == NoType) {
        val up = if (variance.isContravariant) !upper else upper
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
              debuglog(s"$tvar addHiBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addHiBound bound.instantiateTypeParams(tparams, tvars)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.lo.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  debuglog(s"$tvar addHiBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addHiBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          } else {
            if (bound.typeSymbol != NothingClass && bound.typeSymbol != tparam) {
              debuglog(s"$tvar addLoBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addLoBound bound.instantiateTypeParams(tparams, tvars)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.hi.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  debuglog(s"$tvar addLoBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addLoBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          }
        }
        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar

        //println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen)))
        val newInst = (
          if (up) {
            if (depth.isAnyDepth) glb(tvar.constr.hiBounds)
            else glb(tvar.constr.hiBounds, depth)
          }
          else {
            if (depth.isAnyDepth) lub(tvar.constr.loBounds)
            else lub(tvar.constr.loBounds, depth)
          }
          )

        debuglog(s"$tvar setInst $newInst")
        tvar setInst newInst
        //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen))+" = "+tvar.constr.inst)//@MDEBUG
      }
    }

    // println("solving "+tvars+"/"+tparams+"/"+(tparams map (_.info)))
    foreach3(tvars, tparams, variances)(solveOne)

    def logBounds(tv: TypeVar) = log {
      val what = if (!tv.instValid) "is invalid" else s"does not conform to bounds: ${tv.constr}"
      s"Inferred type for ${tv.originString} (${tv.inst}) $what"
    }

    tvars forall (tv => tv.instWithinBounds || util.andFalse(logBounds(tv)))
  }
}

private[internal] object TypeConstraints {
  // UndoPair is declared in companion object to not hold an outer pointer reference
  final case class UndoPair[TypeVar <: SymbolTable#TypeVar,
    TypeConstraint <: TypeConstraints#TypeConstraint](tv: TypeVar, tConstraint: TypeConstraint)
}
