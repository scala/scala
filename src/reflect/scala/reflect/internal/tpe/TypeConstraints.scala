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
package internal
package tpe

import scala.collection.mutable.BitSet
import scala.collection.mutable.Clearable
import scala.reflect.internal.util.ReusableInstance

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

    if (isCompilerUniverse) {
      // register with the auto-clearing cache manager
      // perRunCaches isn't threadsafe so don't do this in runtime reflection, which doesn't (can't)
      // ever call `perRunCaches.clearAll()` anyway
      perRunCaches.recordCache(this)
    }

    /** Undo all changes to constraints to type variables up to `limit`. */
    //OPT this method is public so we can do `manual inlining`
    def undoTo(limit: UndoPairs): Unit = {
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

    def clear(): Unit = {
      if (settings.isDebug)
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
    private[this] var lobounds = lo0 filterNot (_.isNothing)
    private[this] var hibounds = hi0 filterNot (_.isAny)
    private[this] var numlo = numlo0
    private[this] var numhi = numhi0
    private[this] var avoidWidening = avoidWidening0

    def loBounds: List[Type] = if (numlo == NoType) lobounds else numlo :: lobounds
    def hiBounds: List[Type] = if (numhi == NoType) hibounds else numhi :: hibounds
    def avoidWiden: Boolean = avoidWidening
    def stopWidening(): Unit = avoidWidening = true

    def stopWideningIfPrecluded(): Unit =
      if (instValid && TypeVar.precludesWidening(inst)) stopWidening()

    def addLoBound(tp: Type, isNumericBound: Boolean = false): Unit = {
      // For some reason which is still a bit fuzzy, we must let Nothing through as
      // a lower bound despite the fact that Nothing is always a lower bound.  My current
      // supposition is that the side-effecting type constraint accumulation mechanism
      // depends on these subtype tests being performed to make forward progress when
      // there are mutually recursive type vars.
      // See pos/t6367 and pos/t6499 for the competing test cases.
      val mustConsider = tp.typeSymbol match {
        case NothingClass => true
        case _            => !lobounds.contains(tp)
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

    def checkWidening(tp: Type): Unit = {
      if (TypeVar.precludesWidening(tp)) stopWidening()
      else tp match {
        case HasTypeMember() => stopWidening()
        case _ =>
      }
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false): Unit = {
      // My current test case only demonstrates the need to let Nothing through as
      // a lower bound, but I suspect the situation is symmetrical.
      val mustConsider = typeIsAnyOrJavaObject(tp) || !(hibounds contains tp)
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
        val lo = loBounds filterNot (_.isNothing) match {
          case Nil       => ""
          case tp :: Nil => " >: " + tp
          case tps       => tps.mkString(" >: (", ", ", ")")
        }
        val hi = hiBounds filterNot typeIsAnyOrJavaObject match {
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

  private[this] val containsCollectorInstances: ReusableInstance[ContainsCollector] = ReusableInstance(new ContainsCollector(null), enabled = isCompilerUniverse)

  private[this] def containsSymbol(tp: Type, sym: Symbol): Boolean =
    containsCollectorInstances.using { cc =>
      cc.reset(sym)
      cc.collect(tp)
    }

  /** Solve constraint collected in types `tvars`.
    *
    *  @param tvars    All type variables to be instantiated.
    *  @param tparams  The type parameters corresponding to `tvars`
    *  @param getVariance Function to extract variances of type parameters; we need to reverse
    *                    solution direction for all contravariant variables.
    *  @param upper    When `true` search for max solution else min.
    */
  def solve(tvars: List[TypeVar], tparams: List[Symbol], getVariance: Variance.Extractor[Symbol], upper: Boolean, depth: Depth): Boolean = {
    assert(tvars.corresponds(tparams)((tvar, tparam) => tvar.origin.typeSymbol eq tparam), (tparams, tvars.map(_.origin.typeSymbol)))
    val areContravariant: BitSet = BitSet.empty
    foreachWithIndex(tparams)((tparam, ix) => if (getVariance(tparam).isContravariant) areContravariant += ix)

    @inline def toBound(hi: Boolean, tparam: Symbol) = if (hi) tparam.info.upperBound else tparam.info.lowerBound

    def solveOne(tvar: TypeVar, isContravariant: Boolean): Unit =
      if (tvar.constr.inst == NoType) {
        tvar.constr.inst = null // mark tvar as being solved

        val up = if (isContravariant) !upper else upper
        val tparam = tvar.origin.typeSymbol

        // don't use =:= -- we just want to know whether the tparam occurs
        // (using =:= may side-effect additional constraints / unify too much, e.g. with wildcard -- scala/bug#11558)
        @inline def tvarIsBoundOf(tparamOther: Symbol) =
          toBound(!up, tparamOther).dealias match {
            case TypeRef(_, `tparam`, Nil) => true // make sure typeArgs.isEmpty: it gets complicated with type constructor variables -- don't flip those around
            // TODO could add the PolyType equivalent for eta-expanded type constructors
            case _                         => false
          }

        val bound = toBound(up, tparam)
        var otherTypeVarBeingSolved = false

        // Solve other type vars, they are relevant when:
        //   - our current bound mentions the other tparam
        //   - our current tparam equals the other tparam's bound (we'll add the symmetric bound below)
        foreachWithIndex(tvars) { (tvarOther, ix) =>
          val tparamOther = tvarOther.origin.typeSymbol
          if ((tparamOther ne tparam) && containsSymbol(bound, tparamOther) || tvarIsBoundOf(tparamOther)) {
            if (tvarOther.constr.inst eq null) otherTypeVarBeingSolved = true
            solveOne(tvarOther, areContravariant(ix))
          }
        }

        if (!(otherTypeVarBeingSolved || containsSymbol(bound, tparam))) {
          val boundSym = bound.typeSymbol
          if (up) {
            if (boundSym != AnyClass)
              tvar.addHiBound(bound.instantiateTypeParams(tparams, tvars))
          } else {
            if (boundSym != tparam && boundSym != NothingClass)
              tvar.addLoBound(bound.instantiateTypeParams(tparams, tvars))
          }

          // Derive more constraints for `tvar` from its symmetric occurrences in the bounds of other tparams.
          tvars.foreach { tvarOther =>
            val tparamOther = tvarOther.origin.typeSymbol
            if ((tparamOther ne tparam) && tvarIsBoundOf(tparamOther)) {
              if (up) tvar.addHiBound(tvarOther) else tvar.addLoBound(tvarOther)
            }
          }
        }

        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar (about to lub/glb the bounds)

        val newInst =
          if (up || tvar.constr.hiBounds.exists(isSingleType)) { // If we have a singleton upper bound then we should use it.
            if (depth.isAnyDepth) glb(tvar.constr.hiBounds) else glb(tvar.constr.hiBounds, depth)
          } else {
            if (depth.isAnyDepth) lub(tvar.constr.loBounds) else lub(tvar.constr.loBounds, depth)
          }

        // debuglog(s"$tvar setInst $newInst")
        tvar setInst newInst
      }

    // println("solving "+tvars+"/"+tparams+"/"+(tparams map (_.info)))
    foreachWithIndex(tvars)((tvar, i) => solveOne(tvar, areContravariant(i)))

//    def logBounds(tv: TypeVar) = log {
//      val what = if (!tv.instValid) "is invalid" else s"does not conform to bounds: ${tv.constr}"
//      s"Inferred type for ${tv.originString} (${tv.inst}) $what"
//    }

    tvars forall (_.instWithinBounds) // || logBounds(tv)
  }
}

private[internal] object TypeConstraints {
  // UndoPair is declared in companion object to not hold an outer pointer reference
  final case class UndoPair[TypeVar <: SymbolTable#TypeVar,
    TypeConstraint <: TypeConstraints#TypeConstraint](tv: TypeVar, tConstraint: TypeConstraint)
}
