package scala.reflect
package internal
package tpe

import scala.collection.{ mutable }
import Flags._
import util.Statistics
import scala.annotation.tailrec

trait TypeComparers {
  self: SymbolTable =>
  import definitions._
  import TypesStats._

  private final val LogPendingSubTypesThreshold = DefaultLogThreshhold

  private val pendingSubTypes = new mutable.HashSet[SubTypePair]

  class SubTypePair(val tp1: Type, val tp2: Type) {
    override def hashCode = tp1.hashCode * 41 + tp2.hashCode
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      // suspend TypeVars in types compared by =:=,
      // since we don't want to mutate them simply to check whether a subtype test is pending
      // in addition to making subtyping "more correct" for type vars,
      // it should avoid the stackoverflow that's been plaguing us (https://groups.google.com/d/topic/scala-internals/2gHzNjtB4xA/discussion)
      // this method is only called when subtyping hits a recursion threshold (subsametypeRecursions >= LogPendingSubTypesThreshold)
      case stp: SubTypePair =>
        val tvars = List(tp1, stp.tp1, tp2, stp.tp2) flatMap (t => if (t.isGround) Nil else typeVarsInType(t))
        suspendingTypeVars(tvars)(tp1 =:= stp.tp1 && tp2 =:= stp.tp2)
      case _ =>
        false
    })
    override def toString = tp1+" <:<? "+tp2
  }

  private var subsametypeRecursions: Int = 0

  private def isUnifiable(pre1: Type, pre2: Type) =
    (beginsWithTypeVarOrIsRefined(pre1) || beginsWithTypeVarOrIsRefined(pre2)) && (pre1 =:= pre2)

  /** Returns true iff we are past phase specialize,
    *  sym1 and sym2 are two existential skolems with equal names and bounds,
    *  and pre1 and pre2 are equal prefixes
    */
  private def isSameSpecializedSkolem(sym1: Symbol, sym2: Symbol, pre1: Type, pre2: Type) = {
    sym1.isExistentialSkolem && sym2.isExistentialSkolem &&
      sym1.name == sym2.name &&
      phase.specialized &&
      sym1.info =:= sym2.info &&
      pre1 =:= pre2
  }

  private def isSubPre(pre1: Type, pre2: Type, sym: Symbol) =
    if ((pre1 ne pre2) && (pre1 ne NoPrefix) && (pre2 ne NoPrefix) && pre1 <:< pre2) {
      if (settings.debug.value) println(s"new isSubPre $sym: $pre1 <:< $pre2")
      true
    } else
      false

  private def equalSymsAndPrefixes(sym1: Symbol, pre1: Type, sym2: Symbol, pre2: Type): Boolean =
    if (sym1 == sym2) sym1.hasPackageFlag || sym1.owner.hasPackageFlag || phase.erasedTypes || pre1 =:= pre2
    else (sym1.name == sym2.name) && isUnifiable(pre1, pre2)


  def isDifferentType(tp1: Type, tp2: Type): Boolean = try {
    subsametypeRecursions += 1
    undoLog undo { // undo type constraints that arise from operations in this block
      !isSameType1(tp1, tp2)
    }
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  def isDifferentTypeConstructor(tp1: Type, tp2: Type): Boolean = tp1 match {
    case TypeRef(pre1, sym1, _) =>
      tp2 match {
        case TypeRef(pre2, sym2, _) => sym1 != sym2 || isDifferentType(pre1, pre2)
        case _ => true
      }
    case _ => true
  }

  /** Do `tp1` and `tp2` denote equivalent types? */
  def isSameType(tp1: Type, tp2: Type): Boolean = try {
    if (Statistics.canEnable) Statistics.incCounter(sametypeCount)
    subsametypeRecursions += 1
    //OPT cutdown on Function0 allocation
    //was:
    //    undoLog undoUnless {
    //      isSameType1(tp1, tp2)
    //    }

    undoLog.lock()
    try {
      val before = undoLog.log
      var result = false
      try {
        result = isSameType1(tp1, tp2)
      }
      finally if (!result) undoLog.undoTo(before)
      result
    }
    finally undoLog.unlock()
  }
  finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  private def isSameType1(tp1: Type, tp2: Type): Boolean = {
    if ((tp1 eq tp2) ||
      (tp1 eq ErrorType) || (tp1 eq WildcardType) ||
      (tp2 eq ErrorType) || (tp2 eq WildcardType))
      true
    else if ((tp1 eq NoType) || (tp2 eq NoType))
      false
    else if (tp1 eq NoPrefix) // !! I do not see how this would be warranted by the spec
      tp2.typeSymbol.isPackageClass
    else if (tp2 eq NoPrefix) // !! I do not see how this would be warranted by the spec
      tp1.typeSymbol.isPackageClass
    else {
      isSameType2(tp1, tp2) || {
        val tp1n = normalizePlus(tp1)
        val tp2n = normalizePlus(tp2)
        ((tp1n ne tp1) || (tp2n ne tp2)) && isSameType(tp1n, tp2n)
      }
    }
  }

  def isSameType2(tp1: Type, tp2: Type): Boolean = {
    tp1 match {
      case tr1: TypeRef =>
        tp2 match {
          case tr2: TypeRef =>
            return (equalSymsAndPrefixes(tr1.sym, tr1.pre, tr2.sym, tr2.pre) &&
              ((tp1.isHigherKinded && tp2.isHigherKinded && tp1.normalize =:= tp2.normalize) ||
                isSameTypes(tr1.args, tr2.args))) ||
              ((tr1.pre, tr2.pre) match {
                case (tv @ TypeVar(_,_), _) => tv.registerTypeSelection(tr1.sym, tr2)
                case (_, tv @ TypeVar(_,_)) => tv.registerTypeSelection(tr2.sym, tr1)
                case _ => false
              })
          case _: SingleType =>
            return isSameType2(tp2, tp1)  // put singleton type on the left, caught below
          case _ =>
        }
      case tt1: ThisType =>
        tp2 match {
          case tt2: ThisType =>
            if (tt1.sym == tt2.sym) return true
          case _ =>
        }
      case st1: SingleType =>
        tp2 match {
          case st2: SingleType =>
            if (equalSymsAndPrefixes(st1.sym, st1.pre, st2.sym, st2.pre)) return true
          case TypeRef(pre2, sym2, Nil) =>
            if (sym2.isModuleClass && equalSymsAndPrefixes(st1.sym, st1.pre, sym2.sourceModule, pre2)) return true
          case _ =>
        }
      case ct1: ConstantType =>
        tp2 match {
          case ct2: ConstantType =>
            return (ct1.value == ct2.value)
          case _ =>
        }
      case rt1: RefinedType =>
        tp2 match {
          case rt2: RefinedType => //
            def isSubScope(s1: Scope, s2: Scope): Boolean = s2.toList.forall {
              sym2 =>
                var e1 = s1.lookupEntry(sym2.name)
                (e1 ne null) && {
                  val substSym = sym2.info.substThis(sym2.owner, e1.sym.owner)
                  var isEqual = false
                  while (!isEqual && (e1 ne null)) {
                    isEqual = e1.sym.info =:= substSym
                    e1 = s1.lookupNextEntry(e1)
                  }
                  isEqual
                }
            }
            //Console.println("is same? " + tp1 + " " + tp2 + " " + tp1.typeSymbol.owner + " " + tp2.typeSymbol.owner)//DEBUG
            return isSameTypes(rt1.parents, rt2.parents) && {
              val decls1 = rt1.decls
              val decls2 = rt2.decls
              isSubScope(decls1, decls2) && isSubScope(decls2, decls1)
            }
          case _ =>
        }
      case mt1: MethodType =>
        tp2 match {
          case mt2: MethodType =>
            return isSameTypes(mt1.paramTypes, mt2.paramTypes) &&
              mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params) &&
              mt1.isImplicit == mt2.isImplicit
          // note: no case NullaryMethodType(restpe) => return mt1.params.isEmpty && mt1.resultType =:= restpe
          case _ =>
        }
      case NullaryMethodType(restpe1) =>
        tp2 match {
          // note: no case mt2: MethodType => return mt2.params.isEmpty && restpe  =:= mt2.resultType
          case NullaryMethodType(restpe2) =>
            return restpe1 =:= restpe2
          case _ =>
        }
      case PolyType(tparams1, res1) =>
        tp2 match {
          case PolyType(tparams2, res2) =>
            //            assert((tparams1 map (_.typeParams.length)) == (tparams2 map (_.typeParams.length)))
            // @M looks like it might suffer from same problem as #2210
            return (
              (sameLength(tparams1, tparams2)) && // corresponds does not check length of two sequences before checking the predicate
                (tparams1 corresponds tparams2)(_.info =:= _.info.substSym(tparams2, tparams1)) &&
                res1 =:= res2.substSym(tparams2, tparams1)
              )
          case _ =>
        }
      case ExistentialType(tparams1, res1) =>
        tp2 match {
          case ExistentialType(tparams2, res2) =>
            // @M looks like it might suffer from same problem as #2210
            return (
              // corresponds does not check length of two sequences before checking the predicate -- faster & needed to avoid crasher in #2956
              sameLength(tparams1, tparams2) &&
                (tparams1 corresponds tparams2)(_.info =:= _.info.substSym(tparams2, tparams1)) &&
                res1 =:= res2.substSym(tparams2, tparams1)
              )
          case _ =>
        }
      case TypeBounds(lo1, hi1) =>
        tp2 match {
          case TypeBounds(lo2, hi2) =>
            return lo1 =:= lo2 && hi1 =:= hi2
          case _ =>
        }
      case BoundedWildcardType(bounds) =>
        return bounds containsType tp2
      case _ =>
    }
    tp2 match {
      case BoundedWildcardType(bounds) =>
        return bounds containsType tp1
      case _ =>
    }
    tp1 match {
      case tv @ TypeVar(_,_) =>
        return tv.registerTypeEquality(tp2, typeVarLHS = true)
      case _ =>
    }
    tp2 match {
      case tv @ TypeVar(_,_) =>
        return tv.registerTypeEquality(tp1, typeVarLHS = false)
      case _ =>
    }
    tp1 match {
      case _: AnnotatedType =>
        return annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations
      case _ =>
    }
    tp2 match {
      case _: AnnotatedType =>
        return annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations
      case _ =>
    }
    tp1 match {
      case _: SingletonType =>
        tp2 match {
          case _: SingletonType =>
            def chaseDealiasedUnderlying(tp: Type): Type = {
              var origin = tp
              var next = origin.underlying.dealias
              while (next.isInstanceOf[SingletonType]) {
                assert(origin ne next, origin)
                origin = next
                next = origin.underlying.dealias
              }
              origin
            }
            val origin1 = chaseDealiasedUnderlying(tp1)
            val origin2 = chaseDealiasedUnderlying(tp2)
            ((origin1 ne tp1) || (origin2 ne tp2)) && (origin1 =:= origin2)
          case _ =>
            false
        }
      case _ =>
        false
    }
  }

  def isSubType(tp1: Type, tp2: Type): Boolean = isSubType(tp1, tp2, AnyDepth)

  def isSubType(tp1: Type, tp2: Type, depth: Int): Boolean = try {
    subsametypeRecursions += 1

    //OPT cutdown on Function0 allocation
    //was:
    //    undoLog undoUnless { // if subtype test fails, it should not affect constraints on typevars
    //      if (subsametypeRecursions >= LogPendingSubTypesThreshold) {
    //        val p = new SubTypePair(tp1, tp2)
    //        if (pendingSubTypes(p))
    //          false
    //        else
    //          try {
    //            pendingSubTypes += p
    //            isSubType2(tp1, tp2, depth)
    //          } finally {
    //            pendingSubTypes -= p
    //          }
    //      } else {
    //        isSubType2(tp1, tp2, depth)
    //      }
    //    }

    undoLog.lock()
    try {
      val before = undoLog.log
      var result = false

      try result = { // if subtype test fails, it should not affect constraints on typevars
        if (subsametypeRecursions >= LogPendingSubTypesThreshold) {
          val p = new SubTypePair(tp1, tp2)
          if (pendingSubTypes(p))
            false
          else
            try {
              pendingSubTypes += p
              isSubType2(tp1, tp2, depth)
            } finally {
              pendingSubTypes -= p
            }
        } else {
          isSubType2(tp1, tp2, depth)
        }
      } finally if (!result) undoLog.undoTo(before)

      result
    } finally undoLog.unlock()
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  private def isPolySubType(tp1: PolyType, tp2: PolyType): Boolean = {
    val PolyType(tparams1, res1) = tp1
    val PolyType(tparams2, res2) = tp2

    sameLength(tparams1, tparams2) && {
      // fast-path: polymorphic method type -- type params cannot be captured
      val isMethod = tparams1.head.owner.isMethod
      //@M for an example of why we need to generate fresh symbols otherwise, see neg/tcpoly_ticket2101.scala
      val substitutes = if (isMethod) tparams1 else cloneSymbols(tparams1)
      def sub1(tp: Type) = if (isMethod) tp else tp.substSym(tparams1, substitutes)
      def sub2(tp: Type) = tp.substSym(tparams2, substitutes)
      def cmp(p1: Symbol, p2: Symbol) = sub2(p2.info) <:< sub1(p1.info)

      (tparams1 corresponds tparams2)(cmp) && (sub1(res1) <:< sub2(res2))
    }
  }

  // @assume tp1.isHigherKinded || tp2.isHigherKinded
  def isHKSubType(tp1: Type, tp2: Type, depth: Int): Boolean = {
    def isSub(ntp1: Type, ntp2: Type) = (ntp1.withoutAnnotations, ntp2.withoutAnnotations) match {
      case (TypeRef(_, AnyClass, _), _)                                     => false                    // avoid some warnings when Nothing/Any are on the other side
      case (_, TypeRef(_, NothingClass, _))                                 => false
      case (pt1: PolyType, pt2: PolyType)                                   => isPolySubType(pt1, pt2)  // @assume both .isHigherKinded (both normalized to PolyType)
      case (_: PolyType, MethodType(ps, _)) if ps exists (_.tpe.isWildcard) => false                    // don't warn on HasMethodMatching on right hand side
      case _                                                                =>                          // @assume !(both .isHigherKinded) thus cannot be subtypes
        def tp_s(tp: Type): String = f"$tp%-20s ${util.shortClassOfInstance(tp)}%s"
        devWarning(s"HK subtype check on $tp1 and $tp2, but both don't normalize to polytypes:\n  tp1=${tp_s(ntp1)}\n  tp2=${tp_s(ntp2)}")
        false
    }

    (    tp1.typeSymbol == NothingClass       // @M Nothing is subtype of every well-kinded type
      || tp2.typeSymbol == AnyClass           // @M Any is supertype of every well-kinded type (@PP: is it? What about continuations plugin?)
      || isSub(tp1.normalize, tp2.normalize) && annotationsConform(tp1, tp2)  // @M! normalize reduces higher-kinded case to PolyType's
    )
  }

  /** Does type `tp1` conform to `tp2`? */
  private def isSubType2(tp1: Type, tp2: Type, depth: Int): Boolean = {
    if ((tp1 eq tp2) || isErrorOrWildcard(tp1) || isErrorOrWildcard(tp2)) return true
    if ((tp1 eq NoType) || (tp2 eq NoType)) return false
    if (tp1 eq NoPrefix) return (tp2 eq NoPrefix) || tp2.typeSymbol.isPackageClass // !! I do not see how the "isPackageClass" would be warranted by the spec
    if (tp2 eq NoPrefix) return tp1.typeSymbol.isPackageClass
    if (isSingleType(tp1) && isSingleType(tp2) || isConstantType(tp1) && isConstantType(tp2)) return tp1 =:= tp2
    if (tp1.isHigherKinded || tp2.isHigherKinded) return isHKSubType(tp1, tp2, depth)

    /** First try, on the right:
      *   - unwrap Annotated types, BoundedWildcardTypes,
      *   - bind TypeVars  on the right, if lhs is not Annotated nor BoundedWildcard
      *   - handle common cases for first-kind TypeRefs on both sides as a fast path.
      */
    def firstTry = tp2 match {
      // fast path: two typerefs, none of them HK
      case tr2: TypeRef =>
        tp1 match {
          case tr1: TypeRef =>
            val sym1 = tr1.sym
            val sym2 = tr2.sym
            val pre1 = tr1.pre
            val pre2 = tr2.pre
            (((if (sym1 == sym2) phase.erasedTypes || sym1.owner.hasPackageFlag || isSubType(pre1, pre2, depth)
            else (sym1.name == sym2.name && !sym1.isModuleClass && !sym2.isModuleClass &&
              (isUnifiable(pre1, pre2) ||
                isSameSpecializedSkolem(sym1, sym2, pre1, pre2) ||
                sym2.isAbstractType && isSubPre(pre1, pre2, sym2)))) &&
              isSubArgs(tr1.args, tr2.args, sym1.typeParams, depth))
              ||
              sym2.isClass && {
                val base = tr1 baseType sym2
                (base ne tr1) && isSubType(base, tr2, depth)
              }
              ||
              thirdTryRef(tr1, tr2))
          case _ =>
            secondTry
        }
      case AnnotatedType(_, _, _) =>
        isSubType(tp1.withoutAnnotations, tp2.withoutAnnotations, depth) &&
          annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        isSubType(tp1, bounds.hi, depth)
      case tv2 @ TypeVar(_, constr2) =>
        tp1 match {
          case AnnotatedType(_, _, _) | BoundedWildcardType(_) =>
            secondTry
          case _ =>
            tv2.registerBound(tp1, isLowerBound = true)
        }
      case _ =>
        secondTry
    }

    /** Second try, on the left:
      *   - unwrap AnnotatedTypes, BoundedWildcardTypes,
      *   - bind typevars,
      *   - handle existential types by skolemization.
      */
    def secondTry = tp1 match {
      case AnnotatedType(_, _, _) =>
        isSubType(tp1.withoutAnnotations, tp2.withoutAnnotations, depth) &&
          annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        isSubType(tp1.bounds.lo, tp2, depth)
      case tv @ TypeVar(_,_) =>
        tv.registerBound(tp2, isLowerBound = false)
      case ExistentialType(_, _) =>
        try {
          skolemizationLevel += 1
          isSubType(tp1.skolemizeExistential, tp2, depth)
        } finally {
          skolemizationLevel -= 1
        }
      case _ =>
        thirdTry
    }

    def thirdTryRef(tp1: Type, tp2: TypeRef): Boolean = {
      val sym2 = tp2.sym
      sym2 match {
        case NotNullClass => tp1.isNotNull
        case SingletonClass => tp1.isStable || fourthTry
        case _: ClassSymbol =>
          if (isRawType(tp2))
            isSubType(tp1, rawToExistential(tp2), depth)
          else if (sym2.name == tpnme.REFINE_CLASS_NAME)
            isSubType(tp1, sym2.info, depth)
          else
            fourthTry
        case _: TypeSymbol =>
          if (sym2 hasFlag DEFERRED) {
            val tp2a = tp2.bounds.lo
            isDifferentTypeConstructor(tp2, tp2a) &&
              isSubType(tp1, tp2a, depth) ||
              fourthTry
          } else {
            isSubType(tp1.normalize, tp2.normalize, depth)
          }
        case _ =>
          fourthTry
      }
    }

    /** Third try, on the right:
      *   - decompose refined types.
      *   - handle typerefs, existentials, and notnull types.
      *   - handle left+right method types, polytypes, typebounds
      */
    def thirdTry = tp2 match {
      case tr2: TypeRef =>
        thirdTryRef(tp1, tr2)
      case rt2: RefinedType =>
        (rt2.parents forall (isSubType(tp1, _, depth))) &&
          (rt2.decls forall (specializesSym(tp1, _, depth)))
      case et2: ExistentialType =>
        et2.withTypeVars(isSubType(tp1, _, depth), depth) || fourthTry
      case nn2: NotNullType =>
        tp1.isNotNull && isSubType(tp1, nn2.underlying, depth)
      case mt2: MethodType =>
        tp1 match {
          case mt1 @ MethodType(params1, res1) =>
            val params2 = mt2.params
            val res2 = mt2.resultType
            (sameLength(params1, params2) &&
              mt1.isImplicit == mt2.isImplicit &&
              matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&
              isSubType(res1.substSym(params1, params2), res2, depth))
          // TODO: if mt1.params.isEmpty, consider NullaryMethodType?
          case _ =>
            false
        }
      case pt2 @ NullaryMethodType(_) =>
        tp1 match {
          // TODO: consider MethodType mt for which mt.params.isEmpty??
          case pt1 @ NullaryMethodType(_) =>
            isSubType(pt1.resultType, pt2.resultType, depth)
          case _ =>
            false
        }
      case TypeBounds(lo2, hi2) =>
        tp1 match {
          case TypeBounds(lo1, hi1) =>
            isSubType(lo2, lo1, depth) && isSubType(hi1, hi2, depth)
          case _ =>
            false
        }
      case _ =>
        fourthTry
    }

    /** Fourth try, on the left:
      *   - handle typerefs, refined types, notnull and singleton types.
      */
    def fourthTry = tp1 match {
      case tr1 @ TypeRef(pre1, sym1, _) =>
        sym1 match {
          case NothingClass => true
          case NullClass =>
            tp2 match {
              case TypeRef(_, sym2, _) =>
                containsNull(sym2)
              case _ =>
                isSingleType(tp2) && isSubType(tp1, tp2.widen, depth)
            }
          case _: ClassSymbol =>
            if (isRawType(tp1))
              isSubType(rawToExistential(tp1), tp2, depth)
            else if (sym1.isModuleClass) tp2 match {
              case SingleType(pre2, sym2) => equalSymsAndPrefixes(sym1.sourceModule, pre1, sym2, pre2)
              case _                      => false
            }
            else if (sym1.isRefinementClass)
              isSubType(sym1.info, tp2, depth)
            else false

          case _: TypeSymbol =>
            if (sym1 hasFlag DEFERRED) {
              val tp1a = tp1.bounds.hi
              isDifferentTypeConstructor(tp1, tp1a) && isSubType(tp1a, tp2, depth)
            } else {
              isSubType(tp1.normalize, tp2.normalize, depth)
            }
          case _ =>
            false
        }
      case RefinedType(parents1, _) =>
        parents1 exists (isSubType(_, tp2, depth))
      case _: SingletonType | _: NotNullType =>
        isSubType(tp1.underlying, tp2, depth)
      case _ =>
        false
    }

    firstTry
  }


  def isWeakSubType(tp1: Type, tp2: Type) =
    tp1.dealiasWiden match {
      case TypeRef(_, sym1, _) if isNumericValueClass(sym1) =>
        tp2.deconst.dealias match {
          case TypeRef(_, sym2, _) if isNumericValueClass(sym2) =>
            isNumericSubClass(sym1, sym2)
          case tv2 @ TypeVar(_, _) =>
            tv2.registerBound(tp1, isLowerBound = true, isNumericBound = true)
          case _ =>
            isSubType(tp1, tp2)
        }
      case tv1 @ TypeVar(_, _) =>
        tp2.deconst.dealias match {
          case TypeRef(_, sym2, _) if isNumericValueClass(sym2) =>
            tv1.registerBound(tp2, isLowerBound = false, isNumericBound = true)
          case _ =>
            isSubType(tp1, tp2)
        }
      case _ =>
        isSubType(tp1, tp2)
    }

  def isNumericSubType(tp1: Type, tp2: Type) = (
    isNumericSubClass(primitiveBaseClass(tp1.dealiasWiden), primitiveBaseClass(tp2.dealias))
   )

  /** If the given type has a primitive class among its base classes,
   *  the symbol of that class. Otherwise, NoSymbol.
   */
  private def primitiveBaseClass(tp: Type): Symbol = {
    @tailrec def loop(bases: List[Symbol]): Symbol = bases match {
      case Nil     => NoSymbol
      case x :: xs => if (isPrimitiveValueClass(x)) x else loop(xs)
    }
    loop(tp.baseClasses)
  }
}
