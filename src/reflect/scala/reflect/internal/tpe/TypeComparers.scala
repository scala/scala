package scala
package reflect
package internal
package tpe

import scala.collection.{ mutable }
import util.{ Statistics, TriState }
import scala.annotation.tailrec

trait TypeComparers {
  self: SymbolTable =>
  import definitions._
  import TypesStats._

  private final val LogPendingSubTypesThreshold = TypeConstants.DefaultLogThreshhold

  private val _pendingSubTypes = new mutable.HashSet[SubTypePair]
  def pendingSubTypes = _pendingSubTypes

  final case class SubTypePair(tp1: Type, tp2: Type) {
    // SI-8146 we used to implement equality here in terms of pairwise =:=.
    //         But, this was inconsistent with hashCode, which was based on the
    //         Type#hashCode, based on the structure of types, not the meaning.
    //         Now, we use `Type#{equals,hashCode}` as the (consistent) basis for
    //         detecting cycles (aka keeping subtyping decidable.)
    //
    //         I added a tests to show that we detect the cycle: neg/t8146-no-finitary*

    override def toString = tp1+" <:<? "+tp2
  }

  private var _subsametypeRecursions: Int = 0
  def subsametypeRecursions = _subsametypeRecursions
  def subsametypeRecursions_=(value: Int) = _subsametypeRecursions = value

  private def isUnifiable(pre1: Type, pre2: Type) = (
       (isEligibleForPrefixUnification(pre1) || isEligibleForPrefixUnification(pre2))
    && (pre1 =:= pre2)
  )

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
      if (settings.debug) println(s"new isSubPre $sym: $pre1 <:< $pre2")
      true
    } else
      false

  private def equalSymsAndPrefixes(sym1: Symbol, pre1: Type, sym2: Symbol, pre2: Type): Boolean = (
    if (sym1 eq sym2)
      sym1.hasPackageFlag || sym1.owner.hasPackageFlag || phase.erasedTypes || pre1 =:= pre2
    else
      (sym1.name == sym2.name) && isUnifiable(pre1, pre2)
  )

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

  def isDifferentTypeConstructor(tp1: Type, tp2: Type) = !isSameTypeConstructor(tp1, tp2)

  private def isSameTypeConstructor(tr1: TypeRef, tr2: TypeRef): Boolean = (
       (tr1.sym eq tr2.sym)
    && !isDifferentType(tr1.pre, tr2.pre)
  )
  private def isSameTypeConstructor(tp1: Type, tp2: Type): Boolean = (
       tp1.isInstanceOf[TypeRef]
    && tp2.isInstanceOf[TypeRef]
    && isSameTypeConstructor(tp1.asInstanceOf[TypeRef], tp2.asInstanceOf[TypeRef])
  )

  /** Do `tp1` and `tp2` denote equivalent types? */
  def isSameType(tp1: Type, tp2: Type): Boolean = try {
    if (Statistics.canEnable) Statistics.incCounter(sametypeCount)
    subsametypeRecursions += 1
    //OPT cutdown on Function0 allocation
    //was:
    //    undoLog undoUnless {
    //      isSameType1(tp1, tp2)
    //    }

    val before = undoLog.log
    var result = false
    try {
      result = isSameType1(tp1, tp2)
    }
    finally if (!result) undoLog.undoTo(before)
    result
  }
  finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  // @pre: at least one argument has annotations
  private def sameAnnotatedTypes(tp1: Type, tp2: Type) = (
       annotationsConform(tp1, tp2)
    && annotationsConform(tp2, tp1)
    && (tp1.withoutAnnotations =:= tp2.withoutAnnotations)
  )
  // We flush out any AnnotatedTypes before calling isSameType2 because
  // unlike most other subclasses of Type, we have to allow for equivalence of any
  // combination of { tp1, tp2 } { is, is not } an AnnotatedType - this because the
  // logic of "annotationsConform" is arbitrary and unknown.
  private def isSameType1(tp1: Type, tp2: Type): Boolean = typeRelationPreCheck(tp1, tp2) match {
    case state if state.isKnown                                  => state.booleanValue
    case _ if typeHasAnnotations(tp1) || typeHasAnnotations(tp2) => sameAnnotatedTypes(tp1, tp2)
    case _                                                       => isSameType2(tp1, tp2)
  }

  private def isSameHKTypes(tp1: Type, tp2: Type) = (
       tp1.isHigherKinded
    && tp2.isHigherKinded
    && (tp1.normalize =:= tp2.normalize)
  )
  private def isSameTypeRef(tr1: TypeRef, tr2: TypeRef) = (
       equalSymsAndPrefixes(tr1.sym, tr1.pre, tr2.sym, tr2.pre)
    && (isSameHKTypes(tr1, tr2) || isSameTypes(tr1.args, tr2.args))
  )

  private def isSameSingletonType(tp1: SingletonType, tp2: SingletonType): Boolean = {
    // We don't use dealiasWiden here because we are looking for the SAME type,
    // and widening leads to a less specific type. The logic is along the lines of
    // dealiasAndFollowUnderlyingAsLongAsTheTypeIsEquivalent. This method is only
    // called after a surface comparison has failed, so if chaseDealiasedUnderlying
    // does not produce a type other than tp1 and tp2, return false.
    @tailrec def chaseDealiasedUnderlying(tp: Type): Type = tp.underlying.dealias match {
      case next: SingletonType if tp ne next => chaseDealiasedUnderlying(next)
      case _                                 => tp
    }
    val origin1 = chaseDealiasedUnderlying(tp1)
    val origin2 = chaseDealiasedUnderlying(tp2)
    ((origin1 ne tp1) || (origin2 ne tp2)) && (origin1 =:= origin2)
  }

  private def isSameMethodType(mt1: MethodType, mt2: MethodType) = (
       isSameTypes(mt1.paramTypes, mt2.paramTypes)
    && (mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params))
    && (mt1.isImplicit == mt2.isImplicit)
  )

  private def equalTypeParamsAndResult(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type) = {
    def subst(info: Type) = info.substSym(tparams2, tparams1)
    // corresponds does not check length of two sequences before checking the predicate,
    // but SubstMap assumes it has been checked (SI-2956)
    (     sameLength(tparams1, tparams2)
      && (tparams1 corresponds tparams2)((p1, p2) => methodHigherOrderTypeParamsSameVariance(p1, p2) && p1.info =:= subst(p2.info))
      && (res1 =:= subst(res2))
    )
  }

  // SI-2066 This prevents overrides with incompatible variance in higher order type parameters.
  private def methodHigherOrderTypeParamsSameVariance(sym1: Symbol, sym2: Symbol) = {
    def ignoreVariance(sym: Symbol) = !(sym.isHigherOrderTypeParameter && sym.logicallyEnclosingMember.isMethod)
    !settings.isScala211 || ignoreVariance(sym1) || ignoreVariance(sym2) || sym1.variance == sym2.variance
  }

  private def methodHigherOrderTypeParamsSubVariance(low: Symbol, high: Symbol) =
    !settings.isScala211 || methodHigherOrderTypeParamsSameVariance(low, high) || low.variance.isInvariant

  def isSameType2(tp1: Type, tp2: Type): Boolean = {
    def retry(lhs: Type, rhs: Type) = ((lhs ne tp1) || (rhs ne tp2)) && isSameType(lhs, rhs)

    /*  Here we highlight those unfortunate type-like constructs which
     *  are hidden bundles of mutable state, cruising the type system picking
     *  up any type constraints naive enough to get into their hot rods.
     */
    def mutateNonTypeConstructs(lhs: Type, rhs: Type) = lhs match {
      case BoundedWildcardType(bounds)         => bounds containsType rhs
      case tv @ TypeVar(_, _)                  => tv.registerTypeEquality(rhs, typeVarLHS = lhs eq tp1)
      case TypeRef(tv @ TypeVar(_, _), sym, _) => tv.registerTypeSelection(sym, rhs)
      case _                                   => false
    }
    /*  SingletonType receives this additional scrutiny because there are
     *  a variety of Types which must be treated as equivalent even if they
     *  arrive in different guises. For instance, object Foo in the following
     *  might appear in (at least) the four given below.
     *
     *    package pkg { object Foo ; type Bar = Foo.type }
     *
     *  ModuleClassTypeRef(pkg.type, Foo: ModuleClassSymbol, Nil)
     *  ThisType(Foo: ModuleClassSymbol)
     *  SingleType(pkg.type, Foo: ModuleSymbol)
     *  AliasTypeRef(NoPrefix, sym: AliasSymbol, Nil) where sym.info is one of the above
     */
    def sameSingletonType = tp1 match {
      case tp1: SingletonType => tp2 match {
        case tp2: SingletonType => isSameSingletonType(tp1, tp2)
        case _                  => false
      }
      case _ => false
    }

    /*  Those false cases certainly are ugly. There's a proposed SIP to deuglify it.
     *    https://docs.google.com/a/improving.org/document/d/1onPrzSqyDpHScc9PS_hpxJwa3FlPtthxw-bAuuEe8uA
     */
    def sameTypeAndSameCaseClass = tp1 match {
      case tp1: TypeRef               => tp2 match { case tp2: TypeRef               => isSameTypeRef(tp1, tp2)                              ; case _ => false }
      case tp1: MethodType            => tp2 match { case tp2: MethodType            => isSameMethodType(tp1, tp2)                           ; case _ => false }
      case RefinedType(ps1, decls1)   => tp2 match { case RefinedType(ps2, decls2)   => isSameTypes(ps1, ps2) && (decls1 isSameScope decls2) ; case _ => false }
      case SingleType(pre1, sym1)     => tp2 match { case SingleType(pre2, sym2)     => equalSymsAndPrefixes(sym1, pre1, sym2, pre2)         ; case _ => false }
      case PolyType(ps1, res1)        => tp2 match { case PolyType(ps2, res2)        => equalTypeParamsAndResult(ps1, res1, ps2, res2)       ; case _ => false }
      case ExistentialType(qs1, res1) => tp2 match { case ExistentialType(qs2, res2) => equalTypeParamsAndResult(qs1, res1, qs2, res2)       ; case _ => false }
      case ThisType(sym1)             => tp2 match { case ThisType(sym2)             => sym1 eq sym2                                         ; case _ => false }
      case ConstantType(c1)           => tp2 match { case ConstantType(c2)           => c1 == c2                                             ; case _ => false }
      case NullaryMethodType(res1)    => tp2 match { case NullaryMethodType(res2)    => res1 =:= res2                                        ; case _ => false }
      case TypeBounds(lo1, hi1)       => tp2 match { case TypeBounds(lo2, hi2)       => lo1 =:= lo2 && hi1 =:= hi2                           ; case _ => false }
      case _                          => false
    }

    (    sameTypeAndSameCaseClass
      || sameSingletonType
      || mutateNonTypeConstructs(tp1, tp2)
      || mutateNonTypeConstructs(tp2, tp1)
      || retry(normalizePlus(tp1), normalizePlus(tp2))
    )
  }

  def isSubType(tp1: Type, tp2: Type, depth: Depth = Depth.AnyDepth): Boolean = try {
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

    val before = undoLog.log
    var result = false

    try result = { // if subtype test fails, it should not affect constraints on typevars
      if (subsametypeRecursions >= LogPendingSubTypesThreshold) {
        val p = new SubTypePair(tp1, tp2)
        if (pendingSubTypes(p))
          false // see neg/t8146-no-finitary*
        else
          try {
            pendingSubTypes += p
            isSubType1(tp1, tp2, depth)
          } finally {
            pendingSubTypes -= p
          }
      } else {
        isSubType1(tp1, tp2, depth)
      }
    } finally if (!result) undoLog.undoTo(before)

    result
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  /** Check whether the subtype or type equivalence relationship
   *  between the argument is predetermined. Returns a tri-state
   *  value: True means the arguments are always sub/same types,
   *  False means they never are, and Unknown means the caller
   *  will have to figure things out.
   */
  private def typeRelationPreCheck(tp1: Type, tp2: Type): TriState = {
    def isTrue = (
         (tp1 eq tp2)
      || isErrorOrWildcard(tp1)
      || isErrorOrWildcard(tp2)
      || (tp1 eq NoPrefix) && tp2.typeSymbol.isPackageClass // !! I do not see how this would be warranted by the spec
      || (tp2 eq NoPrefix) && tp1.typeSymbol.isPackageClass // !! I do not see how this would be warranted by the spec
    )
    // isFalse, assuming !isTrue
    def isFalse = (
         (tp1 eq NoType)
      || (tp2 eq NoType)
      || (tp1 eq NoPrefix)
      || (tp2 eq NoPrefix)
    )

    if (isTrue) TriState.True
    else if (isFalse) TriState.False
    else TriState.Unknown
  }

  private def isSubType1(tp1: Type, tp2: Type, depth: Depth): Boolean = typeRelationPreCheck(tp1, tp2) match {
    case state if state.isKnown                                  => state.booleanValue
    case _ if typeHasAnnotations(tp1) || typeHasAnnotations(tp2) => annotationsConform(tp1, tp2) && (tp1.withoutAnnotations <:< tp2.withoutAnnotations)
    case _                                                       => isSubType2(tp1, tp2, depth)
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
      def cmp(p1: Symbol, p2: Symbol) = (
            methodHigherOrderTypeParamsSubVariance(p2, p1)
         && sub2(p2.info) <:< sub1(p1.info)
      )

      (tparams1 corresponds tparams2)(cmp) && (sub1(res1) <:< sub2(res2))
    }
  }
  // This is looking for situations such as B.this.x.type <:< B.super.x.type.
  // If it's a ThisType on the lhs and a SuperType on the right, and they originate
  // in the same class, and the 'x' in the ThisType has in its override chain
  // the 'x' in the SuperType, then the types conform.
  private def isThisAndSuperSubtype(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
    case (SingleType(ThisType(lpre), v1), SingleType(SuperType(ThisType(rpre), _), v2)) => (lpre eq rpre) && (v1.overrideChain contains v2)
    case _                                                                              => false
  }

  // @assume tp1.isHigherKinded || tp2.isHigherKinded
  def isHKSubType(tp1: Type, tp2: Type, depth: Depth): Boolean = {
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

    (    (tp1.typeSymbol eq NothingClass)       // @M Nothing is subtype of every well-kinded type
      || (tp2.typeSymbol eq AnyClass)           // @M Any is supertype of every well-kinded type (@PP: is it? What about continuations plugin?)
      || isSub(tp1.normalize, tp2.normalize) && annotationsConform(tp1, tp2)  // @M! normalize reduces higher-kinded case to PolyType's
    )
  }

  /** Does type `tp1` conform to `tp2`? */
  private def isSubType2(tp1: Type, tp2: Type, depth: Depth): Boolean = {
    def retry(lhs: Type, rhs: Type) = ((lhs ne tp1) || (rhs ne tp2)) && isSubType(lhs, rhs, depth)

    if (isSingleType(tp1) && isSingleType(tp2) || isConstantType(tp1) && isConstantType(tp2))
      return (tp1 =:= tp2) || isThisAndSuperSubtype(tp1, tp2) || retry(tp1.underlying, tp2)

    if (tp1.isHigherKinded || tp2.isHigherKinded)
      return isHKSubType(tp1, tp2, depth)

    /* First try, on the right:
     *   - unwrap Annotated types, BoundedWildcardTypes,
     *   - bind TypeVars  on the right, if lhs is not Annotated nor BoundedWildcard
     *   - handle common cases for first-kind TypeRefs on both sides as a fast path.
     */
    def firstTry = tp2 match {
      // fast path: two typerefs, none of them HK
      case tr2: TypeRef =>
        tp1 match {
          case tr1: TypeRef =>
            // TODO - dedicate a method to TypeRef/TypeRef subtyping.
            // These typerefs are pattern matched up and down far more
            // than is necessary.
            val sym1 = tr1.sym
            val sym2 = tr2.sym
            val pre1 = tr1.pre
            val pre2 = tr2.pre
            (((if (sym1 eq sym2) phase.erasedTypes || sym1.owner.hasPackageFlag || isSubType(pre1, pre2, depth)
            else (sym1.name == sym2.name && !sym1.isModuleClass && !sym2.isModuleClass &&
              (isUnifiable(pre1, pre2) ||
                isSameSpecializedSkolem(sym1, sym2, pre1, pre2) ||
                sym2.isAbstractType && isSubPre(pre1, pre2, sym2)))) &&
              isSubArgs(tr1.args, tr2.args, sym1.typeParams, depth))
              ||
              sym2.isClass && {
                val base = tr1 baseType sym2
                // During bootstrap, `base eq NoType` occurs about 2.5 times as often as `base ne NoType`.
                // The extra check seems like a worthwhile optimization (about 2.5M useless calls to isSubtype saved during that run).
                (base ne tr1) && (base ne NoType) && isSubType(base, tr2, depth)
              }
              ||
              thirdTryRef(tr1, tr2))
          case _ =>
            secondTry
        }
      case AnnotatedType(_, _) =>
        isSubType(tp1.withoutAnnotations, tp2.withoutAnnotations, depth) &&
          annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        isSubType(tp1, bounds.hi, depth)
      case tv2 @ TypeVar(_, constr2) =>
        tp1 match {
          case AnnotatedType(_, _) | BoundedWildcardType(_) =>
            secondTry
          case _ =>
            tv2.registerBound(tp1, isLowerBound = true)
        }
      case _ =>
        secondTry
    }

    /* Second try, on the left:
     *   - unwrap AnnotatedTypes, BoundedWildcardTypes,
     *   - bind typevars,
     *   - handle existential types by skolemization.
     */
    def secondTry = tp1 match {
      case AnnotatedType(_, _) =>
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
      def retry(lhs: Type, rhs: Type)   = isSubType(lhs, rhs, depth)
      def abstractTypeOnRight(lo: Type) = isDifferentTypeConstructor(tp2, lo) && retry(tp1, lo)
      def classOnRight                  = (
        if (isRawType(tp2)) retry(tp1, rawToExistential(tp2))
        else if (sym2.isRefinementClass) retry(tp1, sym2.info)
        else fourthTry
      )
      sym2 match {
        case SingletonClass                   => tp1.isStable || fourthTry
        case _: ClassSymbol                   => classOnRight
        case _: TypeSymbol if sym2.isDeferred => abstractTypeOnRight(tp2.bounds.lo) || fourthTry
        case _: TypeSymbol                    => retry(normalizePlus(tp1), normalizePlus(tp2))
        case _                                => fourthTry
      }
    }

    /* Third try, on the right:
     *   - decompose refined types.
     *   - handle typerefs and existentials.
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

    /* Fourth try, on the left:
     *   - handle typerefs, refined types, and singleton types.
     */
    def fourthTry = {
      def retry(lhs: Type, rhs: Type)  = ((tp1 ne lhs) || (tp2 ne rhs)) && isSubType(lhs, rhs, depth)
      def abstractTypeOnLeft(hi: Type) = isDifferentTypeConstructor(tp1, hi) && retry(hi, tp2)

      tp1 match {
        case tr1 @ TypeRef(pre1, sym1, _) =>
          def nullOnLeft = tp2 match {
            case TypeRef(_, sym2, _) => sym1 isBottomSubClass sym2
            case _                   => isSingleType(tp2) && retry(tp1, tp2.widen)
          }

          sym1 match {
            case NothingClass                             => true
            case NullClass                                => nullOnLeft
            case _: ClassSymbol if isRawType(tp1)         => retry(normalizePlus(tp1), normalizePlus(tp2))
            case _: ClassSymbol if sym1.isModuleClass     => retry(normalizePlus(tp1), normalizePlus(tp2))
            case _: ClassSymbol if sym1.isRefinementClass => retry(sym1.info, tp2)
            case _: TypeSymbol if sym1.isDeferred         => abstractTypeOnLeft(tp1.bounds.hi)
            case _: TypeSymbol                            => retry(normalizePlus(tp1), normalizePlus(tp2))
            case _                                        => false
          }
        case RefinedType(parents, _) => parents exists (retry(_, tp2))
        case _: SingletonType        => retry(tp1.underlying, tp2)
        case _                       => false
      }
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
