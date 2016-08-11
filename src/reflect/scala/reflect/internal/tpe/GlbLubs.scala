package scala
package reflect
package internal
package tpe

import scala.collection.mutable
import scala.annotation.tailrec
import util.Statistics
import Variance._

private[internal] trait GlbLubs {
  self: SymbolTable =>
  import definitions._
  import TypesStats._

  private final val printLubs = scala.sys.props contains "scalac.debug.lub"
  private final val strictInference = settings.strictInference

  /** In case anyone wants to turn off lub verification without reverting anything. */
  private final val verifyLubs = true

  private def printLubMatrix(btsMap: Map[Type, List[Type]], depth: Depth) {
    import util.TableDef
    import TableDef.Column
    def str(tp: Type) = {
      if (tp == NoType) ""
      else {
        val s = ("" + tp).replaceAll("""[\w.]+\.(\w+)""", "$1")
        if (s.length < 60) s
        else (s take 57) + "..."
      }
    }

    val sorted       = btsMap.toList.sortWith((x, y) => x._1.typeSymbol isLess y._1.typeSymbol)
    val maxSeqLength = sorted.map(_._2.size).max
    val padded       = sorted map (_._2.padTo(maxSeqLength, NoType))
    val transposed   = padded.transpose

    val columns: List[Column[List[Type]]] = mapWithIndex(sorted) {
      case ((k, v), idx) =>
        Column(str(k), (xs: List[Type]) => str(xs(idx)), left = true)
    }

    val tableDef = TableDef(columns: _*)
    val formatted = tableDef.table(transposed)
    println("** Depth is " + depth + "\n" + formatted)
  }

  /** From a list of types, find any which take type parameters
    *  where the type parameter bounds contain references to other
    *  any types in the list (including itself.)
    *
    *  @return List of symbol pairs holding the recursive type
    *    parameter and the parameter which references it.
    */
  def findRecursiveBounds(ts: List[Type]): List[(Symbol, Symbol)] = {
    if (ts.isEmpty) Nil
    else {
      val sym = ts.head.typeSymbol
      require(ts.tail forall (_.typeSymbol == sym), ts)
      for (p <- sym.typeParams ; in <- sym.typeParams ; if in.info.bounds contains p) yield
        p -> in
    }
  }

  // only called when strictInference
  private def willViolateRecursiveBounds(tp: Type, ts: List[Type], tsElimSub: List[Type]) = {
    val typeSym     = ts.head.typeSymbol // we're uniform, the `.head` is as good as any.
    def fbounds     = findRecursiveBounds(ts) map (_._2)
    def isRecursive = typeSym.typeParams exists fbounds.contains

    isRecursive && (transposeSafe(tsElimSub map (_.normalize.typeArgs)) match {
      case Some(arggsTransposed) =>
        val mergedTypeArgs = (tp match { case et: ExistentialType => et.underlying; case _ => tp}).typeArgs
        exists3(typeSym.typeParams, mergedTypeArgs, arggsTransposed) {
          (param, arg, lubbedArgs) =>
            val isExistential = arg.typeSymbol.isExistentiallyBound
            val isInFBound    = fbounds contains param
            val wasLubbed     = !lubbedArgs.exists(_ =:= arg)
            (!isExistential && isInFBound && wasLubbed)
        }
      case None => false
    })
  }

  /** Given a matrix `tsBts` whose columns are basetype sequences (and the symbols `tsParams` that should be interpreted as type parameters in this matrix),
    * compute its least sorted upwards closed upper bound relative to the following ordering <= between lists of types:
    *
    *    xs <= ys   iff   forall y in ys exists x in xs such that x <: y
    *
    *  @arg tsParams for each type in the original list of types `ts0`, its list of type parameters (if that type is a type constructor)
    *                (these type parameters may be referred to by type arguments in the BTS column of those types,
    *                and must be interpreted as bound variables; i.e., under a type lambda that wraps the types that refer to these type params)
    *  @arg tsBts    a matrix whose columns are basetype sequences
    *                the first row is the original list of types for which we're computing the lub
    *                  (except that type constructors have been applied to their dummyArgs)
    *  @See baseTypeSeq  for a definition of sorted and upwards closed.
    */
  def lubList(ts: List[Type], depth: Depth): List[Type] = {
    var lubListDepth = Depth.Zero
    // This catches some recursive situations which would otherwise
    // befuddle us, e.g. pos/hklub0.scala
    def isHotForTs(xs: List[Type]) = ts exists (_.typeParams == xs.map(_.typeSymbol))

    def elimHigherOrderTypeParam(tp: Type) = tp match {
      case TypeRef(_, _, args) if args.nonEmpty && isHotForTs(args) =>
        logResult("Retracting dummies from " + tp + " in lublist")(tp.typeConstructor)
      case _ => tp
    }
    // pretypes is a tail-recursion-preserving accumulator.
    @tailrec
    def loop(pretypes: List[Type], tsBts: List[List[Type]]): List[Type] = {
      lubListDepth = lubListDepth.incr

      if (tsBts.isEmpty || (tsBts exists typeListIsEmpty)) pretypes.reverse
      else if (tsBts.tail.isEmpty) pretypes.reverse ++ tsBts.head
      else {
        // ts0 is the 1-dimensional frontier of symbols cutting through 2-dimensional tsBts.
        // Invariant: all symbols "under" (closer to the first row) the frontier
        // are smaller (according to _.isLess) than the ones "on and beyond" the frontier
        val ts0 = tsBts map (_.head)

        // Is the frontier made up of types with the same symbol?
        val isUniformFrontier = (ts0: @unchecked) match {
          case t :: ts  => ts forall (_.typeSymbol == t.typeSymbol)
        }

        // Produce a single type for this frontier by merging the prefixes and arguments of those
        // typerefs that share the same symbol: that symbol is the current maximal symbol for which
        // the invariant holds, i.e., the one that conveys most information regarding subtyping. Before
        // merging, strip targs that refer to bound tparams (when we're computing the lub of type
        // constructors.) Also filter out all types that are a subtype of some other type.
        if (isUniformFrontier) {
          val tails = tsBts map (_.tail)
          val ts1   = elimSub(ts0, depth) map elimHigherOrderTypeParam
          mergePrefixAndArgs(ts1, Covariant, depth) match {
            case NoType => loop(pretypes, tails)
            case tp if strictInference && willViolateRecursiveBounds(tp, ts0, ts1) =>
              log(s"Breaking recursion in lublist, advancing frontier and discaring merged prefix/args from $tp")
              loop(pretypes, tails)
            case tp =>
              loop(tp :: pretypes, tails)
          }
        } else {
          // frontier is not uniform yet, move it beyond the current minimal symbol;
          // lather, rinse, repeat
          val sym    = minSym(ts0)
          val newtps = tsBts map (ts => if (ts.head.typeSymbol == sym) ts.tail else ts)
          if (printLubs) {
            val str = (newtps.zipWithIndex map { case (tps, idx) =>
              tps.map("        " + _ + "\n").mkString("   (" + idx + ")\n", "", "\n")
            }).mkString("")

            println("Frontier(\n" + str + ")")
            printLubMatrix((ts zip tsBts).toMap, lubListDepth)
          }

          loop(pretypes, newtps)
        }
      }
    }

    val initialBTSes = ts map (_.baseTypeSeq.toList)
    if (printLubs)
      printLubMatrix((ts zip initialBTSes).toMap, depth)

    loop(Nil, initialBTSes)
  }

  /** The minimal symbol of a list of types (as determined by `Symbol.isLess`). */
  private def minSym(tps: List[Type]): Symbol =
    (tps.head.typeSymbol /: tps.tail) {
      (sym1, tp2) => if (tp2.typeSymbol isLess sym1) tp2.typeSymbol else sym1
    }

  /** A minimal type list which has a given list of types as its base type sequence */
  def spanningTypes(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case first :: rest =>
      first :: spanningTypes(
        rest filter (t => !first.typeSymbol.isSubClass(t.typeSymbol)))
  }

  /** Eliminate from list of types all elements which are a supertype
    *  of some other element of the list. */
  private def elimSuper(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case List(t) => List(t)
    case t :: ts1 =>
      val rest = elimSuper(ts1 filter (t1 => !(t <:< t1)))
      if (rest exists (t1 => t1 <:< t)) rest else t :: rest
  }

  /** Eliminate from list of types all elements which are a subtype
    *  of some other element of the list. */
  private def elimSub(ts: List[Type], depth: Depth): List[Type] = {
    def elimSub0(ts: List[Type]): List[Type] = ts match {
      case List() => List()
      case List(t) => List(t)
      case t :: ts1 =>
        val rest = elimSub0(ts1 filter (t1 => !isSubType(t1, t, depth.decr)))
        if (rest exists (t1 => isSubType(t, t1, depth.decr))) rest else t :: rest
    }
    val ts0 = elimSub0(ts)
    if (ts0.isEmpty || ts0.tail.isEmpty) ts0
    else {
      val ts1 = ts0 mapConserve (t => elimAnonymousClass(t.dealiasWiden))
      if (ts1 eq ts0) ts0
      else elimSub(ts1, depth)
    }
  }

  /** Does this set of types have the same weak lub as
   *  it does regular lub? This is exposed so lub callers
   *  can discover whether the trees they are typing will
   *  may require further adaptation. It may return false
   *  negatives, but it will not return false positives.
   */
  def sameWeakLubAsLub(tps: List[Type]) = tps match {
    case Nil       => true
    case tp :: Nil => !typeHasAnnotations(tp)
    case tps       => !(tps exists typeHasAnnotations) && !(tps forall isNumericValueType)
  }

  /** If the arguments are all numeric value types, the numeric
   *  lub according to the weak conformance spec. If any argument
   *  has type annotations, take the lub of the unannotated type
   *  and call the analyzerPlugin method annotationsLub so it can
   *  be further altered. Otherwise, the regular lub.
   */
  def weakLub(tps: List[Type]): Type = (
    if (tps.isEmpty)
      NothingTpe
    else if (tps forall isNumericValueType)
      numericLub(tps)
    else if (tps exists typeHasAnnotations)
      annotationsLub(lub(tps map (_.withoutAnnotations)), tps)
    else
      lub(tps)
  )

  def numericLub(ts: List[Type]) =
    ts reduceLeft ((t1, t2) =>
      if (isNumericSubType(t1, t2)) t2
      else if (isNumericSubType(t2, t1)) t1
      else IntTpe)

  private val _lubResults = new mutable.HashMap[(Depth, List[Type]), Type]
  def lubResults = _lubResults

  private val _glbResults = new mutable.HashMap[(Depth, List[Type]), Type]
  def glbResults = _glbResults

  def lub(ts: List[Type]): Type = ts match {
    case Nil      => NothingTpe
    case t :: Nil => t
    case _        =>
      if (Statistics.canEnable) Statistics.incCounter(lubCount)
      val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, lubNanos) else null
      try {
        val res = lub(ts, lubDepth(ts))
        // If the number of unapplied type parameters in all incoming
        // types is consistent, and the lub does not match that, return
        // the type constructor of the calculated lub instead.  This
        // is because lubbing type constructors tends to result in types
        // which have been applied to dummies or Nothing.
        ts.map(_.typeParams.size).distinct match {
          case x :: Nil if res.typeParams.size != x =>
            logResult(s"Stripping type args from lub because $res is not consistent with $ts")(res.typeConstructor)
          case _                                    =>
            res
        }
      }
      finally {
        lubResults.clear()
        glbResults.clear()
        if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
      }
  }

  /** The least upper bound wrt <:< of a list of types */
  protected[internal] def lub(ts: List[Type], depth: Depth): Type = {
    def lub0(ts0: List[Type]): Type = elimSub(ts0, depth) match {
      case List() => NothingTpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        val tparams1 = map2(tparams, matchingBounds(ts, tparams).transpose)((tparam, bounds) =>
          tparam.cloneSymbol.setInfo(glb(bounds, depth)))
        PolyType(tparams1, lub0(matchingInstTypes(ts, tparams1)))
      case ts @ (mt @ MethodType(params, _)) :: rest =>
        MethodType(params, lub0(matchingRestypes(ts, mt.paramTypes)))
      case ts @ NullaryMethodType(_) :: rest =>
        NullaryMethodType(lub0(matchingRestypes(ts, Nil)))
      case ts @ TypeBounds(_, _) :: rest =>
        TypeBounds(glb(ts map (_.bounds.lo), depth), lub(ts map (_.bounds.hi), depth))
      case ts @ AnnotatedType(annots, tpe) :: rest =>
        annotationsLub(lub0(ts map (_.withoutAnnotations)), ts)
      case ts =>
        lubResults get ((depth, ts)) match {
          case Some(lubType) =>
            lubType
          case None =>
            lubResults((depth, ts)) = AnyTpe
            val res = if (depth.isNegative) AnyTpe else lub1(ts)
            lubResults((depth, ts)) = res
            res
        }
    }
    def lub1(ts0: List[Type]): Type = {
      val (ts, tparams)            = stripExistentialsAndTypeVars(ts0)
      val lubBaseTypes: List[Type] = lubList(ts, depth)
      val lubParents               = spanningTypes(lubBaseTypes)
      val lubOwner                 = commonOwner(ts)
      val lubBase                  = intersectionType(lubParents, lubOwner)
      val lubType =
        if (phase.erasedTypes || depth.isZero ) lubBase
        else {
          val lubRefined  = refinedType(lubParents, lubOwner)
          val lubThisType = lubRefined.typeSymbol.thisType
          val narrowts    = ts map (_.narrow)
          def excludeFromLub(sym: Symbol) = (
            sym.isClass
              || sym.isConstructor
              || !sym.isPublic
              || isGetClass(sym)
              || sym.isFinal
              || narrowts.exists(t => !refines(t, sym))
            )
          def lubsym(proto: Symbol): Symbol = {
            val prototp = lubThisType.memberInfo(proto)
            val syms = narrowts map (t =>
              // SI-7602 With erroneous code, we could end up with overloaded symbols after filtering
              //         so `suchThat` unsuitable.
              t.nonPrivateMember(proto.name).filter(sym =>
                sym.tpe matches prototp.substThis(lubThisType.typeSymbol, t)))

            if (syms contains NoSymbol) NoSymbol
            else {
              val symtypes =
                map2(narrowts, syms)((t, sym) => t.memberInfo(sym).substThis(t.typeSymbol, lubThisType))
              if (proto.isTerm) // possible problem: owner of info is still the old one, instead of new refinement class
                proto.cloneSymbol(lubRefined.typeSymbol).setInfoOwnerAdjusted(lub(symtypes, depth.decr))
              else if (symtypes.tail forall (symtypes.head =:= _))
                proto.cloneSymbol(lubRefined.typeSymbol).setInfoOwnerAdjusted(symtypes.head)
              else {
                def lubBounds(bnds: List[TypeBounds]): TypeBounds =
                  TypeBounds(glb(bnds map (_.lo), depth.decr), lub(bnds map (_.hi), depth.decr))
                lubRefined.typeSymbol.newAbstractType(proto.name.toTypeName, proto.pos)
                  .setInfoOwnerAdjusted(lubBounds(symtypes map (_.bounds)))
              }
            }
          }
          def refines(tp: Type, sym: Symbol): Boolean = {
            val syms = tp.nonPrivateMember(sym.name).alternatives
            !syms.isEmpty && (syms forall (alt =>
            // todo alt != sym is strictly speaking not correct, but without it we lose
            // efficiency.
              alt != sym && !specializesSym(lubThisType, sym, tp, alt, depth)))
          }
          // add a refinement symbol for all non-class members of lubBase
          // which are refined by every type in ts.
          for (sym <- lubBase.nonPrivateMembers ; if !excludeFromLub(sym)) {
            try lubsym(sym) andAlso (addMember(lubThisType, lubRefined, _, depth))
            catch {
              case ex: NoCommonType =>
            }
          }
          if (lubRefined.decls.isEmpty) lubBase
          else if (!verifyLubs) lubRefined
          else {
            // Verify that every given type conforms to the calculated lub.
            // In theory this should not be necessary, but higher-order type
            // parameters are not handled correctly.
            val ok = ts forall { t =>
              isSubType(t, lubRefined, depth) || {
                if (settings.debug || printLubs) {
                  Console.println(
                    "Malformed lub: " + lubRefined + "\n" +
                      "Argument " + t + " does not conform.  Falling back to " + lubBase
                  )
                }
                false
              }
            }
            // If not, fall back on the more conservative calculation.
            if (ok) lubRefined
            else lubBase
          }
        }
      // dropIllegalStarTypes is a localized fix for SI-6897. We should probably
      // integrate that transformation at a lower level in master, but lubs are
      // the likely and maybe only spot they escape, so fixing here for 2.10.1.
      existentialAbstraction(tparams, dropIllegalStarTypes(lubType))
    }
    if (printLubs) {
      println(indent + "lub of " + ts + " at depth "+depth)//debug
      indent = indent + "  "
      assert(indent.length <= 100)
    }
    if (Statistics.canEnable) Statistics.incCounter(nestedLubCount)
    val res = lub0(ts)
    if (printLubs) {
      indent = indent stripSuffix "  "
      println(indent + "lub of " + ts + " is " + res)//debug
    }
    res
  }

  val GlbFailure = new Throwable

  /** A global counter for glb calls in the `specializes` query connected to the `addMembers`
    *  call in `glb`. There's a possible infinite recursion when `specializes` calls
    *  memberType, which calls baseTypeSeq, which calls mergePrefixAndArgs, which calls glb.
    *  The counter breaks this recursion after two calls.
    *  If the recursion is broken, no member is added to the glb.
    */
  private var globalGlbDepth = Depth.Zero
  private final val globalGlbLimit = Depth(2)

  /** The greatest lower bound of a list of types (as determined by `<:<`). */
  def glb(ts: List[Type]): Type = elimSuper(ts) match {
    case List() => AnyTpe
    case List(t) => t
    case ts0 =>
      if (Statistics.canEnable) Statistics.incCounter(lubCount)
      val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, lubNanos) else null
      try {
        glbNorm(ts0, lubDepth(ts0))
      } finally {
        lubResults.clear()
        glbResults.clear()
        if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
      }
  }

  protected[internal] def glb(ts: List[Type], depth: Depth): Type = elimSuper(ts) match {
    case List() => AnyTpe
    case List(t) => t
    case ts0 => glbNorm(ts0, depth)
  }

  /** The greatest lower bound of a list of types (as determined by `<:<`), which have been normalized
    *  with regard to `elimSuper`. */
  protected def glbNorm(ts: List[Type], depth: Depth): Type = {
    def glb0(ts0: List[Type]): Type = ts0 match {
      case List() => AnyTpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        val tparams1 = map2(tparams, matchingBounds(ts, tparams).transpose)((tparam, bounds) =>
          tparam.cloneSymbol.setInfo(lub(bounds, depth)))
        PolyType(tparams1, glbNorm(matchingInstTypes(ts, tparams1), depth))
      case ts @ (mt @ MethodType(params, _)) :: rest =>
        MethodType(params, glbNorm(matchingRestypes(ts, mt.paramTypes), depth))
      case ts @ NullaryMethodType(_) :: rest =>
        NullaryMethodType(glbNorm(matchingRestypes(ts, Nil), depth))
      case ts @ TypeBounds(_, _) :: rest =>
        TypeBounds(lub(ts map (_.bounds.lo), depth), glb(ts map (_.bounds.hi), depth))
      case ts =>
        glbResults get ((depth, ts)) match {
          case Some(glbType) =>
            glbType
          case _ =>
            glbResults((depth, ts)) = NothingTpe
            val res = if (depth.isNegative) NothingTpe else glb1(ts)
            glbResults((depth, ts)) = res
            res
        }
    }
    def glb1(ts0: List[Type]): Type = {
      try {
        val (ts, tparams) = stripExistentialsAndTypeVars(ts0)
        val glbOwner = commonOwner(ts)
        def refinedToParents(t: Type): List[Type] = t match {
          case RefinedType(ps, _) => ps flatMap refinedToParents
          case _ => List(t)
        }
        def refinedToDecls(t: Type): List[Scope] = t match {
          case RefinedType(ps, decls) =>
            val dss = ps flatMap refinedToDecls
            if (decls.isEmpty) dss else decls :: dss
          case _ => List()
        }
        val ts1 = ts flatMap refinedToParents
        val glbBase = intersectionType(ts1, glbOwner)
        val glbType =
          if (phase.erasedTypes || depth.isZero) glbBase
          else {
            val glbRefined = refinedType(ts1, glbOwner)
            val glbThisType = glbRefined.typeSymbol.thisType
            def glbsym(proto: Symbol): Symbol = {
              val prototp = glbThisType.memberInfo(proto)
              val syms = for (t <- ts;
                              alt <- (t.nonPrivateMember(proto.name).alternatives)
                              if glbThisType.memberInfo(alt) matches prototp
              ) yield alt
              val symtypes = syms map glbThisType.memberInfo
              assert(!symtypes.isEmpty)
              proto.cloneSymbol(glbRefined.typeSymbol).setInfoOwnerAdjusted(
                if (proto.isTerm) glb(symtypes, depth.decr)
                else {
                  def isTypeBound(tp: Type) = tp match {
                    case TypeBounds(_, _) => true
                    case _ => false
                  }
                  def glbBounds(bnds: List[Type]): TypeBounds = {
                    val lo = lub(bnds map (_.bounds.lo), depth.decr)
                    val hi = glb(bnds map (_.bounds.hi), depth.decr)
                    if (lo <:< hi) TypeBounds(lo, hi)
                    else throw GlbFailure
                  }
                  val symbounds = symtypes filter isTypeBound
                  var result: Type =
                    if (symbounds.isEmpty)
                      TypeBounds.empty
                    else glbBounds(symbounds)
                  for (t <- symtypes if !isTypeBound(t))
                    if (result.bounds containsType t) result = t
                    else throw GlbFailure
                  result
                })
            }
            if (globalGlbDepth < globalGlbLimit)
              try {
                globalGlbDepth = globalGlbDepth.incr
                val dss = ts flatMap refinedToDecls
                for (ds <- dss; sym <- ds.iterator)
                  if (globalGlbDepth < globalGlbLimit && !specializesSym(glbThisType, sym, depth))
                    try {
                      addMember(glbThisType, glbRefined, glbsym(sym), depth)
                    } catch {
                      case ex: NoCommonType =>
                    }
              } finally {
                globalGlbDepth = globalGlbDepth.decr
              }
            if (glbRefined.decls.isEmpty) glbBase else glbRefined
          }
        existentialAbstraction(tparams, glbType)
      } catch {
        case GlbFailure =>
          if (ts forall (t => NullTpe <:< t)) NullTpe
          else NothingTpe
      }
    }
    // if (settings.debug.value) { println(indent + "glb of " + ts + " at depth "+depth); indent = indent + "  " } //DEBUG
    if (Statistics.canEnable) Statistics.incCounter(nestedLubCount)
    glb0(ts)
    // if (settings.debug.value) { indent = indent.substring(0, indent.length() - 2); log(indent + "glb of " + ts + " is " + res) }//DEBUG
  }

  /** All types in list must be polytypes with type parameter lists of
    *  same length as tparams.
    *  Returns list of list of bounds infos, where corresponding type
    *  parameters are renamed to tparams.
    */
  private def matchingBounds(tps: List[Type], tparams: List[Symbol]): List[List[Type]] = {
    def getBounds(tp: Type): List[Type] = tp match {
      case PolyType(tparams1, _) if sameLength(tparams1, tparams) =>
        tparams1 map (tparam => tparam.info.substSym(tparams1, tparams))
      case tp =>
        if (tp ne tp.normalize) getBounds(tp.normalize)
        else throw new NoCommonType(tps)
    }
    tps map getBounds
  }

  /** All types in list must be polytypes with type parameter lists of
    *  same length as tparams.
    *  Returns list of instance types, where corresponding type
    *  parameters are renamed to tparams.
    */
  private def matchingInstTypes(tps: List[Type], tparams: List[Symbol]): List[Type] = {
    def transformResultType(tp: Type): Type = tp match {
      case PolyType(tparams1, restpe) if sameLength(tparams1, tparams) =>
        restpe.substSym(tparams1, tparams)
      case tp =>
        if (tp ne tp.normalize) transformResultType(tp.normalize)
        else throw new NoCommonType(tps)
    }
    tps map transformResultType
  }

  /** All types in list must be method types with equal parameter types.
    *  Returns list of their result types.
    */
  private def matchingRestypes(tps: List[Type], pts: List[Type]): List[Type] =
    tps map {
      case mt @ MethodType(params1, res) if isSameTypes(mt.paramTypes, pts) =>
        res
      case NullaryMethodType(res) if pts.isEmpty =>
        res
      case _ =>
        throw new NoCommonType(tps)
    }
}
