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

import scala.collection.mutable, mutable.ListBuffer
import scala.annotation.tailrec
import Variance._

private[internal] trait GlbLubs {
  self: SymbolTable =>

  import definitions._
  import statistics._

  private final val printLubs = System.getProperty("scalac.debug.lub") != null

  /** In case anyone wants to turn off lub verification without reverting anything. */
  private final val verifyLubs = true

  private def printLubMatrix(btsMap: Map[Type, List[Type]], depth: Depth): Unit = {
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

  /** Given a matrix `tsBts` whose columns are basetype sequences (and the symbols `tsParams` that should be interpreted as type parameters in this matrix),
    * compute its least sorted upwards closed upper bound relative to the following ordering <= between lists of types:
    *
    *    xs <= ys   iff   forall y in ys exists x in xs such that x <: y
    *
    *  @param tsParams for each type in the original list of types `ts0`, its list of type parameters (if that type is a type constructor)
    *                (these type parameters may be referred to by type arguments in the BTS column of those types,
    *                and must be interpreted as bound variables; i.e., under a type lambda that wraps the types that refer to these type params)
    *  @param tsBts    a matrix whose columns are basetype sequences
    *                the first row is the original list of types for which we're computing the lub
    *                  (except that type constructors have been applied to their dummyArgs)
    *  @see baseTypeSeq  for a definition of sorted and upwards closed.
    */
  def lubList(ts: List[Type], depth: Depth): List[Type] = ts match {
    case Nil => Nil
    case ty :: Nil => ty.baseTypeSeq.toList
    case _ => lubList_x(ts, depth)
  }

  private[this] def lubList_x(ts: List[Type], depth: Depth): List[Type] = {
    var lubListDepth = Depth.Zero
    // This catches some recursive situations which would otherwise
    // befuddle us, e.g. pos/hklub0.scala
    def isHotForT(tyPar: Symbol, x: Type): Boolean = tyPar eq x.typeSymbol
    def isHotForTs(xs: List[Type]) = ts.exists(_.typeParams.corresponds(xs)(isHotForT(_,_)))

    def elimHigherOrderTypeParam(tp: Type) = tp match {
      case TypeRef(_, _, args) if args.nonEmpty && isHotForTs(args) =>
        logResult("Retracting dummies from " + tp + " in lublist")(tp.typeConstructor)
      case _ => tp
    }

    val baseTypeSeqs: Array[BaseTypeSeq] = mapToArray(ts)(_.baseTypeSeq)
    val ices: Array[Int] = new Array[Int](baseTypeSeqs.length)

    def printLubMatrixAux(depth: Depth): Unit = {
      val btsMap: Map[Type, List[Type]] = ts.zipWithIndex.map {
        case (ty, ix) => ty -> baseTypeSeqs(ix).toList.drop(ices(ix))
      }.toMap
      printLubMatrix(btsMap, depth)
    }

    def headOf(ix: Int) = baseTypeSeqs(ix).rawElem(ices(ix))

    val pretypes: ListBuffer[Type] = ListBuffer.empty[Type]

    var isFinished = false
    while (!isFinished && ices(0) < baseTypeSeqs(0).length) {
      lubListDepth = lubListDepth.incr
      // Step 1: run through the List with these variables:
      // 1) Is there any empty list? Are they equal or are we taking the smallest?
      // isFinished: tsBts.exists(typeListIsEmpty)
      // Is the frontier made up of types with the same symbol?
      var isUniformFrontier = true
      var sym = headOf(0).typeSymbol
      // var tsYs = tsBts
      var ix = 0
      while (!isFinished && ix < baseTypeSeqs.length) {
        if (ices(ix) == baseTypeSeqs(ix).length)
          isFinished = true
        else {
          val btySym = headOf(ix).typeSymbol
          isUniformFrontier = isUniformFrontier && (sym eq btySym)
          if (btySym isLess sym)
            sym = btySym
        }
        ix += 1
      }
      // Produce a single type for this frontier by merging the prefixes and arguments of those
      // typerefs that share the same symbol: that symbol is the current maximal symbol for which
      // the invariant holds, i.e., the one that conveys most information regarding subtyping. Before
      // merging, strip targs that refer to bound tparams (when we're computing the lub of type
      // constructors.) Also filter out all types that are a subtype of some other type.
      if (!isFinished) {
        // ts0 is the 1-dimensional frontier of symbols cutting through 2-dimensional tsBts.
        // Invariant: all symbols "under" (closer to the first row) the frontier
        // are smaller (according to _.isLess) than the ones "on and beyond" the frontier
        val ts0 = {
          var ys: List[Type] = Nil
          var kx = baseTypeSeqs.length
          while (kx > 0){
            kx -= 1
            ys = headOf(kx) :: ys
          }
          ys
        }

        if (isUniformFrontier) {
          val ts1 = elimSub(ts0, depth).map(elimHigherOrderTypeParam)
          mergePrefixAndArgs(ts1, Covariant, depth) match {
            case NoType =>
            case tp => pretypes += tp
          }
          var jx = 0
          while (jx < baseTypeSeqs.length){
            ices(jx) += 1
            jx += 1
          }
        } else {
          // frontier is not uniform yet, move it beyond the current minimal symbol;
          // lather, rinse, repeat
          var jx = 0
          while (jx < baseTypeSeqs.length){
            if (headOf(jx).typeSymbol == sym)
              ices(jx) += 1
            jx += 1
          }
          if (printLubs) {
            println {
              baseTypeSeqs.zipWithIndex.map { case (tps, idx) =>
                tps.toList.drop(ices(idx)).map("        " + _).mkString("   (" + idx + ")\n", "\n", "\n")
              }
              .mkString("Frontier(\n", "", ")")
            }
            printLubMatrixAux(lubListDepth)
          }
        }
      }
    }

    if (printLubs)
      printLubMatrixAux(depth)

    pretypes.toList
  }

  /** A minimal type list which has a given list of types as its base type sequence */
  def spanningTypes(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case first :: rest =>
      first :: spanningTypes(
        rest filter (t => !first.typeSymbol.isSubClass(t.typeSymbol)))
  }

  // OPT: hoist allocation of the collector and lambda out of the loop in partition
  private val isWildCardOrNonGroundTypeVarCollector = new FindTypeCollector( {
    case tv: TypeVar => !tv.isGround
    case t => t.isWildcard
  })

  /** From a list of types, retain only maximal types as determined by the partial order `po`. */
  private def maxTypes(ts: List[Type])(po: (Type, Type) => Boolean): List[Type] = {
    def stacked(ts: List[Type]): List[Type] = ts match {
      case t :: ts1 =>
        val ts2 = stacked(ts1.filterNot(po(_, t)))
        if (ts2.exists(po(t, _))) ts2 else t :: ts2
      case Nil => Nil
    }

    // loop thru tails, filtering for survivors of po test with the current element, which is saved for later culling
    @tailrec
    def loop(survivors: List[Type], toCull: List[Type]): List[Type] = survivors match {
      case h :: rest =>
        loop(rest.filterNot(po(_, h)), h :: toCull)
      case _ =>
        // unwind the stack of saved elements, accumulating a result containing elements surviving po (in swapped order)
        def sieve(res: List[Type], remaining: List[Type]): List[Type] = remaining match {
          case h :: tail =>
            val res1 = if (res.exists(po(h, _))) res else h :: res
            sieve(res1, tail)
          case _ => res
        }
        toCull match {
          case _ :: Nil => toCull
          case _ => sieve(Nil, toCull)
        }
    }

    // The order here matters because type variables and wildcards can act both as subtypes and supertypes.
    val sorted = {
      val (wilds, ts1) = partitionConserve(ts)(isWildCardOrNonGroundTypeVarCollector.collect(_).isDefined)
      ts1 ::: wilds
    }
    if (sorted.lengthCompare(5) > 0) loop(sorted, Nil)
    else stacked(sorted)
  }

  /** Eliminate from list of types all elements which are a supertype
   *  of some other element of the list. */
  private def elimSuper(ts: List[Type]): List[Type] =
    if (ts.lengthCompare(1) <= 0) ts
    else maxTypes(ts)((t1, t2) => t2 <:< t1)

  /** Eliminate from list of types all elements which are a subtype
   *  of some other element of the list. */
  @tailrec private def elimSub(ts: List[Type], depth: Depth): List[Type] =
    if (ts.lengthCompare(1) <= 0) ts else {
      val ts1 = maxTypes(ts)(isSubType(_, _, depth.decr))
      if (ts1.lengthCompare(1) <= 0) ts1 else {
        val ts2 = ts1.mapConserve(t => elimAnonymousClass(t.dealiasWiden))
        if (ts1 eq ts2) ts1 else elimSub(ts2, depth)
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
    case tp :: Nil => tp.annotations.isEmpty
    case tps       => tps.forall(_.annotations.isEmpty) && !(tps forall isNumericValueType)
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
    else if (tps.exists(!_.annotations.isEmpty))
      annotationsLub(lub(tps map (_.withoutAnnotations)), tps)
    else
      lub(tps)
  )

  // Need to widen result when using isNumericSubType to compare.
  // isNumericSubType considers types after dealiasWiden, so should perform same transform on return.
  // Example unit test: `numericLub(0, 1) == Int` (without the dealiasWiden one of the types would be returned as-is...)
  def numericLub(ts: List[Type]) =
    ts reduceLeft ((t1, t2) =>
      if (isNumericSubType(t1, t2)) t2.dealiasWiden
      else if (isNumericSubType(t2, t1)) t1.dealiasWiden
      else IntTpe)

  private[this] val _lubResults = new mutable.HashMap[(Depth, List[Type]), Type]
  def lubResults = _lubResults

  private[this] val _glbResults = new mutable.HashMap[(Depth, List[Type]), Type]
  def glbResults = _glbResults

  def lub(ts: List[Type]): Type = ts match {
    case Nil      => NothingTpe
    case t :: Nil => t
    case _        =>
      if (settings.areStatisticsEnabled) statistics.incCounter(lubCount)
      val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, lubNanos) else null
      try {
        val res = lub(ts, lubDepth(ts))
        // If the number of unapplied type parameters in all incoming
        // types is consistent, and the lub does not match that, return
        // the type constructor of the calculated lub instead.  This
        // is because lubbing type constructors tends to result in types
        // which have been applied to dummies or Nothing.
        val rtps = res.typeParams.size
        val hs = ts.head.typeParams.size
        if (hs != rtps && ts.forall(_.typeParams.size == hs))
          logResult(s"Stripping type args from lub because $res is not consistent with $ts")(res.typeConstructor)
        else
          res
      }
      finally {
        lubResults.clear()
        glbResults.clear()
        if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
      }
  }

  /** The least upper bound wrt <:< of a list of types */
  protected[internal] def lub(ts0: List[Type], depth: Depth): Type = {
    def lub0(ts0: List[Type]): Type = elimSub(ts0, depth) match {
      case List() => NothingTpe
      case List(t) => t
      case (pt @ PolyType(_, _)) :: rest =>
        polyTypeMatch(pt, rest, depth, glb, lub0)
      case ts @ (mt @ MethodType(params, _)) :: rest =>
        MethodType(params, lub0(matchingRestypes(ts, mt.paramTypes)))
      case ts @ NullaryMethodType(_) :: rest =>
        NullaryMethodType(lub0(matchingRestypes(ts, Nil)))
      case ts @ TypeBounds(_, _) :: rest =>
        TypeBounds(glb(ts map (_.lowerBound), depth), lub(ts map (_.upperBound), depth))
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
              // scala/bug#7602 With erroneous code, we could end up with overloaded symbols after filtering
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
                val lubBs = TypeBounds(
                  glb(symtypes.map(_.lowerBound), depth.decr),
                  lub(symtypes.map(_.upperBound), depth.decr)
                )
                lubRefined.typeSymbol.newAbstractType(proto.name.toTypeName, proto.pos)
                  .setInfoOwnerAdjusted(lubBs)
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
              isSubType(t, lubRefined, depth.decr) || {
                if (settings.isDebug || printLubs) {
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
      // dropIllegalStarTypes is a localized fix for scala/bug#6897. We should probably
      // integrate that transformation at a lower level in master, but lubs are
      // the likely and maybe only spot they escape, so fixing here for 2.10.1.
      existentialAbstraction(tparams, dropIllegalStarTypes(lubType))
    }
    if (printLubs) {
      println(indent + "lub of " + ts0 + " at depth "+depth)//debug
      indent = indent + "  "
      assert(indent.length <= 100, "LUB is highly indented")
    }
    if (settings.areStatisticsEnabled) statistics.incCounter(nestedLubCount)
    val res = lub0(ts0)
    if (printLubs) {
      indent = indent stripSuffix "  "
      println(indent + "lub of " + ts0 + " is " + res)//debug
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
  private[this] var globalGlbDepth = Depth.Zero
  private final val globalGlbLimit = Depth(2)

  /** The greatest lower bound of a list of types (as determined by `<:<`). */
  def glb(ts: List[Type]): Type = elimSuper(ts) match {
    case List() => AnyTpe
    case List(t) => t
    case ts0 =>
      if (settings.areStatisticsEnabled) statistics.incCounter(lubCount)
      val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, lubNanos) else null
      try {
        glbNorm(ts0, lubDepth(ts0))
      } finally {
        lubResults.clear()
        glbResults.clear()
        if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
      }
  }

  protected[internal] def glb(ts: List[Type], depth: Depth): Type = elimSuper(ts) match {
    case List() => AnyTpe
    case List(t) => t
    case ts0 => glbNorm(ts0, depth)
  }

  /** The greatest lower bound of a list of types (as determined by `<:<`), which have been normalized
    *  with regard to `elimSuper`. */
  protected def glbNorm(ts0: List[Type], depth: Depth): Type = {
    def glb0(ts0: List[Type]): Type = ts0 match {
      case List() => AnyTpe
      case List(t) => t
      case (pt @ PolyType(_, _)) :: rest =>
        polyTypeMatch(pt, rest, depth, lub, glb0)
      case ts @ (mt @ MethodType(params, _)) :: rest =>
        MethodType(params, glbNorm(matchingRestypes(ts, mt.paramTypes), depth))
      case ts @ NullaryMethodType(_) :: rest =>
        NullaryMethodType(glbNorm(matchingRestypes(ts, Nil), depth))
      case ts @ TypeBounds(_, _) :: rest =>
        TypeBounds(lub(ts map (_.lowerBound), depth), glb(ts map (_.upperBound), depth))
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
        val ts1 = {
          val res = ListBuffer.empty[Type]
          def loop(ty: Type): Unit = ty match {
            case RefinedType(ps, _) => ps.foreach(loop)
            case _ => res += ty
          }
          ts foreach loop
          res.toList
        }
        val glbType =
          if (phase.erasedTypes || depth.isZero)
            intersectionType(ts1, glbOwner)
          else {
            val glbRefined = refinedType(ts1, glbOwner)
            val glbThisType = glbRefined.typeSymbol.thisType
            def glbsym(proto: Symbol): Symbol = {
              val prototp = glbThisType.memberInfo(proto)
              val symtypes: List[Type] = {
                val res = ListBuffer.empty[Type]
                ts foreach { t =>
                  t.nonPrivateMember(proto.name).alternatives foreach { alt =>
                    val mi = glbThisType.memberInfo(alt)
                    if (mi matches prototp)
                      res += mi
                  }
                }
                res.toList
              }
              assert(!symtypes.isEmpty, "No types for GLB")
              proto.cloneSymbol(glbRefined.typeSymbol).setInfoOwnerAdjusted(
                if (proto.isTerm) glb(symtypes, depth.decr)
                else {
                  def isTypeBound(tp: Type) = tp match {
                    case TypeBounds(_, _) => true
                    case _ => false
                  }
                  def glbBounds(bnds: List[Type]): TypeBounds = {
                    val lo = lub(bnds map (_.lowerBound), depth.decr)
                    val hi = glb(bnds map (_.upperBound), depth.decr)
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
                def foreachRefinedDecls(ty: Type): Unit = ty match {
                  case RefinedType(ps, decls) =>
                    ps foreach foreachRefinedDecls
                    if (! decls.isEmpty)
                      decls.iterator.foreach { sym =>
                        if (globalGlbDepth < globalGlbLimit && !specializesSym(glbThisType, sym, depth))
                          try {
                            addMember(glbThisType, glbRefined, glbsym(sym), depth)
                          } catch {
                            case ex: NoCommonType =>
                          }
                      }
                  case _ =>
                }
                ts foreach foreachRefinedDecls
              } finally {
                globalGlbDepth = globalGlbDepth.decr
              }
            if (glbRefined.decls.isEmpty) intersectionType(ts1, glbOwner) else glbRefined
          }
        existentialAbstraction(tparams, glbType)
      } catch {
        case GlbFailure =>
          if (ts0.forall(NullTpe <:< _)) NullTpe
          else NothingTpe
      }
    }
    // if (settings.debug.value) { println(indent + "glb of " + ts + " at depth "+depth); indent = indent + "  " } //DEBUG
    if (settings.areStatisticsEnabled) statistics.incCounter(nestedLubCount)
    glb0(ts0)
    // if (settings.debug.value) { indent = indent.substring(0, indent.length() - 2); log(indent + "glb of " + ts + " is " + res) }//DEBUG
  }

  /** All types in list must be polytypes with type parameter lists of
    *  same length as tparams.
    *  Returns list of list of bounds infos, where corresponding type
    *  parameters are renamed to tparams.
    */
  private def polyTypeMatch(
    ptHead: PolyType,
    ptRest: List[Type],
    depth: Depth,
    infoBoundTop: (List[Type], Depth) => Type,
    resultTypeBottom: List[Type] => Type
  ): PolyType = {
    val tparamsHead: List[Symbol] = ptHead.typeParams

    @tailrec
    def normalizeIter(ty: Type): PolyType = ty match {
      case pt @ PolyType(typeParams, _) if sameLength(typeParams, tparamsHead) => pt
      case tp =>
        val tpn = tp.normalize
        if (tp ne tpn) normalizeIter(tpn) else throw new NoCommonType(ptHead :: ptRest)
    }

    // Since ptHead = PolyType(tparamsHead, _), no need to normalize it or unify tparams
    val ntps: List[PolyType]   = ptHead :: ptRest.map(normalizeIter)

    val tparams1: List[Symbol] = {
      def unifyBounds(ntp: PolyType): List[Type] = {
        val tparams1 = ntp.typeParams
        tparams1 map (tparam => tparam.info.substSym(tparams1, tparamsHead))
      }
      val boundsTts : List[List[Type]] = ntps.tail.map(unifyBounds).transpose
      map2(tparamsHead, boundsTts){ (tparam, bounds) =>
        tparam.cloneSymbol.setInfo(infoBoundTop(tparam.info :: bounds, depth))
      }
    }
    // Do we also need to apply substSym(typeParams, tparams1) to ptHead.resultType ??
    val matchingInstTypes: List[Type] = ntps.map { ntp =>
      ntp.resultType.substSym(ntp.typeParams, tparams1)
    }

    PolyType(tparams1, resultTypeBottom(matchingInstTypes))
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
