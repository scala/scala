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
import util.ReusableInstance
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
  private[this] val isWildCardOrNonGroundTypeVar: Type => Boolean = {
    case tv: TypeVar => !tv.isGround
    case t => t.isWildcard
  }
  private[this] val isWildCardOrNonGroundTypeVarCollectorInstance: ReusableInstance[FindTypeCollector] =
    ReusableInstance(new FindTypeCollector(isWildCardOrNonGroundTypeVar), enabled = isCompilerUniverse)

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
      val (wilds, ts1) = isWildCardOrNonGroundTypeVarCollectorInstance.using { collector =>
        partitionConserve(ts)(collector.collect(_).isDefined)
      }
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

  /** Memo tables and recursion guards for one outermost `lub` or `glb`. Entries are only
   *  valid within that call: they may be in-progress placeholders (`Any` / `Nothing`) and
   *  were computed at the current phase.
   */
  final class LubGlbCaches(val outer: LubGlbCaches) {
    val lubResults = new mutable.HashMap[(Depth, List[Type]), Type]
    val glbResults = new mutable.HashMap[(Depth, List[Type]), Type]
    var inUse = false
    /** The arguments of the top-level `lub` (`ownerIsLub`) or `glb` that claimed these caches,
     *  `null` if a call with explicit depth claimed them.
     */
    var ownerTs: List[Type] = null
    var ownerIsLub = false
    def clear(): Unit = { lubResults.clear(); glbResults.clear() }
    /** How many enclosing top-level calls, including the owner, compute this `lub` or `glb`. */
    def inProgress(isLub: Boolean, ts: List[Type]): Int = {
      val here = if (ownerIsLub == isLub && ownerTs == ts) 1 else 0
      if (outer eq null) here else here + outer.inProgress(isLub, ts)
    }
  }
  private[this] var _lubGlbCaches = new LubGlbCaches(null)
  def lubGlbCaches: LubGlbCaches = _lubGlbCaches
  def lubGlbCaches_=(caches: LubGlbCaches): Unit = _lubGlbCaches = caches

  def lubResults = lubGlbCaches.lubResults
  def glbResults = lubGlbCaches.glbResults

  /** Run a top-level `lub` or `glb` with caches private to it.
   *
   *  A top-level `lub`/`glb` can be re-entered while another one is in progress, e.g. when
   *  forcing the info of a member runs a lazy type completer that typechecks code. The nested
   *  call must neither observe the outer call's recursion guards (it would return `Any` or
   *  `Nothing`) nor discard them when it finishes, so it gets fresh caches.
   *
   *  A call already in progress twice is taken to be a cycle, e.g. through `solve`, and shares
   *  the current caches so that their recursion guards stop it.
   */
  private def withLubGlbCaches[T](isLub: Boolean, ts: List[Type])(body: => T): T = {
    // Single-threaded only: `inUse` and the maps are plain fields, so a holder must never be
    // visible to two threads. The compiler universe is single-threaded; runtime reflection
    // (`SynchronizedTypes`) overrides `lubGlbCaches` with thread-local storage, so each thread
    // gets, replaces and restores only its own holder. A universe used concurrently must do
    // the same; making `inUse` volatile would not help, as the maps would still be shared.
    val saved = lubGlbCaches
    if (!saved.inUse) {
      saved.ownerTs = ts
      saved.ownerIsLub = isLub
      try claimLubGlbCaches(saved, body) finally saved.ownerTs = null
    } else if (saved.inProgress(isLub, ts) >= 2) body
    else {
      val fresh = new LubGlbCaches(saved)
      fresh.inUse = true
      fresh.ownerTs = ts
      fresh.ownerIsLub = isLub
      lubGlbCaches = fresh
      try body finally lubGlbCaches = saved
    }
  }

  /** Run a `lub` or `glb` with explicit depth in the caches of the enclosing call. Without one,
   *  it is outermost and must claim the caches, so a nested top-level call gets fresh ones and
   *  no entries outlive it (scala/bug#6437).
   *
   *  `body` gets the caches so that it need not look them up again, which in runtime reflection
   *  costs a thread-local access. They stay current for the whole call: a nested top-level call
   *  that installs fresh ones restores them when it finishes.
   */
  private def withEnclosingLubGlbCaches[T](body: LubGlbCaches => T): T = {
    val caches = lubGlbCaches
    if (caches.inUse) body(caches) else claimLubGlbCaches(caches, body(caches))
  }

  private def claimLubGlbCaches[T](caches: LubGlbCaches, body: => T): T = {
    caches.inUse = true
    try body finally { caches.inUse = false; caches.clear() }
  }

  def lub(ts: List[Type]): Type = ts match {
    case Nil      => NothingTpe
    case t :: Nil => t
    case _        =>
      if (settings.areStatisticsEnabled) statistics.incCounter(lubCount)
      val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, lubNanos) else null
      try withLubGlbCaches(isLub = true, ts) {
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
        if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
      }
  }

  /** A superset of the names of the members of `lubBase` that could be refined by every type in `ts`,
   *  or `null` for no restriction.
   *
   *  `t` can only refine member `sym` of `lubBase` if `t.nonPrivateMember(sym.name)` finds
   *  some symbol other than `sym`. That requires one of:
   *
   *   - a base class of `t` that is not a base class of `lubBase` declares a member with that name.
   *   - two distinct base classes shared by `t` and `lubBase` declare a member with that name, and either
   *     - they are linearized in a different order in `t` and in `lubBase`, or
   *     - their signatures match (so one overrides the other) as seen from `t` but not as seen from `lubBase`.
   *       This can happen when `lubBase` has existentially bound type arguments, e.g. `WrappedArray[_ >: Int with Double]`,
   *       or when a base class of `t` that is not a base class of `lubBase` defines a type member,
   *       e.g. `type X = Int` makes `def m(x: X)` and `def m(x: Int)` match,
   *       or implements a val of a shared base class, e.g. `val o: OInt.type` makes `def m(x: o.X)` and `def m(x: Int)` match.
   *
   *  Otherwise, both lookups walk the same declarations in the same order and find `sym`.
   *
   *  This lets `lub` avoid computing and checking all the members of `lubBase`
   *  (which typically has many members, few or none of which are refined by all of `ts`).
   *  Returns `null` (no restriction) if collecting the names looks more expensive than that.
   */
  private def lubRefinableNames(lubBase: Type, ts: List[Type]): mutable.Set[Name] = {
    val lubBaseClasses = lubBase.baseClasses
    val lubBaseTypeSeq = lubBase.baseTypeSeq
    def inLubBase(bc: Symbol) = lubBaseTypeSeq.baseTypeIndex(bc) >= 0
    val lubBaseDecls = lubBaseClasses.foldLeft(0)((n, bc) => n + bc.info.decls.size)
    // The number of declarations in the base classes of `t` that are not base classes of `lubBase`,
    // or `Int.MaxValue` if that exceeds `lubBaseDecls`.
    def ownDecls(t: Type): Int = {
      var n   = 0
      var bcs = t.baseClasses
      while (!bcs.isEmpty && n <= lubBaseDecls) {
        if (!inLubBase(bcs.head)) n += bcs.head.info.decls.size
        bcs = bcs.tail
      }
      if (n > lubBaseDecls) Int.MaxValue else n
    }
    val sized = ts.map(t => (t, ownDecls(t)))
    // Collecting the names would visit more declarations than considering all members of `lubBase`.
    if (!sized.exists(_._2 != Int.MaxValue)) return null

    val mayCollapse = containsExistential(lubBase)
    var result: mutable.Set[Name] = null
    var remaining = sized.sortBy(_._2) // fewest declarations first, so that the intersection shrinks quickly
    while (!remaining.isEmpty && ((result eq null) || !result.isEmpty)) {
      val t = remaining.head._1
      remaining = remaining.tail
      val tBaseClasses = t.baseClasses
      val names = mutable.HashSet.empty[Name]
      def add(name: Name): Unit = if ((result eq null) || result(name)) names += name
      var declaresType = false
      var stableNames: List[Name] = Nil
      tBaseClasses foreach { bc =>
        if (!inLubBase(bc)) bc.info.decls foreach { decl =>
          add(decl.name)
          if (decl.isType) declaresType = true
          else if (decl.isStable) stableNames ::= decl.name
        }
      }
      def implementsSharedVal = stableNames.exists(name =>
        tBaseClasses.exists(bc => inLubBase(bc) && bc.info.decls.lookupAll(name).exists(_.isStable)))
      val multiplyDeclaredMatters = mayCollapse || declaresType || implementsSharedVal ||
        !tBaseClasses.filter(inLubBase).corresponds(lubBaseClasses.filter(t.baseTypeIndex(_) >= 0))(_ eq _)
      if (multiplyDeclaredMatters) {
        val sharedNames = mutable.HashSet.empty[Name]
        tBaseClasses foreach { bc =>
          if (inLubBase(bc)) {
            val decls = bc.info.decls
            decls foreach (decl => if (sharedNames(decl.name)) add(decl.name))
            decls foreach (decl => sharedNames += decl.name)
          }
        }
      }
      result = names
    }
    result
  }

  /** The least upper bound wrt <:< of a list of types */
  protected[internal] def lub(ts0: List[Type], depth: Depth): Type = withEnclosingLubGlbCaches { caches =>
    val lubResults = caches.lubResults
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
          // `ts` rather than `narrowts`: `t.narrow` only adds a refinement class without decls to the base classes of `t`,
          // and computing the base classes of the fresh narrowed types would cost more than the members we are trying to avoid.
          val refinableNames = lubRefinableNames(lubBase, ts)
          val lubBaseMembers =
            if (refinableNames eq null) lubBase.nonPrivateMembers
            else if (refinableNames.isEmpty) EmptyScope
            else lubBase.nonPrivateMembersNamed(refinableNames)
          for (sym <- lubBaseMembers ; if !excludeFromLub(sym)) {
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
  private[this] var _globalGlbDepth = Depth.Zero
  def globalGlbDepth: Depth = _globalGlbDepth
  def globalGlbDepth_=(value: Depth): Unit = _globalGlbDepth = value
  private final val globalGlbLimit = Depth(2)

  /** The greatest lower bound of a list of types (as determined by `<:<`). */
  def glb(ts: List[Type]): Type = ts match {
    case Nil      => AnyTpe
    case t :: Nil => t
    case _        =>
      // `elimSuper` runs subtype checks, which must not see the caches of an enclosing call either.
      withLubGlbCaches(isLub = false, ts) {
        elimSuper(ts) match {
          case List() => AnyTpe
          case List(t) => t
          case ts0 =>
            if (settings.areStatisticsEnabled) statistics.incCounter(lubCount)
            val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, lubNanos) else null
            try glbNorm(ts0, lubDepth(ts0))
            finally {
              if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
            }
        }
      }
  }

  protected[internal] def glb(ts: List[Type], depth: Depth): Type = withEnclosingLubGlbCaches { _ =>
    elimSuper(ts) match {
      case List() => AnyTpe
      case List(t) => t
      case ts0 => glbNorm(ts0, depth)
    }
  }

  /** The greatest lower bound of a list of types (as determined by `<:<`), which have been normalized
    *  with regard to `elimSuper`. */
  protected def glbNorm(ts0: List[Type], depth: Depth): Type = withEnclosingLubGlbCaches { caches =>
    val glbResults = caches.glbResults
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
