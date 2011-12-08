/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

// Added: Sat Oct 7 16:08:21 2006
//todo: use inherited type info also for vars and values

// Added: Thu Apr 12 18:23:58 2007
//todo: disallow C#D in superclass
//todo: treat :::= correctly
package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.tools.nsc.util.BatchSourceFile
import mutable.ListBuffer
import symtab.Flags._
import util.Statistics
import util.Statistics._
import scala.tools.util.StringOps.{ countAsString, countElementsAsString }
import scala.tools.util.EditDistance.similarString

// Suggestion check whether we can do without priming scopes with symbols of outer scopes,
// like the IDE does.
/** This trait provides methods to assign types to trees.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Typers extends Modes with Adaptations with PatMatVirtualiser {
  self: Analyzer =>

  import global._
  import definitions._

  final def forArgMode(fun: Tree, mode: Int) =
    if (treeInfo.isSelfOrSuperConstrCall(fun)) mode | SCCmode
    else mode

  // namer calls typer.computeType(rhs) on DefDef / ValDef when tpt is empty. the result
  // is cached here and re-used in typedDefDef / typedValDef
  // Also used to cache imports type-checked by namer.
  val transformed = new mutable.HashMap[Tree, Tree]

  final val shortenImports = false

  def resetTyper() {
    //println("resetTyper called")
    resetContexts()
    resetNamer()
    resetImplicits()
    transformed.clear()
  }

  object UnTyper extends Traverser {
    override def traverse(tree: Tree) = {
      if (tree != EmptyTree) tree.tpe = null
      if (tree.hasSymbol) tree.symbol = NoSymbol
      super.traverse(tree)
    }
  }
/* needed for experimental version where eraly types can be type arguments
  class EarlyMap(clazz: Symbol) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(NoPrefix, sym, List()) if (sym hasFlag PRESUPER) =>
        TypeRef(ThisType(clazz), sym, List())
      case _ =>
        mapOver(tp)
    }
  }
*/

  def newTyper(context: Context): Typer = new NormalTyper(context)
  private class NormalTyper(context : Context) extends Typer(context)

  // A transient flag to mark members of anonymous classes
  // that are turned private by typedBlock
  private final val SYNTHETIC_PRIVATE = TRANS_FLAG

  private def isPastTyper = phase.id > currentRun.typerPhase.id

  abstract class Typer(context0: Context) extends TyperDiagnostics with Adaptation {
    import context0.unit
    import typeDebug.{ ptTree, ptBlock, ptLine }

    val infer = new Inferencer(context0) {
      override def isCoercible(tp: Type, pt: Type): Boolean = undoLog undo { // #3281
        tp.isError || pt.isError ||
        context0.implicitsEnabled && // this condition prevents chains of views
        inferView(EmptyTree, tp, pt, false) != EmptyTree
      }}

    /** Find implicit arguments and pass them to given tree.
     */
    def applyImplicitArgs(fun: Tree): Tree = fun.tpe match {
      case MethodType(params, _) =>
        val argResultsBuff = new ListBuffer[SearchResult]()
        val argBuff = new ListBuffer[Tree]()

        def mkPositionalArg(argTree: Tree, paramName: Name) = argTree
        def mkNamedArg(argTree: Tree, paramName: Name) = atPos(argTree.pos)(new AssignOrNamedArg(Ident(paramName), (argTree)))
        var mkArg: (Tree, Name) => Tree = mkPositionalArg

        def errorMessage(paramName: Name, paramTp: Type) =
          paramTp.typeSymbol match {
            case ImplicitNotFoundMsg(msg) => msg.format(paramName, paramTp)
            case _ =>
              "could not find implicit value for "+
                 (if (paramName startsWith nme.EVIDENCE_PARAM_PREFIX) "evidence parameter of type "
                  else "parameter "+paramName+": ")+paramTp
          }

        // DEPMETTODO: instantiate type vars that depend on earlier implicit args (see adapt (4.1))
        //
        // apply the substitutions (undet type param -> type) that were determined
        // by implicit resolution of implicit arguments on the left of this argument
        for(param <- params) {
          var paramTp = param.tpe
          for(ar <- argResultsBuff)
            paramTp = paramTp.subst(ar.subst.from, ar.subst.to)

          val res = inferImplicit(fun, paramTp, true, false, context)
          argResultsBuff += res

          if (res != SearchFailure) {
            argBuff += mkArg(res.tree, param.name)
          } else {
            mkArg = mkNamedArg // don't pass the default argument (if any) here, but start emitting named arguments for the following args
            if (!param.hasDefault)
              context.error(fun.pos, errorMessage(param.name, param.tpe))
            /* else {
             TODO: alternative (to expose implicit search failure more) -->
             resolve argument, do type inference, keep emitting positional args, infer type params based on default value for arg
             for (ar <- argResultsBuff) ar.subst traverse defaultVal
             val targs = exprTypeArgs(context.undetparams, defaultVal.tpe, paramTp)
             substExpr(tree, tparams, targs, pt)
            }*/
          }
        }

        val args = argBuff.toList
        for (ar <- argResultsBuff) {
          ar.subst traverse fun
          for (arg <- args) ar.subst traverse arg
        }

        new ApplyToImplicitArgs(fun, args) setPos fun.pos
      case ErrorType =>
        fun
    }

    /** Infer an implicit conversion (``view'') between two types.
     *  @param tree             The tree which needs to be converted.
     *  @param from             The source type of the conversion
     *  @param to               The target type of the conversion
     *  @param reportAmbiguous  Should ambiguous implicit errors be reported?
     *                          False iff we search for a view to find out
     *                          whether one type is coercible to another.
     */
    def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean): Tree = {
      debuglog("infer view from "+from+" to "+to)//debug
      if (isPastTyper) EmptyTree
      else from match {
        case MethodType(_, _) => EmptyTree
        case OverloadedType(_, _) => EmptyTree
        case PolyType(_, _) => EmptyTree
        case _ =>
          def wrapImplicit(from: Type): Tree = {
            val result = inferImplicit(tree, functionType(List(from), to), reportAmbiguous, true, context)
            if (result.subst != EmptyTreeTypeSubstituter) result.subst traverse tree
            result.tree
          }
          val result = wrapImplicit(from)
          if (result != EmptyTree) result
          else wrapImplicit(appliedType(ByNameParamClass.typeConstructor, List(from)))
      }
    }

    import infer._

    private var namerCache: Namer = null
    def namer = {
      if ((namerCache eq null) || namerCache.context != context)
        namerCache = newNamer(context)
      namerCache
    }

    var context = context0
    def context1 = context

    def dropExistential(tp: Type): Type = tp match {
      case ExistentialType(tparams, tpe) =>
        new SubstWildcardMap(tparams).apply(tp)
      case TypeRef(_, sym, _) if sym.isAliasType =>
        val tp0 = tp.normalize
        val tp1 = dropExistential(tp0)
        if (tp1 eq tp0) tp else tp1
      case _ => tp
    }

    /** Check that <code>tree</code> is a stable expression.
     *
     *  @param tree ...
     *  @return     ...
     */
    def checkStable(tree: Tree): Tree =
      if (treeInfo.isExprSafeToInline(tree)) tree
      else errorTree(
        tree,
        "stable identifier required, but "+tree+" found."+
        (if (isStableExceptVolatile(tree)) {
          val tpe = tree.symbol.tpe match {
            case PolyType(_, rtpe) => rtpe
            case t => t
          }
          "\n Note that "+tree.symbol+" is not stable because its type, "+tree.tpe+", is volatile."
      } else ""))

    /** Would tree be a stable (i.e. a pure expression) if the type
     *  of its symbol was not volatile?
     */
    private def isStableExceptVolatile(tree: Tree) = {
      tree.hasSymbol && tree.symbol != NoSymbol && tree.tpe.isVolatile &&
      { val savedTpe = tree.symbol.info
        val savedSTABLE = tree.symbol getFlag STABLE
        tree.symbol setInfo AnyRefClass.tpe
        tree.symbol setFlag STABLE
        val result = treeInfo.isExprSafeToInline(tree)
        tree.symbol setInfo savedTpe
        tree.symbol setFlag savedSTABLE
        result
      }
    }

    /** Check that `tpt` refers to a non-refinement class type */
    def checkClassType(tpt: Tree, existentialOK: Boolean, stablePrefix: Boolean) {
      def errorNotClass(found: AnyRef) = error(tpt.pos, "class type required but "+found+" found")
      def check(tpe: Type): Unit = tpe.normalize match {
        case TypeRef(pre, sym, _) if sym.isClass && !sym.isRefinementClass =>
          if (stablePrefix && !isPastTyper) {
            if (!pre.isStable)
              error(tpt.pos, "type "+pre+" is not a stable prefix")
            // A type projection like X#Y can get by the stable check if the
            // prefix is singleton-bounded, so peek at the tree too.
            else tpt match {
              case SelectFromTypeTree(qual, _) if !isSingleType(qual.tpe) => errorNotClass(tpt)
              case _                                                      => ;
            }
          }
        case ErrorType => ;
        case PolyType(_, restpe) => check(restpe)
        case ExistentialType(_, restpe) if existentialOK => check(restpe)
        case AnnotatedType(_, underlying, _) => check(underlying)
        case t => errorNotClass(t)
      }
      check(tpt.tpe)
    }

    /** Check that type <code>tp</code> is not a subtype of itself.
     *
     *  @param pos ...
     *  @param tp  ...
     *  @return    <code>true</code> if <code>tp</code> is not a subtype of itself.
     */
    def checkNonCyclic(pos: Position, tp: Type): Boolean = {
      def checkNotLocked(sym: Symbol): Boolean = {
        sym.initialize
        sym.lockOK || {error(pos, "cyclic aliasing or subtyping involving "+sym); false}
      }
      tp match {
        case TypeRef(pre, sym, args) =>
          (checkNotLocked(sym)) && (
            !sym.isNonClassType ||
            checkNonCyclic(pos, appliedType(pre.memberInfo(sym), args), sym)   // @M! info for a type ref to a type parameter now returns a polytype
            // @M was: checkNonCyclic(pos, pre.memberInfo(sym).subst(sym.typeParams, args), sym)
          )
        case SingleType(pre, sym) =>
          checkNotLocked(sym)
/*
        case TypeBounds(lo, hi) =>
          var ok = true
          for (t <- lo) ok = ok & checkNonCyclic(pos, t)
          ok
*/
        case st: SubType =>
          checkNonCyclic(pos, st.supertype)
        case ct: CompoundType =>
          ct.parents forall (x => checkNonCyclic(pos, x))
        case _ =>
          true
      }
    }

    def checkNonCyclic(pos: Position, tp: Type, lockedSym: Symbol): Boolean = try {
      lockedSym.lock {
        throw new TypeError("illegal cyclic reference involving " + lockedSym)
      }
      checkNonCyclic(pos, tp)
    } finally {
      lockedSym.unlock()
    }

    def checkNonCyclic(sym: Symbol) {
      if (!checkNonCyclic(sym.pos, sym.tpe)) sym.setInfo(ErrorType)
    }

    def checkNonCyclic(defn: Tree, tpt: Tree) {
      if (!checkNonCyclic(defn.pos, tpt.tpe, defn.symbol)) {
        tpt.tpe = ErrorType
        defn.symbol.setInfo(ErrorType)
      }
    }

    def checkParamsConvertible(pos: Position, tpe: Type) {
      tpe match {
        case MethodType(formals, restpe) =>
          /*
          if (formals.exists(_.typeSymbol == ByNameParamClass) && formals.length != 1)
            error(pos, "methods with `=>`-parameter can be converted to function values only if they take no other parameters")
          if (formals exists (isRepeatedParamType(_)))
            error(pos, "methods with `*`-parameters cannot be converted to function values");
          */
          if (restpe.isDependent)
            error(pos, "method with dependent type "+tpe+" cannot be converted to function value")
          checkParamsConvertible(pos, restpe)
        case _ =>
      }
    }

    def checkStarPatOK(pos: Position, mode: Int) =
      if ((mode & STARmode) == 0 && !isPastTyper)
        error(pos, "star patterns must correspond with varargs parameters")

    /** Check that type of given tree does not contain local or private
     *  components.
     */
    object checkNoEscaping extends TypeMap {
      private var owner: Symbol = _
      private var scope: Scope = _
      private var hiddenSymbols: List[Symbol] = _

      /** Check that type <code>tree</code> does not refer to private
       *  components unless itself is wrapped in something private
       *  (<code>owner</code> tells where the type occurs).
       *
       *  @param owner ...
       *  @param tree  ...
       *  @return      ...
       */
      def privates[T <: Tree](owner: Symbol, tree: T): T =
        check(owner, EmptyScope, WildcardType, tree)

      /** Check that type <code>tree</code> does not refer to entities
       *  defined in scope <code>scope</code>.
       *
       *  @param scope ...
       *  @param pt    ...
       *  @param tree  ...
       *  @return      ...
       */
      def locals[T <: Tree](scope: Scope, pt: Type, tree: T): T =
        check(NoSymbol, scope, pt, tree)

      def check[T <: Tree](owner: Symbol, scope: Scope, pt: Type, tree: T): T = {
        this.owner = owner
        this.scope = scope
        hiddenSymbols = List()
        val tp1 = apply(tree.tpe)
        if (hiddenSymbols.isEmpty) tree setType tp1
        else if (hiddenSymbols exists (_.isErroneous)) setError(tree)
        else if (isFullyDefined(pt)) tree setType pt
        else if (tp1.typeSymbol.isAnonymousClass)
          check(owner, scope, pt, tree setType tp1.typeSymbol.classBound)
        else if (owner == NoSymbol)
          tree setType packSymbols(hiddenSymbols.reverse, tp1)
        else if (!phase.erasedTypes) { // privates
          val badSymbol = hiddenSymbols.head
          error(tree.pos,
                (if (badSymbol.isPrivate) "private " else "") + badSymbol +
                " escapes its defining scope as part of type "+tree.tpe)
          setError(tree)
        } else tree
      }

      def addHidden(sym: Symbol) =
        if (!(hiddenSymbols contains sym)) hiddenSymbols = sym :: hiddenSymbols

      override def apply(t: Type): Type = {
        def checkNoEscape(sym: Symbol) {
          if (sym.isPrivate && !sym.hasFlag(SYNTHETIC_PRIVATE)) {
            var o = owner
            while (o != NoSymbol && o != sym.owner && o != sym.owner.linkedClassOfClass &&
                   !o.isLocal && !o.isPrivate &&
                   !o.privateWithin.hasTransOwner(sym.owner))
              o = o.owner
            if (o == sym.owner || o == sym.owner.linkedClassOfClass)
              addHidden(sym)
          } else if (sym.owner.isTerm && !sym.isTypeParameterOrSkolem) {
            var e = scope.lookupEntry(sym.name)
            var found = false
            while (!found && (e ne null) && e.owner == scope) {
              if (e.sym == sym) {
                found = true
                addHidden(sym)
              } else {
                e = scope.lookupNextEntry(e)
              }
            }
          }
        }
        mapOver(
          t match {
            case TypeRef(_, sym, args) =>
              checkNoEscape(sym)
              if (!hiddenSymbols.isEmpty && hiddenSymbols.head == sym &&
                  sym.isAliasType && sameLength(sym.typeParams, args)) {
                hiddenSymbols = hiddenSymbols.tail
                t.normalize
              } else t
            case SingleType(_, sym) =>
              checkNoEscape(sym)
              t
            case _ =>
              t
          })
      }
    }

    def reenterValueParams(vparamss: List[List[ValDef]]) {
      for (vparams <- vparamss)
        for (vparam <- vparams)
          vparam.symbol = context.scope enter vparam.symbol
    }

    def reenterTypeParams(tparams: List[TypeDef]): List[Symbol] =
      for (tparam <- tparams) yield {
        tparam.symbol = context.scope enter tparam.symbol
        tparam.symbol.deSkolemize
      }

    /** The qualifying class
     *  of a this or super with prefix <code>qual</code>.
     */
    def qualifyingClass(tree: Tree, qual: Name, packageOK: Boolean): Symbol =
      context.enclClass.owner.ownerChain.find(o => qual.isEmpty || o.isClass && o.name == qual) match {
        case Some(c) if packageOK || !c.isPackageClass =>
          c
        case _ =>
          error(
            tree.pos,
            if (qual.isEmpty) tree+" can be used only in a class, object, or template"
            else qual+" is not an enclosing class")
          NoSymbol
      }

    /** The typer for an expression, depending on where we are. If we are before a superclass
     *  call, this is a typer over a constructor context; otherwise it is the current typer.
     */
    def constrTyperIf(inConstr: Boolean): Typer =
      if (inConstr) {
        assert(context.undetparams.isEmpty)
        newTyper(context.makeConstructorContext)
      } else this

    /** The typer for a label definition. If this is part of a template we
     *  first have to enter the label definition.
     */
    def labelTyper(ldef: LabelDef): Typer =
      if (ldef.symbol == NoSymbol) { // labeldef is part of template
        val typer1 = newTyper(context.makeNewScope(ldef, context.owner))
        typer1.enterLabelDef(ldef)
        typer1
      } else this

    final val xtypes = false

    /** Is symbol defined and not stale?
     */
    def reallyExists(sym: Symbol) = {
      if (isStale(sym)) sym.setInfo(NoType)
      sym.exists
    }

    /** A symbol is stale if it is toplevel, to be loaded from a classfile, and
     *  the classfile is produced from a sourcefile which is compiled in the current run.
     */
    def isStale(sym: Symbol): Boolean = {
      sym.rawInfo.isInstanceOf[loaders.ClassfileLoader] && {
        sym.rawInfo.load(sym)
        (sym.sourceFile ne null) &&
        (currentRun.compiledFiles contains sym.sourceFile.path)
      }
    }

    /** Does the context of tree <code>tree</code> require a stable type?
     */
    private def isStableContext(tree: Tree, mode: Int, pt: Type) =
      isNarrowable(tree.tpe) && ((mode & (EXPRmode | LHSmode)) == EXPRmode) &&
      (xtypes ||
      (pt.isStable ||
       (mode & QUALmode) != 0 && !tree.symbol.isConstant ||
       pt.typeSymbol.isAbstractType && pt.bounds.lo.isStable && !(tree.tpe <:< pt)) ||
       pt.typeSymbol.isRefinementClass && !(tree.tpe <:< pt))

    /** Make symbol accessible. This means:
     *  If symbol refers to package object, insert `.package` as second to last selector.
     *  (exception for some symbols in scala package which are dealiased immediately)
     *  Call checkAccessible, which sets tree's attributes.
     *  Also note that checkAccessible looks up sym on pre without checking that pre is well-formed
     *  (illegal type applications in pre will be skipped -- that's why typedSelect wraps the resulting tree in a TreeWithDeferredChecks)
     *  @return modified tree and new prefix type
     */
    private def makeAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): (Tree, Type) =
      if (isInPackageObject(sym, pre.typeSymbol)) {
        if (pre.typeSymbol == ScalaPackageClass && sym.isTerm) {
          // short cut some aliases. It seems pattern matching needs this
          // to notice exhaustiveness and to generate good code when
          // List extractors are mixed with :: patterns. See Test5 in lists.scala.
          def dealias(sym: Symbol) =
            (atPos(tree.pos) {gen.mkAttributedRef(sym)}, sym.owner.thisType)
          sym.name match {
            case nme.List => return dealias(ListModule)
            case nme.Seq  => return dealias(SeqModule)
            case nme.Nil  => return dealias(NilModule)
            case _ =>
          }
        }
        val qual = typedQualifier { atPos(tree.pos.makeTransparent) {
          tree match {
            case Ident(_) => Ident(nme.PACKAGEkw)
            case Select(qual, _) => Select(qual, nme.PACKAGEkw)
            case SelectFromTypeTree(qual, _) => Select(qual, nme.PACKAGEkw)
          }
        }}
        val tree1 = atPos(tree.pos) {
          tree match {
            case Ident(name) => Select(qual, name)
            case Select(_, name) => Select(qual, name)
            case SelectFromTypeTree(_, name) => SelectFromTypeTree(qual, name)
          }
        }
        (checkAccessible(tree1, sym, qual.tpe, qual), qual.tpe)
      } else {
        (checkAccessible(tree, sym, pre, site), pre)
      }

    /** Is `sym` defined in package object of package `pkg`?
     */
    private def isInPackageObject(sym: Symbol, pkg: Symbol) =
      pkg.isPackageClass && {
        sym.alternatives forall { sym =>
          !sym.owner.isPackage && {
            sym.owner.isPackageObjectClass &&
            sym.owner.owner == pkg ||
            pkg.isInitialized && {
              // need to be careful here to not get a cyclic reference during bootstrap
              val pkgobj = pkg.info.member(nme.PACKAGEkw)
              pkgobj.isInitialized &&
              (pkgobj.info.member(sym.name).alternatives contains sym)
            }
          }
        }
      }

    /** Post-process an identifier or selection node, performing the following:
     *  1. Check that non-function pattern expressions are stable
     *  2. Check that packages and static modules are not used as values
     *  3. Turn tree type into stable type if possible and required by context.
     */
    private def stabilize(tree: Tree, pre: Type, mode: Int, pt: Type): Tree = {
      if (tree.symbol.isOverloaded && !inFunMode(mode))
        inferExprAlternative(tree, pt)

      val sym = tree.symbol
      def fail() = errorTree(tree, sym.kindString + " " + sym.fullName + " is not a value")

      if (tree.tpe.isError) tree
      else if ((mode & (PATTERNmode | FUNmode)) == PATTERNmode && tree.isTerm) { // (1)
        if (sym.isValue) checkStable(tree)
        else fail()
      } else if ((mode & (EXPRmode | QUALmode)) == EXPRmode && !sym.isValue && !phase.erasedTypes) { // (2)
        fail()
      } else {
        if (sym.isStable && pre.isStable && !isByNameParamType(tree.tpe) &&
            (isStableContext(tree, mode, pt) || sym.isModule && !sym.isMethod))
          tree.setType(singleType(pre, sym))
        else tree
      }
    }

    private def isNarrowable(tpe: Type): Boolean = tpe match {
      case TypeRef(_, _, _) | RefinedType(_, _) => true
      case ExistentialType(_, tpe1) => isNarrowable(tpe1)
      case AnnotatedType(_, tpe1, _) => isNarrowable(tpe1)
      case PolyType(_, tpe1) => isNarrowable(tpe1)
      case NullaryMethodType(tpe1) => isNarrowable(tpe1)
      case _ => !phase.erasedTypes
    }

    /**
     *  @param tree ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def stabilizeFun(tree: Tree, mode: Int, pt: Type): Tree = {
      val sym = tree.symbol
      val pre = tree match {
        case Select(qual, _) => qual.tpe
        case _ => NoPrefix
      }
      if (tree.tpe.isInstanceOf[MethodType] && pre.isStable && sym.tpe.params.isEmpty &&
          (isStableContext(tree, mode, pt) || sym.isModule))
        tree.setType(MethodType(List(), singleType(pre, sym)))
      else tree
    }

    /** The member with given name of given qualifier tree */
    def member(qual: Tree, name: Name) = {
      def callSiteWithinClass(clazz: Symbol) = context.enclClass.owner hasTransOwner clazz
      val includeLocals = qual.tpe match {
        case ThisType(clazz) if callSiteWithinClass(clazz)                => true
        case SuperType(clazz, _) if callSiteWithinClass(clazz.typeSymbol) => true
        case _                                                            => phase.next.erasedTypes
      }
      if (includeLocals) qual.tpe member name
      else qual.tpe nonLocalMember name
    }

    def silent[T](op: Typer => T,
                  reportAmbiguousErrors: Boolean = context.reportAmbiguousErrors,
                  newtree: Tree = context.tree): Any /* in fact, TypeError or T */ = {
      val rawTypeStart = startCounter(rawTypeFailed)
      val findMemberStart = startCounter(findMemberFailed)
      val subtypeStart = startCounter(subtypeFailed)
      val failedSilentStart = startTimer(failedSilentNanos)
      try {
        if (context.reportGeneralErrors ||
            reportAmbiguousErrors != context.reportAmbiguousErrors ||
            newtree != context.tree) {
          val context1 = context.makeSilent(reportAmbiguousErrors, newtree)
          context1.undetparams = context.undetparams
          context1.savedTypeBounds = context.savedTypeBounds
          context1.namedApplyBlockInfo = context.namedApplyBlockInfo
          val typer1 = newTyper(context1)
          val result = op(typer1)
          context.undetparams = context1.undetparams
          context.savedTypeBounds = context1.savedTypeBounds
          context.namedApplyBlockInfo = context1.namedApplyBlockInfo
          result
        } else {
          op(this)
        }
      } catch {
        case ex: CyclicReference => throw ex
        case ex: TypeError =>
          stopCounter(rawTypeFailed, rawTypeStart)
          stopCounter(findMemberFailed, findMemberStart)
          stopCounter(subtypeFailed, subtypeStart)
          stopTimer(failedSilentNanos, failedSilentStart)
          ex
      }
    }

    /** Utility method: Try op1 on tree. If that gives an error try op2 instead.
     */
    def tryBoth(tree: Tree)(op1: (Typer, Tree) => Tree)(op2: (Typer, Tree) => Tree): Tree =
      silent(op1(_, tree)) match {
        case result1: Tree =>
          result1
        case ex1: TypeError =>
          silent(op2(_, resetAllAttrs(tree))) match {
            case result2: Tree =>
//              println("snd succeeded: "+result2)
              result2
            case ex2: TypeError =>
              reportTypeError(tree.pos, ex1)
              setError(tree)
          }
      }

    def isCodeType(tpe: Type) = tpe.typeSymbol isNonBottomSubClass CodeClass

    /** Perform the following adaptations of expression, pattern or type `tree` wrt to
     *  given mode `mode` and given prototype `pt`:
     *  (-1) For expressions with annotated types, let AnnotationCheckers decide what to do
     *  (0) Convert expressions with constant types to literals (unless in interactive/scaladoc mode)
     *  (1) Resolve overloading, unless mode contains FUNmode
     *  (2) Apply parameterless functions
     *  (3) Apply polymorphic types to fresh instances of their type parameters and
     *      store these instances in context.undetparams,
     *      unless followed by explicit type application.
     *  (4) Do the following to unapplied methods used as values:
     *  (4.1) If the method has only implicit parameters pass implicit arguments
     *  (4.2) otherwise, if `pt` is a function type and method is not a constructor,
     *        convert to function by eta-expansion,
     *  (4.3) otherwise, if the method is nullary with a result type compatible to `pt`
     *        and it is not a constructor, apply it to ()
     *  otherwise issue an error
     *  (5) Convert constructors in a pattern as follows:
     *  (5.1) If constructor refers to a case class factory, set tree's type to the unique
     *        instance of its primary constructor that is a subtype of the expected type.
     *  (5.2) If constructor refers to an extractor, convert to application of
     *        unapply or unapplySeq method.
     *
     *  (6) Convert all other types to TypeTree nodes.
     *  (7) When in TYPEmode but not FUNmode or HKmode, check that types are fully parameterized
     *      (7.1) In HKmode, higher-kinded types are allowed, but they must have the expected kind-arity
     *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
     *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
     *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
     *  (10) If the expected type is Byte, Short or Char, and the expression
     *      is an integer fitting in the range of that type, convert it to that type.
     *  (11) Widen numeric literals to their expected type, if necessary
     *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is scala.Unit.
     *  (13) When in mode EXPRmode, apply AnnotationChecker conversion if expected type is annotated.
     *  (14) When in mode EXPRmode, apply a view
     *  If all this fails, error
     */
    protected def adapt(tree: Tree, mode: Int, pt: Type, original: Tree = EmptyTree): Tree = {

      def adaptToImplicitMethod(mt: MethodType): Tree = {
        if (context.undetparams nonEmpty) { // (9) -- should revisit dropped condition `(mode & POLYmode) == 0`
          // dropped so that type args of implicit method are inferred even if polymorphic expressions are allowed
          // needed for implicits in 2.8 collection library -- maybe once #3346 is fixed, we can reinstate the condition?
          context.undetparams =
            inferExprInstance(tree, context.extractUndetparams(), pt,
              // approximate types that depend on arguments since dependency on implicit argument is like dependency on type parameter
              mt.approximate,
              // if we are looking for a manifest, instantiate type to Nothing anyway,
              // as we would get ambiguity errors otherwise. Example
              // Looking for a manifest of Nil: This has many potential types,
              // so we need to instantiate to minimal type List[Nothing].
              keepNothings = false, // retract Nothing's that indicate failure, ambiguities in manifests are dealt with in manifestOfType
              useWeaklyCompatible = true) // #3808
        }

        val typer1 = constrTyperIf(treeInfo.isSelfOrSuperConstrCall(tree))
        if (original != EmptyTree && pt != WildcardType)
          typer1.silent(tpr => tpr.typed(tpr.applyImplicitArgs(tree), mode, pt)) match {
            case result: Tree => result
            case ex: TypeError =>
              debuglog("fallback on implicits: " + tree + "/" + resetAllAttrs(original))
              val tree1 = typed(resetAllAttrs(original), mode, WildcardType)
              tree1.tpe = addAnnotations(tree1, tree1.tpe)
              if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt, EmptyTree)
          }
        else
          typer1.typed(typer1.applyImplicitArgs(tree), mode, pt)
      }

      def instantiateToMethodType(mt: MethodType): Tree = {
        val meth = tree match {
          // a partial named application is a block (see comment in EtaExpansion)
          case Block(_, tree1) => tree1.symbol
          case _               => tree.symbol
        }
        if (!meth.isConstructor && !meth.isMacro && isFunctionType(pt)) { // (4.2)
          debuglog("eta-expanding " + tree + ":" + tree.tpe + " to " + pt)
          checkParamsConvertible(tree.pos, tree.tpe)
          val tree0 = etaExpand(context.unit, tree)
          // println("eta "+tree+" ---> "+tree0+":"+tree0.tpe+" undet: "+context.undetparams+ " mode: "+Integer.toHexString(mode))

          if (meth.typeParams.nonEmpty) {
            // #2624: need to infer type arguments for eta expansion of a polymorphic method
            // context.undetparams contains clones of meth.typeParams (fresh ones were generated in etaExpand)
            // need to run typer on tree0, since etaExpansion sets the tpe's of its subtrees to null
            // can't type with the expected type, as we can't recreate the setup in (3) without calling typed
            // (note that (3) does not call typed to do the polymorphic type instantiation --
            //  it is called after the tree has been typed with a polymorphic expected result type)
            instantiate(typed(tree0, mode, WildcardType), mode, pt)
          } else
            typed(tree0, mode, pt)
        } else if (!meth.isConstructor && mt.params.isEmpty) { // (4.3)
          adapt(typed(Apply(tree, List()) setPos tree.pos), mode, pt, original)
        } else if (context.implicitsEnabled) {
          errorTree(tree, "missing arguments for " + meth + meth.locationString +
            (if (meth.isConstructor) ""
            else ";\nfollow this method with `_' if you want to treat it as a partially applied function"))
        } else {
          setError(tree)
        }
      }

      def adaptType(): Tree = {
        if (inFunMode(mode)) {
          tree
        } else if (tree.hasSymbol && !tree.symbol.typeParams.isEmpty && !inHKMode(mode) &&
          !(tree.symbol.isJavaDefined && context.unit.isJava)) { // (7)
          // @M When not typing a higher-kinded type ((mode & HKmode) == 0)
          // or raw type (tree.symbol.isJavaDefined && context.unit.isJava), types must be of kind *,
          // and thus parameterized types must be applied to their type arguments
          // @M TODO: why do kind-* tree's have symbols, while higher-kinded ones don't?
          errorTree(tree, tree.symbol + " takes type parameters")
          tree setType tree.tpe
        } else if ( // (7.1) @M: check kind-arity
        // @M: removed check for tree.hasSymbol and replace tree.symbol by tree.tpe.symbol (TypeTree's must also be checked here, and they don't directly have a symbol)
        (inHKMode(mode)) &&
          // @M: don't check tree.tpe.symbol.typeParams. check tree.tpe.typeParams!!!
          // (e.g., m[Int] --> tree.tpe.symbol.typeParams.length == 1, tree.tpe.typeParams.length == 0!)
          !sameLength(tree.tpe.typeParams, pt.typeParams) &&
          !(tree.tpe.typeSymbol == AnyClass ||
            tree.tpe.typeSymbol == NothingClass ||
            pt == WildcardType)) {
          // Check that the actual kind arity (tree.symbol.typeParams.length) conforms to the expected
          // kind-arity (pt.typeParams.length). Full checks are done in checkKindBounds in Infer.
          // Note that we treat Any and Nothing as kind-polymorphic.
          // We can't perform this check when typing type arguments to an overloaded method before the overload is resolved
          // (or in the case of an error type) -- this is indicated by pt == WildcardType (see case TypeApply in typed1).
          errorTree(tree, tree.tpe + " takes " + countElementsAsString(tree.tpe.typeParams.length, "type parameter") +
            ", expected: " + countAsString(pt.typeParams.length))
          tree setType tree.tpe
        } else tree match { // (6)
          case TypeTree() => tree
          case _          => TypeTree(tree.tpe) setOriginal (tree) setPos (tree.pos)
        }
      }

      def adaptConstrPattern(): Tree = { // (5)
        val extractor = tree.symbol.filter(sym => reallyExists(unapplyMember(sym.tpe)))
        if (extractor != NoSymbol) {
          tree setSymbol extractor
          val unapply = unapplyMember(extractor.tpe)
          val clazz = unapplyParameterType(unapply)

          if (unapply.isCase && clazz.isCase && !(clazz.ancestors exists (_.isCase))) {
            // convert synthetic unapply of case class to case class constructor
            val prefix = tree.tpe.prefix
            val tree1 = TypeTree(clazz.primaryConstructor.tpe.asSeenFrom(prefix, clazz.owner))
              .setOriginal(tree)

            inferConstructorInstance(tree1, clazz.typeParams, pt)
            tree1
          } else {
            tree
          }
        } else {
          errorTree(tree, tree.symbol + " is not a case class constructor, nor does it have an unapply/unapplySeq method")
        }
      }

      def insertApply(): Tree = {
        assert(!inHKMode(mode)) //@M
        val qual = adaptToName(tree, nme.apply) match {
          case id @ Ident(_) =>
            val pre = if (id.symbol.owner.isPackageClass) id.symbol.owner.thisType
            else if (id.symbol.owner.isClass)
              context.enclosingSubClassContext(id.symbol.owner).prefix
            else NoPrefix
            stabilize(id, pre, EXPRmode | QUALmode, WildcardType)
          case sel @ Select(qualqual, _) =>
            stabilize(sel, qualqual.tpe, EXPRmode | QUALmode, WildcardType)
          case other =>
            other
        }
        typed(atPos(tree.pos)(Select(qual, nme.apply)), mode, pt)
      }

      // begin adapt
      tree.tpe match {
        case atp @ AnnotatedType(_, _, _) if canAdaptAnnotations(tree, mode, pt) => // (-1)
          adaptAnnotations(tree, mode, pt)
        case ct @ ConstantType(value) if inNoModes(mode, TYPEmode | FUNmode) && (ct <:< pt) && !forScaladoc && !forInteractive => // (0)
          val sym = tree.symbol
          if (sym != null && sym.isDeprecated) {
            val msg = sym.toString + sym.locationString + " is deprecated: " + sym.deprecationMessage.getOrElse("")
            unit.deprecationWarning(tree.pos, msg)
          }
          treeCopy.Literal(tree, value)
        case OverloadedType(pre, alts) if !inFunMode(mode) => // (1)
          inferExprAlternative(tree, pt)
          adapt(tree, mode, pt, original)
        case NullaryMethodType(restpe) => // (2)
          adapt(tree setType restpe, mode, pt, original)
        case TypeRef(_, ByNameParamClass, List(arg)) if ((mode & EXPRmode) != 0) => // (2)
          adapt(tree setType arg, mode, pt, original)
        case tr @ TypeRef(_, sym, _) if sym.isAliasType && tr.normalize.isInstanceOf[ExistentialType] &&
          ((mode & (EXPRmode | LHSmode)) == EXPRmode) =>
          adapt(tree setType tr.normalize.skolemizeExistential(context.owner, tree), mode, pt, original)
        case et @ ExistentialType(_, _) if ((mode & (EXPRmode | LHSmode)) == EXPRmode) =>
          adapt(tree setType et.skolemizeExistential(context.owner, tree), mode, pt, original)
        case PolyType(tparams, restpe) if inNoModes(mode, TAPPmode | PATTERNmode | HKmode) => // (3)
          // assert((mode & HKmode) == 0) //@M a PolyType in HKmode represents an anonymous type function,
          // we're in HKmode since a higher-kinded type is expected --> hence, don't implicitly apply it to type params!
          // ticket #2197 triggered turning the assert into a guard
          // I guess this assert wasn't violated before because type aliases weren't expanded as eagerly
          //  (the only way to get a PolyType for an anonymous type function is by normalisation, which applies eta-expansion)
          // -- are we sure we want to expand aliases this early?
          // -- what caused this change in behaviour??
          val tparams1 = cloneSymbols(tparams)
          val tree1 = if (tree.isType) tree
          else TypeApply(tree, tparams1 map (tparam =>
            TypeTree(tparam.tpeHK) setPos tree.pos.focus)) setPos tree.pos //@M/tcpolyinfer: changed tparam.tpe to tparam.tpeHK
          context.undetparams ++= tparams1
          adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt, original)
        case mt: MethodType if mt.isImplicit && ((mode & (EXPRmode | FUNmode | LHSmode)) == EXPRmode) => // (4.1)
          adaptToImplicitMethod(mt)

        case mt: MethodType if (((mode & (EXPRmode | FUNmode | LHSmode)) == EXPRmode) &&
          (context.undetparams.isEmpty || inPolyMode(mode))) =>
          instantiateToMethodType(mt)

        case _ =>
          def applyPossible = {
            def applyMeth = member(adaptToName(tree, nme.apply), nme.apply)
            if ((mode & TAPPmode) != 0)
              tree.tpe.typeParams.isEmpty && applyMeth.filter(!_.tpe.typeParams.isEmpty) != NoSymbol
            else
              applyMeth.filter(_.tpe.paramSectionCount > 0) != NoSymbol
          }
          if (tree.isType)
            adaptType()
          else if ((mode & (PATTERNmode | FUNmode)) == (PATTERNmode | FUNmode))
            adaptConstrPattern()
          else if (inAllModes(mode, EXPRmode | FUNmode) &&
            !tree.tpe.isInstanceOf[MethodType] &&
            !tree.tpe.isInstanceOf[OverloadedType] &&
            applyPossible)
            insertApply()
          else if (!context.undetparams.isEmpty && !inPolyMode(mode)) { // (9)
            assert(!inHKMode(mode)) //@M
            if (inExprModeButNot(mode, FUNmode) && pt.typeSymbol == UnitClass)
              instantiateExpectingUnit(tree, mode)
            else
              instantiate(tree, mode, pt)
          } else if (tree.tpe <:< pt) {
            tree
          } else {
            if (inPatternMode(mode)) {
              if ((tree.symbol ne null) && tree.symbol.isModule)
                inferModulePattern(tree, pt)
              if (isPopulated(tree.tpe, approximateAbstracts(pt)))
                return tree
            }
            val tree1 = constfold(tree, pt) // (10) (11)
            if (tree1.tpe <:< pt) adapt(tree1, mode, pt, original)
            else {
              if (inExprModeButNot(mode, FUNmode)) {
                pt.normalize match {
                  case TypeRef(_, sym, _) =>
                    // note: was if (pt.typeSymbol == UnitClass) but this leads to a potentially
                    // infinite expansion if pt is constant type ()
                    if (sym == UnitClass && tree.tpe <:< AnyClass.tpe) { // (12)
                      if (settings.warnValueDiscard.value)
                        context.unit.warning(tree.pos, "discarded non-Unit value")
                      return typed(atPos(tree.pos)(Block(List(tree), Literal(Constant()))), mode, pt)
                    } else if (isNumericValueClass(sym) && isNumericSubType(tree.tpe, pt)) {
                      if (settings.warnNumericWiden.value)
                        context.unit.warning(tree.pos, "implicit numeric widening")
                      return typed(atPos(tree.pos)(Select(tree, "to" + sym.name)), mode, pt)
                    }
                  case AnnotatedType(_, _, _) if canAdaptAnnotations(tree, mode, pt) => // (13)
                    return typed(adaptAnnotations(tree, mode, pt), mode, pt)
                  case _ =>
                }
                if (!context.undetparams.isEmpty) {
                  return instantiate(tree, mode, pt)
                }
                if (context.implicitsEnabled && !tree.tpe.isError && !pt.isError) {
                  // (14); the condition prevents chains of views
                  debuglog("inferring view from " + tree.tpe + " to " + pt)
                  val coercion = inferView(tree, tree.tpe, pt, true)
                  // convert forward views of delegate types into closures wrapped around
                  // the delegate's apply method (the "Invoke" method, which was translated into apply)
                  if (forMSIL && coercion != null && isCorrespondingDelegate(tree.tpe, pt)) {
                    val meth: Symbol = tree.tpe.member(nme.apply)
                    debuglog("replacing forward delegate view with: " + meth + ":" + meth.tpe)
                    return typed(Select(tree, meth), mode, pt)
                  }
                  if (coercion != EmptyTree) {
                    debuglog("inferred view from " + tree.tpe + " to " + pt + " = " + coercion + ":" + coercion.tpe)
                    return newTyper(context.makeImplicit(context.reportAmbiguousErrors)).typed(
                      new ApplyImplicitView(coercion, List(tree)) setPos tree.pos, mode, pt)
                  }
                }
              }
              if (settings.debug.value) {
                log("error tree = " + tree)
                if (settings.explaintypes.value) explainTypes(tree.tpe, pt)
              }
              try {
                typeErrorTree(tree, tree.tpe, pt)
              } catch {
                case ex: TypeError =>
                  if (isPastTyper && pt.existentialSkolems.nonEmpty) {
                    // Ignore type errors raised in later phases that are due to mismatching types with existential skolems
                    // We have lift crashing in 2.9 with an adapt failure in the pattern matcher.
                    // Here's my hypothsis why this happens. The pattern matcher defines a variable of type
                    //
                    //   val x: T = expr
                    //
                    // where T is the type of expr, but T contains existential skolems ts.
                    // In that case, this value definition does not typecheck.
                    // The value definition
                    //
                    //   val x: T forSome { ts } = expr
                    //
                    // would typecheck. Or one can simply leave out the type of the `val`:
                    //
                    //   val x = expr
                    context.unit.warning(tree.pos, "recovering from existential Skolem type error in tree \n" + tree + "\nwith type " + tree.tpe + "\n expected type = " + pt + "\n context = " + context.tree)
                    adapt(tree, mode, deriveTypeWithWildcards(pt.existentialSkolems)(pt))
                  } else
                    throw ex
              }
            }
          }
      }
    }

    def instantiate(tree: Tree, mode: Int, pt: Type): Tree = {
      inferExprInstance(tree, context.extractUndetparams(), pt)
      adapt(tree, mode, pt)
    }
    /** If the expected type is Unit: try instantiating type arguments
     *  with expected type Unit, but if that fails, try again with pt = WildcardType
     *  and discard the expression.
     */
    def instantiateExpectingUnit(tree: Tree, mode: Int): Tree = {
      val savedUndetparams = context.undetparams
      silent(_.instantiate(tree, mode, UnitClass.tpe)) match {
        case t: Tree => t
        case _ =>
          context.undetparams = savedUndetparams
          val valueDiscard = atPos(tree.pos)(Block(List(instantiate(tree, mode, WildcardType)), Literal(Constant())))
          typed(valueDiscard, mode, UnitClass.tpe)
      }
    }

    private def isAdaptableWithView(qual: Tree) = {
      val qtpe = qual.tpe.widen
      (    !isPastTyper
        && qual.isTerm
        && ((qual.symbol eq null) || !qual.symbol.isTerm || qual.symbol.isValue)
        && !qtpe.isError
        && !qtpe.typeSymbol.isBottomClass
        && qtpe != WildcardType
        && !qual.isInstanceOf[ApplyImplicitView] // don't chain views
        && context.implicitsEnabled
        // Elaborating `context.implicitsEnabled`:
        // don't try to adapt a top-level type that's the subject of an implicit search
        // this happens because, if isView, typedImplicit tries to apply the "current" implicit value to
        // a value that needs to be coerced, so we check whether the implicit value has an `apply` method.
        // (If we allow this, we get divergence, e.g., starting at `conforms` during ant quick.bin)
        // Note: implicit arguments are still inferred (this kind of "chaining" is allowed)
      )
    }

    def adaptToMember(qual: Tree, searchTemplate: Type): Tree = {
      if (isAdaptableWithView(qual)) {
        qual.tpe.widen.normalize match {
          case et: ExistentialType =>
            qual setType et.skolemizeExistential(context.owner, qual) // open the existential
          case _ =>
        }
        inferView(qual, qual.tpe, searchTemplate, true) match {
          case EmptyTree  => qual
          case coercion   => typedQualifier(atPos(qual.pos)(new ApplyImplicitView(coercion, List(qual))))
        }
      }
      else qual
    }

    /** Try to apply an implicit conversion to `qual` to that it contains
     *  a method `name` which can be applied to arguments `args` with expected type `pt`.
     *  If `pt` is defined, there is a fallback to try again with pt = ?.
     *  This helps avoiding propagating result information too far and solves
     *  #1756.
     *  If no conversion is found, return `qual` unchanged.
     *
     */
    def adaptToArguments(qual: Tree, name: Name, args: List[Tree], pt: Type): Tree = {
      def doAdapt(restpe: Type) =
        //util.trace("adaptToArgs "+qual+", name = "+name+", argtpes = "+(args map (_.tpe))+", pt = "+pt+" = ")
        adaptToMember(qual, HasMethodMatching(name, args map (_.tpe), restpe))
      if (pt != WildcardType) {
        silent(_ => doAdapt(pt)) match {
          case result: Tree if result != qual =>
            result
          case _ =>
            debuglog("fallback on implicits in adaptToArguments: "+qual+" . "+name)
            doAdapt(WildcardType)
        }
      } else
        doAdapt(pt)
    }

    /** Try o apply an implicit conversion to `qual` to that it contains
     *  a method `name`. If that's ambiguous try taking arguments into account using `adaptToArguments`.
     */
    def adaptToMemberWithArgs(tree: Tree, qual: Tree, name: Name, mode: Int): Tree = {
      try {
        adaptToMember(qual, HasMember(name))
      } catch {
        case ex: TypeError =>
        // this happens if implicits are ambiguous; try again with more context info.
        // println("last ditch effort: "+qual+" . "+name)
        context.tree match {
          case Apply(tree1, args) if (tree1 eq tree) && args.nonEmpty => // try handling the arguments
            // println("typing args: "+args)
            silent(_.typedArgs(args, mode)) match {
              case args: List[_] =>
                adaptToArguments(qual, name, args.asInstanceOf[List[Tree]], WildcardType)
              case _ =>
                throw ex
            }
          case _ =>
            // println("not in an apply: "+context.tree+"/"+tree)
            throw ex
        }
      }
    }

    /** Try to apply an implicit conversion to `qual` to that it contains a
     *  member `name` of arbitrary type.
     *  If no conversion is found, return `qual` unchanged.
     */
    def adaptToName(qual: Tree, name: Name) =
      if (member(qual, name) != NoSymbol) qual
      else adaptToMember(qual, HasMember(name))

    private def typePrimaryConstrBody(clazz : Symbol, cbody: Tree, tparams: List[Symbol], enclTparams: List[Symbol], vparamss: List[List[ValDef]]): Tree = {
      // XXX: see about using the class's symbol....
      enclTparams foreach (sym => context.scope.enter(sym))
      namer.enterValueParams(vparamss)
      typed(cbody)
    }

    private def validateNoCaseAncestor(clazz: Symbol) = {
      if (!phase.erasedTypes) {
        for (ancestor <- clazz.ancestors find (_.isCase)) {
          unit.error(clazz.pos, (
            "case %s has case ancestor %s, but case-to-case inheritance is prohibited."+
            " To overcome this limitation, use extractors to pattern match on non-leaf nodes."
          ).format(clazz, ancestor.fullName))
        }
      }
    }

    def parentTypes(templ: Template): List[Tree] =
      if (templ.parents.isEmpty) List()
      else try {
        val clazz = context.owner
        // Normalize supertype and mixins so that supertype is always a class, not a trait.
        var supertpt = typedTypeConstructor(templ.parents.head)
        val firstParent = supertpt.tpe.typeSymbol
        var mixins = templ.parents.tail map typedType
        // If first parent is a trait, make it first mixin and add its superclass as first parent
        while ((supertpt.tpe.typeSymbol ne null) && supertpt.tpe.typeSymbol.initialize.isTrait) {
          val supertpt1 = typedType(supertpt)
          if (!supertpt1.tpe.isError) {
            mixins = supertpt1 :: mixins
            supertpt = TypeTree(supertpt1.tpe.parents.head) setPos supertpt.pos.focus
          }
        }

        // Determine
        //  - supertparams: Missing type parameters from supertype
        //  - supertpe: Given supertype, polymorphic in supertparams
        val supertparams = if (supertpt.hasSymbol) supertpt.symbol.typeParams else List()
        var supertpe = supertpt.tpe
        if (!supertparams.isEmpty)
          supertpe = PolyType(supertparams, appliedType(supertpe, supertparams map (_.tpeHK)))

        // A method to replace a super reference by a New in a supercall
        def transformSuperCall(scall: Tree): Tree = (scall: @unchecked) match {
          case Apply(fn, args) =>
            treeCopy.Apply(scall, transformSuperCall(fn), args map (_.duplicate))
          case Select(Super(_, _), nme.CONSTRUCTOR) =>
            treeCopy.Select(
              scall,
              atPos(supertpt.pos.focus)(New(TypeTree(supertpe)) setType supertpe),
              nme.CONSTRUCTOR)
        }

        treeInfo.firstConstructor(templ.body) match {
          case constr @ DefDef(_, _, _, vparamss, _, cbody @ Block(cstats, cunit)) =>
            // Convert constructor body to block in environment and typecheck it
            val (preSuperStats, superCall) = {
              val (stats, rest) = cstats span (x => !treeInfo.isSuperConstrCall(x))
              (stats map (_.duplicate), if (rest.isEmpty) EmptyTree else rest.head.duplicate)
            }
            val cstats1 = if (superCall == EmptyTree) preSuperStats else preSuperStats :+ superCall
            val cbody1 = treeCopy.Block(cbody, preSuperStats, superCall match {
              case Apply(_, _) if supertparams.nonEmpty => transformSuperCall(superCall)
              case _                                    => cunit.duplicate
            })
            val outercontext = context.outer

            assert(clazz != NoSymbol)
            val cscope = outercontext.makeNewScope(constr, outercontext.owner)
            val cbody2 = newTyper(cscope) // called both during completion AND typing.
                .typePrimaryConstrBody(clazz,
                  cbody1, supertparams, clazz.unsafeTypeParams, vparamss map (_.map(_.duplicate)))
            superCall match {
              case Apply(_, _) =>
                val sarg = treeInfo.firstArgument(superCall)
                if (sarg != EmptyTree && supertpe.typeSymbol != firstParent)
                  error(sarg.pos, firstParent+" is a trait; does not take constructor arguments")
                if (!supertparams.isEmpty) supertpt = TypeTree(cbody2.tpe) setPos supertpt.pos.focus
              case _ =>
                if (!supertparams.isEmpty) error(supertpt.pos, "missing type arguments")
            }

            val preSuperVals = treeInfo.preSuperFields(templ.body)
            if (preSuperVals.isEmpty && preSuperStats.nonEmpty)
              debugwarn("Wanted to zip empty presuper val list with " + preSuperStats)
            else
              (preSuperStats, preSuperVals).zipped map { case (ldef, gdef) => gdef.tpt.tpe = ldef.symbol.tpe }

          case _ =>
            if (!supertparams.isEmpty) error(supertpt.pos, "missing type arguments")
        }
/* experimental: early types as type arguments
        val hasEarlyTypes = templ.body exists (treeInfo.isEarlyTypeDef)
        val earlyMap = new EarlyMap(clazz)
        List.mapConserve(supertpt :: mixins){ tpt =>
          val tpt1 = checkNoEscaping.privates(clazz, tpt)
          if (hasEarlyTypes) tpt1 else tpt1 setType earlyMap(tpt1.tpe)
        }
*/

        //Console.println("parents("+clazz") = "+supertpt :: mixins);//DEBUG

        // Certain parents are added in the parser before it is known whether
        // that class also declared them as parents.  For instance, this is an
        // error unless we take corrective action here:
        //
        //   case class Foo() extends Serializable
        //
        // So we strip the duplicates before typer.
        def fixDuplicates(remaining: List[Tree]): List[Tree] = remaining match {
          case Nil      => Nil
          case x :: xs  =>
            val sym = x.symbol
            x :: fixDuplicates(
              if (isPossibleSyntheticParent(sym)) xs filterNot (_.symbol == sym)
              else xs
            )
        }
        fixDuplicates(supertpt :: mixins) mapConserve (tpt => checkNoEscaping.privates(clazz, tpt))
      }
      catch {
        case ex: TypeError =>
          templ.tpe = null
          reportTypeError(templ.pos, ex)
          List(TypeTree(AnyRefClass.tpe))
      }

    /** <p>Check that</p>
     *  <ul>
     *    <li>all parents are class types,</li>
     *    <li>first parent class is not a mixin; following classes are mixins,</li>
     *    <li>final classes are not inherited,</li>
     *    <li>
     *      sealed classes are only inherited by classes which are
     *      nested within definition of base class, or that occur within same
     *      statement sequence,
     *    </li>
     *    <li>self-type of current class is a subtype of self-type of each parent class.</li>
     *    <li>no two parents define same symbol.</li>
     *  </ul>
     */
    def validateParentClasses(parents: List[Tree], selfType: Type) {
      def validateParentClass(parent: Tree, superclazz: Symbol) {
        if (!parent.tpe.isError) {
          val psym = parent.tpe.typeSymbol.initialize
          checkClassType(parent, false, true)
          if (psym != superclazz) {
            if (psym.isTrait) {
              val ps = psym.info.parents
              if (!ps.isEmpty && !superclazz.isSubClass(ps.head.typeSymbol))
                error(parent.pos, "illegal inheritance; super"+superclazz+
                      "\n is not a subclass of the super"+ps.head.typeSymbol+
                      "\n of the mixin " + psym)
            } else {
              error(parent.pos, psym+" needs to be a trait to be mixed in")
            }
          }
          if (psym.isFinal) {
            error(parent.pos, "illegal inheritance from final "+psym)
          }
          if (psym.isSealed && !phase.erasedTypes) {
            // AnyVal is sealed, but we have to let the value classes through manually
            if (context.unit.source.file == psym.sourceFile || isValueClass(context.owner))
              psym addChild context.owner
            else
              error(parent.pos, "illegal inheritance from sealed "+psym+": " + context.unit.source.file.canonicalPath + " != " + psym.sourceFile.canonicalPath)
          }
          if (!(selfType <:< parent.tpe.typeOfThis) &&
              !phase.erasedTypes &&
              !context.owner.isSynthetic &&   // don't check synthetic concrete classes for virtuals (part of DEVIRTUALIZE)
              !settings.noSelfCheck.value &&  // setting to suppress this very check
              !selfType.isErroneous &&
              !parent.tpe.isErroneous)
          {
            //Console.println(context.owner);//DEBUG
            //Console.println(context.owner.unsafeTypeParams);//DEBUG
            //Console.println(List.fromArray(context.owner.info.closure));//DEBUG
            error(parent.pos, "illegal inheritance;\n self-type "+
                  selfType+" does not conform to "+parent +
                  "'s selftype "+parent.tpe.typeOfThis)
            if (settings.explaintypes.value) explainTypes(selfType, parent.tpe.typeOfThis)
          }
          if (parents exists (p => p != parent && p.tpe.typeSymbol == psym && !psym.isError))
            error(parent.pos, psym+" is inherited twice")
        }
      }

      if (!parents.isEmpty && !parents.head.tpe.isError)
        for (p <- parents) validateParentClass(p, parents.head.tpe.typeSymbol)

/*
      if (settings.Xshowcls.value != "" &&
          settings.Xshowcls.value == context.owner.fullName)
        println("INFO "+context.owner+
                ", baseclasses = "+(context.owner.info.baseClasses map (_.fullName))+
                ", lin = "+(context.owner.info.baseClasses map (context.owner.thisType.baseType)))
*/
    }

    def checkFinitary(classinfo: ClassInfoType) {
      val clazz = classinfo.typeSymbol
      for (tparam <- clazz.typeParams) {
        if (classinfo.expansiveRefs(tparam) contains tparam) {
          error(tparam.pos, "class graph is not finitary because type parameter "+tparam.name+" is expansively recursive")
          val newinfo = ClassInfoType(
            classinfo.parents map (_.instantiateTypeParams(List(tparam), List(AnyRefClass.tpe))),
            classinfo.decls,
            clazz)
          clazz.setInfo {
            clazz.info match {
              case PolyType(tparams, _) => PolyType(tparams, newinfo)
              case _ => newinfo
            }
          }
        }
      }
    }

    /**
     *  @param cdef ...
     *  @return     ...
     */
    def typedClassDef(cdef: ClassDef): Tree = {
//      attributes(cdef)
      val clazz = cdef.symbol
      val typedMods = removeAnnotations(cdef.mods)
      assert(clazz != NoSymbol)
      reenterTypeParams(cdef.tparams)
      val tparams1 = cdef.tparams mapConserve (typedTypeDef)
      val impl1 = newTyper(context.make(cdef.impl, clazz, new Scope))
        .typedTemplate(cdef.impl, parentTypes(cdef.impl))
      val impl2 = finishMethodSynthesis(impl1, clazz, context)
      if ((clazz != ClassfileAnnotationClass) &&
          (clazz isNonBottomSubClass ClassfileAnnotationClass))
        restrictionWarning(cdef.pos, unit,
          "subclassing Classfile does not\n"+
          "make your annotation visible at runtime.  If that is what\n"+
          "you want, you must write the annotation class in Java.")
      if (!isPastTyper) {
        for (ann <- clazz.getAnnotation(DeprecatedAttr)) {
          val m = companionSymbolOf(clazz, context)
          if (m != NoSymbol)
            m.moduleClass.addAnnotation(AnnotationInfo(ann.atp, ann.args, List()))
        }
      }
      treeCopy.ClassDef(cdef, typedMods, cdef.name, tparams1, impl2)
        .setType(NoType)
    }

    /**
     *  @param mdef ...
     *  @return     ...
     */
    def typedModuleDef(mdef: ModuleDef): Tree = {
      // initialize all constructors of the linked class: the type completer (Namer.methodSig)
      // might add default getters to this object. example: "object T; class T(x: Int = 1)"
      val linkedClass = companionSymbolOf(mdef.symbol, context)
      if (linkedClass != NoSymbol)
        linkedClass.info.decl(nme.CONSTRUCTOR).alternatives foreach (_.initialize)

      val clazz     = mdef.symbol.moduleClass
      val typedMods = removeAnnotations(mdef.mods)
      assert(clazz != NoSymbol, mdef)

      val typer0 = newTyper(context.make(mdef.impl, clazz, new Scope))
      val impl1  = typer0.typedTemplate(mdef.impl, {
        parentTypes(mdef.impl) ++ (
          if (linkedClass == NoSymbol || !linkedClass.isSerializable || clazz.isSerializable) Nil
          else {
            clazz.makeSerializable()
            List(TypeTree(SerializableClass.tpe) setPos clazz.pos.focus)
          }
        )
      })
      val impl2  = finishMethodSynthesis(impl1, clazz, context)

      treeCopy.ModuleDef(mdef, typedMods, mdef.name, impl2) setType NoType
    }
    /** In order to override this in the TreeCheckers Typer so synthetics aren't re-added
     *  all the time, it is exposed here the module/class typing methods go through it.
     *  ...but it turns out it's also the ideal spot for namer/typer coordination for
     *  the tricky method synthesis scenarios, so we'll make it that.
     */
    protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template = {
      addSyntheticMethods(templ, clazz, context)
    }
    /** For flatMapping a list of trees when you want the DocDefs and Annotated
     *  to be transparent.
     */
    def rewrappingWrapperTrees(f: Tree => List[Tree]): Tree => List[Tree] = {
      case dd @ DocDef(comment, defn) => f(defn) map (stat => DocDef(comment, stat) setPos dd.pos)
      case Annotated(annot, defn)     => f(defn) map (stat => Annotated(annot, stat))
      case tree                       => f(tree)
    }

    protected def enterSyms(txt: Context, trees: List[Tree]) = {
      var txt0 = txt
      for (tree <- trees) txt0 = enterSym(txt0, tree)
    }

    protected def enterSym(txt: Context, tree: Tree): Context =
      if (txt eq context) namer.enterSym(tree)
      else newNamer(txt).enterSym(tree)

    /**
     *  @param templ    ...
     *  @param parents1 ...
     *    <li> <!-- 2 -->
     *      Check that inner classes do not inherit from Annotation
     *    </li>
     *  @return         ...
     */
    def typedTemplate(templ: Template, parents1: List[Tree]): Template = {
      val clazz = context.owner
      // complete lazy annotations
      val annots = clazz.annotations
      if (templ.symbol == NoSymbol)
        templ setSymbol clazz.newLocalDummy(templ.pos)
      val self1 = templ.self match {
        case vd @ ValDef(mods, name, tpt, EmptyTree) =>
          val tpt1 = checkNoEscaping.privates(
            clazz.thisSym,
            treeCopy.TypeTree(tpt).setOriginal(tpt) setType vd.symbol.tpe
          )
          treeCopy.ValDef(vd, mods, name, tpt1, EmptyTree) setType NoType
      }
      // was:
      //          val tpt1 = checkNoEscaping.privates(clazz.thisSym, typedType(tpt))
      //          treeCopy.ValDef(vd, mods, name, tpt1, EmptyTree) setType NoType
      // but this leads to cycles for existential self types ==> #2545
      if (self1.name != nme.WILDCARD)
        context.scope enter self1.symbol

      val selfType = (
        if (clazz.isAnonymousClass && !phase.erasedTypes)
          intersectionType(clazz.info.parents, clazz.owner)
        else
          clazz.typeOfThis
      )
      // the following is necessary for templates generated later
      assert(clazz.info.decls != EmptyScope, clazz)
      enterSyms(context.outer.make(templ, clazz, clazz.info.decls), templ.body)
      validateParentClasses(parents1, selfType)
      if (clazz.isCase)
        validateNoCaseAncestor(clazz)

      if ((clazz isSubClass ClassfileAnnotationClass) && !clazz.owner.isPackageClass)
        unit.error(clazz.pos, "inner classes cannot be classfile annotations")
      if (!phase.erasedTypes && !clazz.info.resultType.isError) // @S: prevent crash for duplicated type members
        checkFinitary(clazz.info.resultType.asInstanceOf[ClassInfoType])

      val body =
        if (isPastTyper || reporter.hasErrors) templ.body
        else templ.body flatMap rewrappingWrapperTrees(namer.finishGetterSetter(Typer.this, _))

      val body1 = typedStats(body, templ.symbol)
      treeCopy.Template(templ, parents1, self1, body1) setType clazz.tpe
    }

    /** Remove definition annotations from modifiers (they have been saved
     *  into the symbol's ``annotations'' in the type completer / namer)
     */
    def removeAnnotations(mods: Modifiers): Modifiers =
      mods.copy(annotations = Nil) setPositions mods.positions

    /**
     *  @param vdef ...
     *  @return     ...
     */
    def typedValDef(vdef: ValDef): ValDef = {
//      attributes(vdef)
      val sym = vdef.symbol.initialize
      val typer1 = constrTyperIf(sym.isParameter && sym.owner.isConstructor)
      val typedMods = removeAnnotations(vdef.mods)

      // complete lazy annotations
      val annots = sym.annotations
      var tpt1 = checkNoEscaping.privates(sym, typer1.typedType(vdef.tpt))
      checkNonCyclic(vdef, tpt1)

      if (sym.hasAnnotation(definitions.VolatileAttr)) {
        if (!sym.isMutable)
          error(vdef.pos, "values cannot be volatile")
        else if (sym.isFinal)
          error(vdef.pos, "final vars cannot be volatile")
      }
      val rhs1 =
        if (vdef.rhs.isEmpty) {
          if (sym.isVariable && sym.owner.isTerm && !isPastTyper)
            error(vdef.pos, "local variables must be initialized")
          vdef.rhs
        } else {
          val tpt2 = if (sym.hasDefault) {
            // When typechecking default parameter, replace all type parameters in the expected type by Wildcard.
            // This allows defining "def foo[T](a: T = 1)"
            val tparams = sym.owner.skipConstructor.info.typeParams
            val subst = new SubstTypeMap(tparams, tparams map (_ => WildcardType)) {
              override def matches(sym: Symbol, sym1: Symbol) =
                if (sym.isSkolem) matches(sym.deSkolemize, sym1)
                else if (sym1.isSkolem) matches(sym, sym1.deSkolemize)
                else super[SubstTypeMap].matches(sym, sym1)
            }
            // allow defaults on by-name parameters
            if (sym hasFlag BYNAMEPARAM)
              if (tpt1.tpe.typeArgs.isEmpty) WildcardType // during erasure tpt1 is Function0
              else subst(tpt1.tpe.typeArgs(0))
            else subst(tpt1.tpe)
          } else tpt1.tpe
          newTyper(typer1.context.make(vdef, sym)).transformedOrTyped(vdef.rhs, EXPRmode | BYVALmode, tpt2)
        }
      treeCopy.ValDef(vdef, typedMods, vdef.name, tpt1, checkDead(rhs1)) setType NoType
    }

    /** Enter all aliases of local parameter accessors.
     *
     *  @param clazz    ...
     *  @param vparamss ...
     *  @param rhs      ...
     */
    def computeParamAliases(clazz: Symbol, vparamss: List[List[ValDef]], rhs: Tree) {
      debuglog("computing param aliases for "+clazz+":"+clazz.primaryConstructor.tpe+":"+rhs)//debug
      def decompose(call: Tree): (Tree, List[Tree]) = call match {
        case Apply(fn, args) =>
          val (superConstr, args1) = decompose(fn)
          val params = fn.tpe.params
          val args2 = if (params.isEmpty || !isRepeatedParamType(params.last.tpe)) args
                      else args.take(params.length - 1) :+ EmptyTree
          assert(sameLength(args2, params), "mismatch " + clazz + " " + (params map (_.tpe)) + " " + args2)//debug
          (superConstr, args1 ::: args2)
        case Block(stats, expr) if !stats.isEmpty =>
          decompose(stats.last)
        case _ =>
          (call, List())
      }
      val (superConstr, superArgs) = decompose(rhs)
      assert(superConstr.symbol ne null)//debug

      // an object cannot be allowed to pass a reference to itself to a superconstructor
      // because of initialization issues; bug #473
      for (arg <- superArgs ; tree <- arg) {
        val sym = tree.symbol
        if (sym != null && (sym.info.baseClasses contains clazz)) {
          if (sym.isModule)
            error(tree.pos, "super constructor cannot be passed a self reference unless parameter is declared by-name")
          tree match {
            case This(qual) =>
              error(tree.pos, "super constructor arguments cannot reference unconstructed `this`")
            case _ => ()
          }
        }
      }

      if (superConstr.symbol.isPrimaryConstructor) {
        val superClazz = superConstr.symbol.owner
        if (!superClazz.isJavaDefined) {
          val superParamAccessors = superClazz.constrParamAccessors
          if (sameLength(superParamAccessors, superArgs)) {
            for ((superAcc, superArg @ Ident(name)) <- superParamAccessors zip superArgs) {
              if (vparamss.exists(_.exists(_.symbol == superArg.symbol))) {
                var alias = superAcc.initialize.alias
                if (alias == NoSymbol)
                  alias = superAcc.getter(superAcc.owner)
                if (alias != NoSymbol &&
                    superClazz.info.nonPrivateMember(alias.name) != alias)
                  alias = NoSymbol
                if (alias != NoSymbol) {
                  var ownAcc = clazz.info.decl(name).suchThat(_.isParamAccessor)
                  if ((ownAcc hasFlag ACCESSOR) && !ownAcc.isDeferred)
                    ownAcc = ownAcc.accessed
                  if (!ownAcc.isVariable && !alias.accessed.isVariable) {
                    debuglog("" + ownAcc + " has alias "+alias.fullLocationString) //debug
                    ownAcc.asInstanceOf[TermSymbol].setAlias(alias)
                  }
                }
              }
            }
          }
        }
      }
    }

    /** Check if a structurally defined method violates implementation restrictions.
     *  A method cannot be called if it is a non-private member of a refinement type
     *  and if its parameter's types are any of:
     *    - the self-type of the refinement
     *    - a type member of the refinement
     *    - an abstract type declared outside of the refinement.
     */
    def checkMethodStructuralCompatible(meth: Symbol): Unit = {
      def fail(msg: String) = unit.error(meth.pos, msg)
      val tp: Type = meth.tpe match {
        case mt @ MethodType(_, _)     => mt
        case NullaryMethodType(restpe) => restpe  // TODO_NMT: drop NullaryMethodType from resultType?
        case PolyType(_, restpe)       => restpe
        case _                         => NoType
      }

      for (paramType <- tp.paramTypes) {
        val sym = paramType.typeSymbol

        if (sym.isAbstractType) {
          if (!sym.hasTransOwner(meth.owner))
            fail("Parameter type in structural refinement may not refer to an abstract type defined outside that refinement")
          else if (!sym.hasTransOwner(meth))
            fail("Parameter type in structural refinement may not refer to a type member of that refinement")
        }
        if (paramType.isInstanceOf[ThisType] && sym == meth.owner)
          fail("Parameter type in structural refinement may not refer to the type of that refinement (self type)")
      }
    }
    def typedUseCase(useCase: UseCase) {
      def stringParser(str: String): syntaxAnalyzer.Parser = {
        val file = new BatchSourceFile(context.unit.source.file, str) {
          override def positionInUltimateSource(pos: Position) = {
            pos.withSource(context.unit.source, useCase.pos.start)
          }
        }
        val unit = new CompilationUnit(file)
        new syntaxAnalyzer.UnitParser(unit)
      }
      val trees = stringParser(useCase.body+";").nonLocalDefOrDcl
      val enclClass = context.enclClass.owner
      def defineAlias(name: Name) =
        if (context.scope.lookup(name) == NoSymbol) {
          lookupVariable(name.toString.substring(1), enclClass) match {
            case Some(repl) =>
              silent(_.typedTypeConstructor(stringParser(repl).typ())) match {
                case tpt: Tree =>
                  val alias = enclClass.newAliasType(useCase.pos, name.toTypeName)
                  val tparams = cloneSymbolsAtOwner(tpt.tpe.typeSymbol.typeParams, alias)
                  alias setInfo typeFun(tparams, appliedType(tpt.tpe, tparams map (_.tpe)))
                  context.scope.enter(alias)
                case _ =>
              }
            case _ =>
          }
        }
      for (tree <- trees; t <- tree)
        t match {
          case Ident(name) if name startsWith '$' => defineAlias(name)
          case _ =>
        }
      useCase.aliases = context.scope.toList
      namer.enterSyms(trees)
      typedStats(trees, NoSymbol)
      useCase.defined = context.scope.toList filterNot (useCase.aliases contains _)
      if (settings.debug.value)
        useCase.defined foreach (sym => println("defined use cases: %s:%s".format(sym, sym.tpe)))
    }

    /**
     *  @param ddef ...
     *  @return     ...
     */
    def typedDefDef(ddef: DefDef): DefDef = {
      val meth = ddef.symbol.initialize

      reenterTypeParams(ddef.tparams)
      reenterValueParams(ddef.vparamss)

      // for `val` and `var` parameter, look at `target` meta-annotation
      if (!isPastTyper && meth.isPrimaryConstructor) {
        for (vparams <- ddef.vparamss; vd <- vparams) {
          if (vd.mods.isParamAccessor) {
            namer.validateParam(vd)
          }
        }
      }

      val tparams1 = ddef.tparams mapConserve typedTypeDef
      val vparamss1 = ddef.vparamss mapConserve (_ mapConserve typedValDef)

      // complete lazy annotations
      val annots = meth.annotations

      for (vparams1 <- vparamss1; vparam1 <- vparams1 dropRight 1)
        if (isRepeatedParamType(vparam1.symbol.tpe))
          error(vparam1.pos, "*-parameter must come last")

      var tpt1 = checkNoEscaping.privates(meth, typedType(ddef.tpt))
      checkNonCyclic(ddef, tpt1)
      ddef.tpt.setType(tpt1.tpe)
      val typedMods = removeAnnotations(ddef.mods)
      var rhs1 =
        if (ddef.name == nme.CONSTRUCTOR && !ddef.symbol.hasStaticFlag) { // need this to make it possible to generate static ctors
          if (!meth.isPrimaryConstructor &&
              (!meth.owner.isClass ||
               meth.owner.isModuleClass ||
               meth.owner.isAnonOrRefinementClass))
            error(ddef.pos, "constructor definition not allowed here")
          typed(ddef.rhs)
        } else if (meth.isMacro) {
          EmptyTree
        } else {
          transformedOrTyped(ddef.rhs, EXPRmode, tpt1.tpe)
        }

      if (meth.isPrimaryConstructor && meth.isClassConstructor && !isPastTyper && !reporter.hasErrors)
        computeParamAliases(meth.owner, vparamss1, rhs1)
      if (tpt1.tpe.typeSymbol != NothingClass && !context.returnsSeen && rhs1.tpe.typeSymbol != NothingClass)
        rhs1 = checkDead(rhs1)

      if (!isPastTyper && meth.owner.isClass &&
          meth.paramss.exists(ps => ps.exists(_.hasDefaultFlag) && isRepeatedParamType(ps.last.tpe)))
        error(meth.pos, "a parameter section with a `*'-parameter is not allowed to have default arguments")

      if (!isPastTyper) {
        val allParams = meth.paramss.flatten
        for (p <- allParams) {
          for (n <- p.deprecatedParamName) {
            if (allParams.exists(p1 => p1.name == n || (p != p1 && p1.deprecatedParamName.exists(_ == n))))
              error(p.pos, "deprecated parameter name "+ n +" has to be distinct from any other parameter name (deprecated or not).")
          }
        }
      }
      if (meth.isStructuralRefinementMember)
        checkMethodStructuralCompatible(meth)

      treeCopy.DefDef(ddef, typedMods, ddef.name, tparams1, vparamss1, tpt1, rhs1) setType NoType
    }

    def typedTypeDef(tdef: TypeDef): TypeDef = {
      def typeDefTyper = {
        if(tdef.tparams isEmpty) Typer.this
        else newTyper(context.makeNewScope(tdef, tdef.symbol))
      }
      typeDefTyper.typedTypeDef0(tdef)
    }

    // call typedTypeDef instead
    // a TypeDef with type parameters must always be type checked in a new scope
    private def typedTypeDef0(tdef: TypeDef): TypeDef = {
      tdef.symbol.initialize
      reenterTypeParams(tdef.tparams)
      val tparams1 = tdef.tparams mapConserve typedTypeDef
      val typedMods = removeAnnotations(tdef.mods)
      // complete lazy annotations
      val annots = tdef.symbol.annotations

      // @specialized should not be pickled when compiling with -no-specialize
      if (settings.nospecialization.value && currentRun.compiles(tdef.symbol)) {
        tdef.symbol.removeAnnotation(definitions.SpecializedClass)
        tdef.symbol.deSkolemize.removeAnnotation(definitions.SpecializedClass)
      }

      val rhs1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.rhs))
      checkNonCyclic(tdef.symbol)
      if (tdef.symbol.owner.isType)
        rhs1.tpe match {
          case TypeBounds(lo1, hi1) =>
            if (!(lo1 <:< hi1))
              error(tdef.pos, "lower bound "+lo1+" does not conform to upper bound "+hi1)
          case _ =>
        }
      treeCopy.TypeDef(tdef, typedMods, tdef.name, tparams1, rhs1) setType NoType
    }

    private def enterLabelDef(stat: Tree) {
      stat match {
        case ldef @ LabelDef(_, _, _) =>
          if (ldef.symbol == NoSymbol)
            ldef.symbol = namer.enterInScope(
              context.owner.newLabel(ldef.pos, ldef.name) setInfo MethodType(List(), UnitClass.tpe))
        case _ =>
      }
    }

    def typedLabelDef(ldef: LabelDef): LabelDef = {
      if (!nme.isLoopHeaderLabel(ldef.symbol.name) || isPastTyper) {
        val restpe = ldef.symbol.tpe.resultType
        val rhs1 = typed(ldef.rhs, restpe)
        ldef.params foreach (param => param.tpe = param.symbol.tpe)
        treeCopy.LabelDef(ldef, ldef.name, ldef.params, rhs1) setType restpe
      } else {
        val initpe = ldef.symbol.tpe.resultType
        val rhs1 = typed(ldef.rhs)
        val restpe = rhs1.tpe
        if (restpe == initpe) { // stable result, no need to check again
          ldef.params foreach (param => param.tpe = param.symbol.tpe)
          treeCopy.LabelDef(ldef, ldef.name, ldef.params, rhs1) setType restpe
        } else {
          context.scope.unlink(ldef.symbol)
          val sym2 = namer.enterInScope(
            context.owner.newLabel(ldef.pos, ldef.name) setInfo MethodType(List(), restpe))
          val rhs2 = typed(resetAllAttrs(ldef.rhs), restpe)
          ldef.params foreach (param => param.tpe = param.symbol.tpe)
          treeCopy.LabelDef(ldef, ldef.name, ldef.params, rhs2) setSymbol sym2 setType restpe
        }
      }
    }

    /**
     *  @param block ...
     *  @param mode  ...
     *  @param pt    ...
     *  @return      ...
     */
    def typedBlock(block: Block, mode: Int, pt: Type): Block = {
      val syntheticPrivates = new ListBuffer[Symbol]
      try {
        namer.enterSyms(block.stats)
        for (stat <- block.stats) enterLabelDef(stat)

        if (phaseId(currentPeriod) <= currentRun.typerPhase.id) {
          // This is very tricky stuff, because we are navigating the Skylla and Charybdis of
          // anonymous classes and what to return from them here. On the one hand, we cannot admit
          // every non-private member of an anonymous class as a part of the structural type of the
          // enclosing block. This runs afoul of the restriction that a structural type may not
          // refer to an enclosing type parameter or abstract types (which in turn is necessitated
          // by what can be done in Java reflection). On the other hand, making every term member
          // private conflicts with private escape checking - see ticket #3174 for an example.
          //
          // The cleanest way forward is if we would find a way to suppress structural type checking
          // for these members and maybe defer type errors to the places where members are called.
          // But that would be a big refactoring and also a big departure from existing code. The
          // probably safest fix for 2.8 is to keep members of an anonymous class that are not
          // mentioned in a parent type private (as before) but to disable escape checking for code
          // that's in the same anonymous class. That's what's done here.
          //
          // We really should go back and think hard whether we find a better way to address the
          // problem of escaping idents on the one hand and well-formed structural types on the
          // other.
          block match {
            case Block(List(classDef @ ClassDef(_, _, _, _)), Apply(Select(New(_), _), _)) =>
              val classDecls = classDef.symbol.info.decls
              val visibleMembers = pt match {
                case WildcardType                           => classDecls.toList
                case BoundedWildcardType(TypeBounds(lo, _)) => lo.members
                case _                                      => pt.members
              }
              def matchesVisibleMember(member: Symbol) = visibleMembers exists { vis =>
                (member.name == vis.name) &&
                (member.tpe <:< vis.tpe.substThis(vis.owner, classDef.symbol))
              }
              // The block is an anonymous class definitions/instantiation pair
              //   -> members that are hidden by the type of the block are made private
              val toHide = (
                classDecls filter (member =>
                     member.isTerm
                  && member.isPossibleInRefinement
                  && member.isPublic
                  && !matchesVisibleMember(member)
                ) map (member => member
                  resetFlag (PROTECTED | LOCAL)
                  setFlag (PRIVATE | SYNTHETIC_PRIVATE)
                  setPrivateWithin NoSymbol
                )
              )
              syntheticPrivates ++= toHide
            case _ =>
          }
        }
        val stats1 = typedStats(block.stats, context.owner)
        val expr1 = typed(block.expr, mode & ~(FUNmode | QUALmode), pt)
        treeCopy.Block(block, stats1, expr1)
          .setType(if (treeInfo.isExprSafeToInline(block)) expr1.tpe else expr1.tpe.deconst)
      } finally {
        // enable escaping privates checking from the outside and recycle
        // transient flag
        syntheticPrivates foreach (_ resetFlag SYNTHETIC_PRIVATE)
      }
    }

    /**
     *  @param cdef   ...
     *  @param pattpe ...
     *  @param pt     ...
     *  @return       ...
     */
    def typedCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef = {
      // verify no _* except in last position
      for (Apply(_, xs) <- cdef.pat ; x <- xs dropRight 1 ; if treeInfo isStar x)
        error(x.pos, "_* may only come last")

      val pat1: Tree = typedPattern(cdef.pat, pattpe)
      // When case classes have more than two parameter lists, the pattern ends
      // up typed as a method.  We only pattern match on the first parameter
      // list, so substitute the final result type of the method, i.e. the type
      // of the case class.
      if (pat1.tpe.paramSectionCount > 0)
        pat1 setType pat1.tpe.finalResultType

      if (forInteractive) {
        for (bind @ Bind(name, _) <- cdef.pat)
          if (name.toTermName != nme.WILDCARD && bind.symbol != null && bind.symbol != NoSymbol)
            namer.enterIfNotThere(bind.symbol)
      }

      val guard1: Tree = if (cdef.guard == EmptyTree) EmptyTree
                         else typed(cdef.guard, BooleanClass.tpe)
      var body1: Tree = typed(cdef.body, pt)
      if (!context.savedTypeBounds.isEmpty) {
        body1.tpe = context.restoreTypeBounds(body1.tpe)
        if (isFullyDefined(pt) && !(body1.tpe <:< pt)) {
          // @M no need for pt.normalize here, is done in erasure
          body1 = typedPos(body1.pos)(gen.mkCast(body1, pt))
        }
      }
//    body1 = checkNoEscaping.locals(context.scope, pt, body1)
      treeCopy.CaseDef(cdef, pat1, guard1, body1) setType body1.tpe
    }

    def typedCases(tree: Tree, cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef] =
      cases mapConserve { cdef =>
        newTyper(context.makeNewScope(cdef, context.owner)).typedCase(cdef, pattp, pt)
      }

    /**
     *  @param fun  ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def typedFunction(fun: Function, mode: Int, pt: Type): Tree = {
      val numVparams = fun.vparams.length
      val codeExpected = !forMSIL && (pt.typeSymbol isNonBottomSubClass CodeClass)

      if (numVparams > definitions.MaxFunctionArity)
        return errorTree(fun, "implementation restricts functions to " + definitions.MaxFunctionArity + " parameters")

      def decompose(pt: Type): (Symbol, List[Type], Type) =
        if ((isFunctionType(pt)
             ||
             pt.typeSymbol == PartialFunctionClass &&
             numVparams == 1 && fun.body.isInstanceOf[Match])
             && // see bug901 for a reason why next conditions are needed
            (pt.normalize.typeArgs.length - 1 == numVparams
             ||
             fun.vparams.exists(_.tpt.isEmpty)))
          (pt.typeSymbol, pt.normalize.typeArgs.init, pt.normalize.typeArgs.last)
        else
          (FunctionClass(numVparams), fun.vparams map (x => NoType), WildcardType)

      val (clazz, argpts, respt) = decompose(if (codeExpected) pt.normalize.typeArgs.head else pt)

      if (argpts.lengthCompare(numVparams) != 0)
        errorTree(fun, "wrong number of parameters; expected = " + argpts.length)
      else {
        val vparamSyms = (fun.vparams, argpts).zipped map { (vparam, argpt) =>
          if (vparam.tpt.isEmpty) {
            vparam.tpt.tpe =
              if (isFullyDefined(argpt)) argpt
              else {
                fun match {
                  case etaExpansion(vparams, fn, args) if !codeExpected =>
                    silent(_.typed(fn, forFunMode(mode), pt)) match {
                      case fn1: Tree if context.undetparams.isEmpty =>
                        // if context,undetparams is not empty, the function was polymorphic,
                        // so we need the missing arguments to infer its type. See #871
                        //println("typing eta "+fun+":"+fn1.tpe+"/"+context.undetparams)
                        val ftpe = normalize(fn1.tpe) baseType FunctionClass(numVparams)
                        if (isFunctionType(ftpe) && isFullyDefined(ftpe))
                          return typedFunction(fun, mode, ftpe)
                      case _ =>
                    }
                  case _ =>
                }
                error(vparam.pos, missingParameterTypeMsg(fun, vparam, pt))
                ErrorType
              }
            if (!vparam.tpt.pos.isDefined) vparam.tpt setPos vparam.pos.focus
          }
          enterSym(context, vparam)
          if (context.retyping) context.scope enter vparam.symbol
          vparam.symbol
        }

        val vparams = fun.vparams mapConserve (typedValDef)
//        for (vparam <- vparams) {
//          checkNoEscaping.locals(context.scope, WildcardType, vparam.tpt); ()
//        }
        var body = typed(fun.body, respt)
        val formals = vparamSyms map (_.tpe)
        val restpe = packedType(body, fun.symbol).deconst.resultType
        val funtpe = typeRef(clazz.tpe.prefix, clazz, formals :+ restpe)
//        body = checkNoEscaping.locals(context.scope, restpe, body)
        val fun1 = treeCopy.Function(fun, vparams, body).setType(funtpe)
        if (codeExpected) lifted(fun1) else fun1
      }
    }

    def lifted(tree: Tree): Tree = typedPos(tree.pos) {
      Apply(Select(Ident(CodeModule), nme.lift_), List(tree))
    }

    def typedRefinement(stats: List[Tree]) {
      namer.enterSyms(stats)
      // need to delay rest of typedRefinement to avoid cyclic reference errors
      unit.toCheck += { () =>
        // go to next outer context which is not silent, see #3614
        var c = context
        while (!c.reportGeneralErrors) c = c.outer
        val stats1 = newTyper(c).typedStats(stats, NoSymbol)
        for (stat <- stats1 if stat.isDef) {
          val member = stat.symbol
          if (!(context.owner.ancestors forall
                (bc => member.matchingSymbol(bc, context.owner.thisType) == NoSymbol))) {
                  member setFlag OVERRIDE
                }
        }
      }
    }

    def typedImport(imp : Import) : Import = (transformed remove imp) match {
      case Some(imp1: Import) => imp1
      case None => log("unhandled import: "+imp+" in "+unit); imp
    }

    private def isWarnablePureExpression(tree: Tree) = tree match {
      case EmptyTree | Literal(Constant(())) => false
      case _                                 =>
        (treeInfo isExprSafeToInline tree) && {
          val sym = tree.symbol
          (sym == null) || !(sym.isModule || sym.isLazy) || {
            debuglog("'Pure' but side-effecting expression in statement position: " + tree)
            false
          }
        }
    }

    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val inBlock = exprOwner == context.owner
      def includesTargetPos(tree: Tree) =
        tree.pos.isRange && context.unit.exists && (tree.pos includes context.unit.targetPos)
      val localTarget = stats exists includesTargetPos
      def typedStat(stat: Tree): Tree = {
        if (context.owner.isRefinementClass && !treeInfo.isDeclarationOrTypeDef(stat))
          errorTree(stat, "only declarations allowed here")
        else
          stat match {
            case imp @ Import(_, _) =>
              context = context.makeNewImport(imp)
              imp.symbol.initialize
              typedImport(imp)
            case _ =>
              if (localTarget && !includesTargetPos(stat)) {
                // skip typechecking of statements in a sequence where some other statement includes
                // the targetposition
                stat
              } else {
                val localTyper = if (inBlock || (stat.isDef && !stat.isInstanceOf[LabelDef])) this
                                 else newTyper(context.make(stat, exprOwner))
                // XXX this creates a spurious dead code warning if an exception is thrown
                // in a constructor, even if it is the only thing in the constructor.
                val result = checkDead(localTyper.typed(stat, EXPRmode | BYVALmode, WildcardType))
                if (treeInfo.isSelfOrSuperConstrCall(result)) {
                  context.inConstructorSuffix = true
                  if (treeInfo.isSelfConstrCall(result) && result.symbol.pos.pointOrElse(0) >= exprOwner.enclMethod.pos.pointOrElse(0))
                    error(stat.pos, "called constructor's definition must precede calling constructor's definition")
                }
                if (isWarnablePureExpression(result)) context.warning(stat.pos,
                  "a pure expression does nothing in statement position; " +
                  "you may be omitting necessary parentheses"
                )
                result
              }
          }
      }

      /** 'accessor' and 'accessed' are so similar it becomes very difficult to
       *  follow the logic, so I renamed one to something distinct.
       */
      def accesses(looker: Symbol, accessed: Symbol) = accessed.hasLocalFlag && (
           (accessed.isParamAccessor)
        || (looker.hasAccessorFlag && !accessed.hasAccessorFlag && accessed.isPrivate)
      )

      def checkNoDoubleDefsAndAddSynthetics(stats: List[Tree]): List[Tree] = {
        val scope = if (inBlock) context.scope else context.owner.info.decls
        var newStats = new ListBuffer[Tree]
        var needsCheck = true
        var moreToAdd = true
        while (moreToAdd) {
          val initSize = scope.size
          var e = scope.elems
          while ((e ne null) && e.owner == scope) {

            // check no double def
            if (needsCheck) {
              var e1 = scope.lookupNextEntry(e)
              while ((e1 ne null) && e1.owner == scope) {
                if (!accesses(e.sym, e1.sym) && !accesses(e1.sym, e.sym) &&
                    (e.sym.isType || inBlock || (e.sym.tpe matches e1.sym.tpe) || e.sym.isMacro && e1.sym.isMacro))
                  // default getters are defined twice when multiple overloads have defaults. an
                  // error for this is issued in RefChecks.checkDefaultsInOverloaded
                  if (!e.sym.isErroneous && !e1.sym.isErroneous && !e.sym.hasDefaultFlag &&
                      !e.sym.hasAnnotation(BridgeClass) && !e1.sym.hasAnnotation(BridgeClass)) {
                    error(e.sym.pos, e1.sym+" is defined twice"+
                    {if(!settings.debug.value) "" else " in "+unit.toString}+
                    {if (e.sym.isMacro && e1.sym.isMacro) " \n(note that macros cannot be overloaded)" else ""})
                    scope.unlink(e1) // need to unlink to avoid later problems with lub; see #2779
                  }
                e1 = scope.lookupNextEntry(e1)
              }
            }

          // add synthetics
          context.unit.synthetics get e.sym foreach { tree =>
            newStats += typedStat(tree) // might add even more synthetics to the scope
            context.unit.synthetics -= e.sym
          }

          e = e.next
        }
        needsCheck = false
        // the type completer of a synthetic might add more synthetics. example: if the
        // factory method of a case class (i.e. the constructor) has a default.
        moreToAdd = initSize != scope.size
        }
        if (newStats.isEmpty) stats
        else {
          // put default getters next to the method they belong to,
          // same for companion objects. fixes #2489 and #4036.
          def matches(stat: Tree, synt: Tree) = (stat, synt) match {
            case (DefDef(_, statName, _, _, _, _), DefDef(mods, syntName, _, _, _, _)) =>
              mods.hasDefaultFlag && syntName.toString.startsWith(statName.toString)

            case (ClassDef(_, className, _, _), ModuleDef(_, moduleName, _)) =>
              className.toTermName == moduleName

            case _ => false
          }

          def matching(stat: Tree): List[Tree] = {
            val (pos, neg) = newStats.partition(synt => matches(stat, synt))
            newStats = neg
            pos.toList
          }

          (stats foldRight List[Tree]())((stat, res) => {
            stat :: matching(stat) ::: res
          }) ::: newStats.toList
        }
      }
      val result = stats mapConserve typedStat
      if (phase.erasedTypes) result
      else checkNoDoubleDefsAndAddSynthetics(result)
    }

    def typedArg(arg: Tree, mode: Int, newmode: Int, pt: Type): Tree = {
      val typedMode = onlyStickyModes(mode) | newmode
      val t = constrTyperIf((mode & SCCmode) != 0).typed(arg, typedMode, pt)
      checkDead.inMode(typedMode, t)
    }

    def typedArgs(args: List[Tree], mode: Int) =
      args mapConserve (arg => typedArg(arg, mode, 0, WildcardType))

    def typedArgs(args0: List[Tree], mode: Int, formals0: List[Type], adapted0: List[Type]): List[Tree] = {
      val sticky = onlyStickyModes(mode)
      def loop(args: List[Tree], formals: List[Type], adapted: List[Type]): List[Tree] = {
        if (args.isEmpty || adapted.isEmpty) Nil
        else {
          // No formals left or * indicates varargs.
          val isVarArgs = formals.isEmpty || formals.tail.isEmpty && isRepeatedParamType(formals.head)
          val typedMode = sticky | (
            if (isVarArgs) STARmode | BYVALmode
            else if (isByNameParamType(formals.head)) 0
            else BYVALmode
          )
          val tree = typedArg(args.head, mode, typedMode, adapted.head)
          // formals may be empty, so don't call tail
          tree :: loop(args.tail, formals drop 1, adapted.tail)
        }
      }
      loop(args0, formals0, adapted0)
    }

    /** Does function need to be instantiated, because a missing parameter
     *  in an argument closure overlaps with an uninstantiated formal?
     */
    def needsInstantiation(tparams: List[Symbol], formals: List[Type], args: List[Tree]) = {
      def isLowerBounded(tparam: Symbol) = !tparam.info.bounds.lo.typeSymbol.isBottomClass

      (formals, args).zipped exists {
        case (formal, Function(vparams, _)) =>
          (vparams exists (_.tpt.isEmpty)) &&
          vparams.length <= MaxFunctionArity &&
          (formal baseType FunctionClass(vparams.length) match {
            case TypeRef(_, _, formalargs) =>
              (formalargs, vparams).zipped.exists ((formalarg, vparam) =>
                vparam.tpt.isEmpty && (tparams exists (formalarg contains))) &&
              (tparams forall isLowerBounded)
            case _ =>
              false
          })
        case _ =>
          false
      }
    }

    /** Is `tree` a block created by a named application?
     */
    def isNamedApplyBlock(tree: Tree) =
      context.namedApplyBlockInfo exists (_._1 == tree)

    def callToCompanionConstr(context: Context, calledFun: Symbol) = {
      calledFun.isConstructor && {
        val methCtx = context.enclMethod
        (methCtx != NoContext) && {
          val contextFun = methCtx.tree.symbol
          contextFun.isPrimaryConstructor && contextFun.owner.isModuleClass &&
          companionSymbolOf(calledFun.owner, context).moduleClass == contextFun.owner
        }
      }
    }

    def doTypedApply(tree: Tree, fun0: Tree, args: List[Tree], mode: Int, pt: Type): Tree = {
      // TODO_NMT: check the assumption that args nonEmpty
      def errTree = setError(treeCopy.Apply(tree, fun0, args))
      def errorTree(msg: String) = { error(tree.pos, msg); errTree }

      var fun = fun0
      if (fun.hasSymbol && fun.symbol.isOverloaded) {
        // remove alternatives with wrong number of parameters without looking at types.
        // less expensive than including them in inferMethodAlternatvie (see below).
        def shapeType(arg: Tree): Type = arg match {
          case Function(vparams, body) =>
            functionType(vparams map (vparam => AnyClass.tpe), shapeType(body))
          case AssignOrNamedArg(Ident(name), rhs) =>
            NamedType(name, shapeType(rhs))
          case _ =>
            NothingClass.tpe
        }
        val argtypes = args map shapeType
        val pre = fun.symbol.tpe.prefix

        var sym = fun.symbol filter { alt =>
          // must use pt as expected type, not WildcardType (a tempting quick fix to #2665)
          // now fixed by using isWeaklyCompatible in exprTypeArgs
          // TODO: understand why exactly -- some types were not inferred anymore (`ant clean quick.bin` failed)
          // (I had expected inferMethodAlternative to pick up the slack introduced by using WildcardType here)
          //
          // @PP responds: I changed it to pass WildcardType instead of pt and only one line in
          // trunk (excluding scalacheck, which had another) failed to compile. It was this line in
          // Types: "refs = Array(Map(), Map())".  I determined that inference fails if there are at
          // least two invariant type parameters. See the test case I checked in to help backstop:
          // pos/isApplicableSafe.scala.
          isApplicableSafe(context.undetparams, followApply(pre.memberType(alt)), argtypes, pt)
        }
        if (sym.isOverloaded) {
          val sym1 = sym filter (alt => {
            // eliminate functions that would result from tupling transforms
            // keeps alternatives with repeated params
            hasExactlyNumParams(followApply(alt.tpe), argtypes.length) ||
            // also keep alts which define at least one default
            alt.tpe.paramss.exists(_.exists(_.hasDefault))
          })
          if (sym1 != NoSymbol) sym = sym1
        }
        if (sym != NoSymbol)
          fun = adapt(fun setSymbol sym setType pre.memberType(sym), forFunMode(mode), WildcardType)
      }

      fun.tpe match {
        case OverloadedType(pre, alts) =>
          val undetparams = context.extractUndetparams()

          val argtpes = new ListBuffer[Type]
          val amode = forArgMode(fun, mode)
          val args1 = args map {
            case arg @ AssignOrNamedArg(Ident(name), rhs) =>
              // named args: only type the righthand sides ("unknown identifier" errors otherwise)
              val rhs1 = typedArg(rhs, amode, BYVALmode, WildcardType)
              argtpes += NamedType(name, rhs1.tpe.deconst)
              // the assign is untyped; that's ok because we call doTypedApply
              atPos(arg.pos) { new AssignOrNamedArg(arg.lhs , rhs1) }
            case arg =>
              val arg1 = typedArg(arg, amode, BYVALmode, WildcardType)
              argtpes += arg1.tpe.deconst
              arg1
          }
          context.undetparams = undetparams
          inferMethodAlternative(fun, undetparams, argtpes.toList, pt, varArgsOnly = treeInfo.isWildcardStarArgList(args))
          doTypedApply(tree, adapt(fun, forFunMode(mode), WildcardType), args1, mode, pt)

        case mt @ MethodType(params, _) =>
          val paramTypes = mt.paramTypes
          // repeat vararg as often as needed, remove by-name
          val formals = formalTypes(paramTypes, args.length)

          /** Try packing all arguments into a Tuple and apply `fun`
           *  to that. This is the last thing which is tried (after
           *  default arguments)
           */
          def tryTupleApply: Option[Tree] = {
            // if 1 formal, 1 arg (a tuple), otherwise unmodified args
            val tupleArgs = actualArgs(tree.pos.makeTransparent, args, formals.length)

            if (!sameLength(tupleArgs, args) && !isUnitForVarArgs(args, params)) {
              // expected one argument, but got 0 or >1 ==>  try applying to tuple
              // the inner "doTypedApply" does "extractUndetparams" => restore when it fails
              val savedUndetparams = context.undetparams
              silent(_.doTypedApply(tree, fun, tupleArgs, mode, pt)) match {
                case t: Tree =>
                  // Depending on user options, may warn or error here if
                  // a Unit or tuple was inserted.
                  Some(t) filter (tupledTree =>
                       !inExprModeButNot(mode, FUNmode)
                    || tupledTree.symbol == null
                    || checkValidAdaptation(tupledTree, args)
                  )
                case ex =>
                  context.undetparams = savedUndetparams
                  None
              }
            } else None
          }

          /** Treats an application which uses named or default arguments.
           *  Also works if names + a vararg used: when names are used, the vararg
           *  parameter has to be specified exactly once. Note that combining varargs
           *  and defaults is ruled out by typedDefDef.
           */
          def tryNamesDefaults: Tree = {
            val lencmp = compareLengths(args, formals)

            if (mt.isErroneous) errTree
            else if (inPatternMode(mode))
              // #2064
              errorTree("wrong number of arguments for "+ treeSymTypeMsg(fun))
            else if (lencmp > 0) {
              tryTupleApply getOrElse errorTree("too many arguments for "+treeSymTypeMsg(fun))
            } else if (lencmp == 0) {
              // we don't need defaults. names were used, so this application is transformed
              // into a block (@see transformNamedApplication in NamesDefaults)
              val (namelessArgs, argPos) = removeNames(Typer.this)(args, params)
              if (namelessArgs exists (_.isErroneous)) {
                errTree
              } else if (!isIdentity(argPos) && !sameLength(formals, params))
                // !isIdentity indicates that named arguments are used to re-order arguments
                errorTree("when using named arguments, the vararg parameter has to be specified exactly once")
              else if (isIdentity(argPos) && !isNamedApplyBlock(fun)) {
                // if there's no re-ordering, and fun is not transformed, no need to transform
                // more than an optimization, e.g. important in "synchronized { x = update-x }"
                doTypedApply(tree, fun, namelessArgs, mode, pt)
              } else {
                transformNamedApplication(Typer.this, mode, pt)(
                                          treeCopy.Apply(tree, fun, namelessArgs), argPos)
              }
            } else {
              // defaults are needed. they are added to the argument list in named style as
              // calls to the default getters. Example:
              //  foo[Int](a)()  ==>  foo[Int](a)(b = foo$qual.foo$default$2[Int](a))
              val fun1 = transformNamedApplication(Typer.this, mode, pt)(fun, x => x)
              if (fun1.isErroneous) errTree
              else {
                assert(isNamedApplyBlock(fun1), fun1)
                val NamedApplyInfo(qual, targs, previousArgss, _) = context.namedApplyBlockInfo.get._2
                val blockIsEmpty = fun1 match {
                  case Block(Nil, _) =>
                    // if the block does not have any ValDef we can remove it. Note that the call to
                    // "transformNamedApplication" is always needed in order to obtain targs/previousArgss
                    context.namedApplyBlockInfo = None
                    true
                  case _ => false
                }
                val (allArgs, missing) = addDefaults(args, qual, targs, previousArgss, params, fun.pos.focus, context)
                val funSym = fun1 match { case Block(_, expr) => expr.symbol }
                val lencmp2 = compareLengths(allArgs, formals)

                if (!sameLength(allArgs, args) && callToCompanionConstr(context, funSym)) {
                  errorTree("module extending its companion class cannot use default constructor arguments")
                } else if (lencmp2 > 0) {
                  removeNames(Typer.this)(allArgs, params) // #3818
                  errTree
                } else if (lencmp2 == 0) {
                  // useful when a default doesn't match parameter type, e.g. def f[T](x:T="a"); f[Int]()
                  val note = "Error occurred in an application involving default arguments."
                  if (!(context.diagnostic contains note)) context.diagnostic = note :: context.diagnostic
                  doTypedApply(tree, if (blockIsEmpty) fun else fun1, allArgs, mode, pt)
                } else {
                  tryTupleApply getOrElse errorTree(notEnoughArgumentsMsg(fun, missing))
                }
              }
            }
          }

          if (!sameLength(formals, args) ||   // wrong nb of arguments
              (args exists isNamed) ||        // uses a named argument
              isNamedApplyBlock(fun)) {       // fun was transformed to a named apply block =>
                                              // integrate this application into the block
            tryNamesDefaults
          } else {
            val tparams = context.extractUndetparams()
            if (tparams.isEmpty) { // all type params are defined
              // In order for checkDead not to be misled by the unfortunate special
              // case of AnyRef#synchronized (which is implemented with signature T => T
              // but behaves as if it were (=> T) => T) we need to know what is the actual
              // target of a call.  Since this information is no longer available from
              // typedArg, it is recorded here.
              checkDead.updateExpr(fun)
              val args1 = typedArgs(args, forArgMode(fun, mode), paramTypes, formals)
              // instantiate dependent method types, must preserve singleton types where possible (stableTypeFor) -- example use case:
              // val foo = "foo"; def precise(x: String)(y: x.type): x.type = {...}; val bar : foo.type = precise(foo)(foo)
              // precise(foo) : foo.type => foo.type
              val restpe = mt.resultType(args1 map (arg => gen.stableTypeFor(arg) getOrElse arg.tpe))
              def ifPatternSkipFormals(tp: Type) = tp match {
                case MethodType(_, rtp) if (inPatternMode(mode)) => rtp
                case _ => tp
              }

              // Replace the Delegate-Chainer methods += and -= with corresponding
              // + and - calls, which are translated in the code generator into
              // Combine and Remove
              if (forMSIL) {
                fun match {
                  case Select(qual, name) =>
                   if (isSubType(qual.tpe, DelegateClass.tpe)
                      && (name == encode("+=") || name == encode("-=")))
                     {
                       val n = if (name == encode("+=")) nme.PLUS else nme.MINUS
                       val f = Select(qual, n)
                       // the compiler thinks, the PLUS method takes only one argument,
                       // but he thinks it's an instance method -> still two ref's on the stack
                       //  -> translated by backend
                       val rhs = treeCopy.Apply(tree, f, args)
                       return typed(Assign(qual, rhs))
                     }
                  case _ => ()
                }
              }

              /** This is translating uses of List() into Nil.  This is less
               *  than ideal from a consistency standpoint, but it shouldn't be
               *  altered without due caution.
               */
              if (fun.symbol == List_apply && args.isEmpty && !forInteractive)
                atPos(tree.pos)(gen.mkNil setType restpe)
              else
                constfold(treeCopy.Apply(tree, fun, args1) setType ifPatternSkipFormals(restpe))

            } else if (needsInstantiation(tparams, formals, args)) {
              //println("needs inst "+fun+" "+tparams+"/"+(tparams map (_.info)))
              inferExprInstance(fun, tparams)
              doTypedApply(tree, fun, args, mode, pt)
            } else {
              assert(!inPatternMode(mode)) // this case cannot arise for patterns
              val lenientTargs = protoTypeArgs(tparams, formals, mt.resultApprox, pt)
              val strictTargs = (lenientTargs, tparams).zipped map ((targ, tparam) =>
                if (targ == WildcardType) tparam.tpe else targ) //@M TODO: should probably be .tpeHK
              var remainingParams = paramTypes
              def typedArgToPoly(arg: Tree, formal: Type): Tree = { //TR TODO: cleanup
                val lenientPt = formal.instantiateTypeParams(tparams, lenientTargs)
                val newmode =
                  if (isByNameParamType(remainingParams.head)) POLYmode
                  else POLYmode | BYVALmode
                if (remainingParams.tail.nonEmpty) remainingParams = remainingParams.tail
                val arg1 = typedArg(arg, forArgMode(fun, mode), newmode, lenientPt)
                val argtparams = context.extractUndetparams()
                if (!argtparams.isEmpty) {
                  val strictPt = formal.instantiateTypeParams(tparams, strictTargs)
                  inferArgumentInstance(arg1, argtparams, strictPt, lenientPt)
                }
                arg1
              }
              val args1 = (args, formals).zipped map typedArgToPoly
              if (args1 exists (_.tpe.isError)) errTree
              else {
                debuglog("infer method inst "+fun+", tparams = "+tparams+", args = "+args1.map(_.tpe)+", pt = "+pt+", lobounds = "+tparams.map(_.tpe.bounds.lo)+", parambounds = "+tparams.map(_.info)) //debug
                // define the undetparams which have been fixed by this param list, replace the corresponding symbols in "fun"
                // returns those undetparams which have not been instantiated.
                val undetparams = inferMethodInstance(fun, tparams, args1, pt)
                val result = doTypedApply(tree, fun, args1, mode, pt)
                context.undetparams = undetparams
                result
              }
            }
          }

        case SingleType(_, _) =>
          doTypedApply(tree, fun setType fun.tpe.widen, args, mode, pt)

        case ErrorType =>
          setError(treeCopy.Apply(tree, fun, args))
        /* --- begin unapply  --- */

        case otpe if inPatternMode(mode) && unapplyMember(otpe).exists =>
          if (args.length > MaxTupleArity)
            error(fun.pos, "too many arguments for unapply pattern, maximum = "+MaxTupleArity)

          //
          def freshArgType(tp: Type): (List[Symbol], Type) = tp match {
            case MethodType(param :: _, _) =>
              (Nil, param.tpe)
            case PolyType(tparams, restpe) =>
              createFromClonedSymbols(tparams, freshArgType(restpe)._2)((ps, t) => ((ps, t)))
            case OverloadedType(_, _) =>
              error(fun.pos, "cannot resolve overloaded unapply")
              (Nil, ErrorType)
            case _ =>
              error(fun.pos, "an unapply method must accept a single argument.")
              (Nil, ErrorType)
          }

          val unapp     = unapplyMember(otpe)
          val unappType = otpe.memberType(unapp)
          val argDummy  = context.owner.newValue(fun.pos, nme.SELECTOR_DUMMY) setFlag SYNTHETIC setInfo pt
          val arg       = Ident(argDummy) setType pt

          if (!isApplicableSafe(Nil, unappType, List(pt), WildcardType)) {
            //Console.println("UNAPP: need to typetest, arg.tpe = "+arg.tpe+", unappType = "+unappType)
            val (freeVars, unappFormal) = freshArgType(unappType.skolemizeExistential(context.owner, tree))
            val unapplyContext = context.makeNewScope(context.tree, context.owner)
            freeVars foreach unapplyContext.scope.enter

            val typer1 = newTyper(unapplyContext)
            val pattp  = typer1.infer.inferTypedPattern(tree.pos, unappFormal, arg.tpe)

            // turn any unresolved type variables in freevars into existential skolems
            val skolems = freeVars map (fv => newExistentialSkolem(fv, unapplyContext.owner, fv))
            arg.tpe = pattp.substSym(freeVars, skolems)
            argDummy setInfo arg.tpe
          }

          // setType null is necessary so that ref will be stabilized; see bug 881
          val fun1 = typedPos(fun.pos)(Apply(Select(fun setType null, unapp), List(arg)))

          if (fun1.tpe.isErroneous) errTree
          else {
            val formals0 = unapplyTypeList(fun1.symbol, fun1.tpe)
            val formals1 = formalTypes(formals0, args.length)
            if (sameLength(formals1, args)) {
              val args1 = typedArgs(args, mode, formals0, formals1)
              // This used to be the following (failing) assert:
              //   assert(isFullyDefined(pt), tree+" ==> "+UnApply(fun1, args1)+", pt = "+pt)
              // I modified as follows.  See SI-1048.
              val pt1 = if (isFullyDefined(pt)) pt else makeFullyDefined(pt)

              val itype = glb(List(pt1, arg.tpe))
              arg.tpe = pt1    // restore type (arg is a dummy tree, just needs to pass typechecking)
              UnApply(fun1, args1) setPos tree.pos setType itype
            }
            else {
              errorTree("wrong number of arguments for "+treeSymTypeMsg(fun))
            }
          }

/* --- end unapply  --- */
        case _ =>
          errorTree(fun.tpe+" does not take parameters")
      }
    }

    /**
     * Convert an annotation constructor call into an AnnotationInfo.
     *
     * @param annClass the expected annotation class
     */
    def typedAnnotation(ann: Tree, mode: Int = EXPRmode, selfsym: Symbol = NoSymbol, annClass: Symbol = AnnotationClass, requireJava: Boolean = false): AnnotationInfo = {
      lazy val annotationError = AnnotationInfo(ErrorType, Nil, Nil)
      var hasError: Boolean = false
      def error(pos: Position, msg: String) = {
        context.error(pos, msg)
        hasError = true
        annotationError
      }

      /** Calling constfold right here is necessary because some trees (negated
       *  floats and literals in particular) are not yet folded.
       */
      def tryConst(tr: Tree, pt: Type): Option[LiteralAnnotArg] = {
        val const: Constant = typed(constfold(tr), EXPRmode, pt) match {
          case l @ Literal(c) if !l.isErroneous => c
          case tree => tree.tpe match {
            case ConstantType(c)  => c
            case tpe              => null
          }
        }
        def fail(msg: String) = { error(tr.pos, msg) ; None }

        if (const == null)
          fail("annotation argument needs to be a constant; found: " + tr)
        else if (const.value == null)
          fail("annotation argument cannot be null")
        else
          Some(LiteralAnnotArg(const))
      }

      /** Converts an untyped tree to a ClassfileAnnotArg. If the conversion fails,
       *  an error message is reported and None is returned.
       */
      def tree2ConstArg(tree: Tree, pt: Type): Option[ClassfileAnnotArg] = tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) if (pt.typeSymbol == ArrayClass) =>
          error(tree.pos, "Array constants have to be specified using the `Array(...)' factory method")
          None

        case ann @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val annInfo = typedAnnotation(ann, mode, NoSymbol, pt.typeSymbol, true)
          if (annInfo.atp.isErroneous) {
            // recursive typedAnnotation call already printed an error, so don't call "error"
            hasError = true
            None
          } else Some(NestedAnnotArg(annInfo))

        // use of Array.apply[T: ClassManifest](xs: T*): Array[T]
        // and    Array.apply(x: Int, xs: Int*): Array[Int]       (and similar)
        case Apply(fun, args) =>
          val typedFun = typed(fun, forFunMode(mode), WildcardType)
          if (typedFun.symbol.owner == ArrayModule.moduleClass && typedFun.symbol.name == nme.apply)
            pt match {
              case TypeRef(_, ArrayClass, targ :: _) =>
                trees2ConstArg(args, targ)
              case _ =>
                // For classfile annotations, pt can only be T:
                //   BT = Int, .., String, Class[_], JavaAnnotClass
                //   T = BT | Array[BT]
                // So an array literal as argument can only be valid if pt is Array[_]
                error(tree.pos, "found array constant, expected argument of type "+ pt)
                None
            }
          else
            tryConst(tree, pt)

        case Typed(t, _) => tree2ConstArg(t, pt)

        case tree =>
          tryConst(tree, pt)
      }
      def trees2ConstArg(trees: List[Tree], pt: Type): Option[ArrayAnnotArg] = {
        val args = trees.map(tree2ConstArg(_, pt))
        if (args.exists(_.isEmpty)) None
        else Some(ArrayAnnotArg(args.flatten.toArray))
      }

      // begin typedAnnotation
      val (fun, argss) = {
        def extract(fun: Tree, outerArgss: List[List[Tree]]):
          (Tree, List[List[Tree]]) = fun match {
            case Apply(f, args) =>
              extract(f, args :: outerArgss)
            case Select(New(tpt), nme.CONSTRUCTOR) =>
              (fun, outerArgss)
            case _ =>
              error(fun.pos, "unexpected tree in annotation: "+ fun)
              (setError(fun), outerArgss)
          }
        extract(ann, List())
      }

      if (fun.isErroneous) annotationError
      else {
        val typedFun @ Select(New(tpt), _) = typed(fun, forFunMode(mode), WildcardType)
        val annType = tpt.tpe

        if (typedFun.isErroneous) annotationError
        else if (annType.typeSymbol isNonBottomSubClass ClassfileAnnotationClass) {
          // annotation to be saved as java classfile annotation
          val isJava = typedFun.symbol.owner.isJavaDefined
          if (!annType.typeSymbol.isNonBottomSubClass(annClass)) {
            error(tpt.pos, "expected annotation of type "+ annClass.tpe +", found "+ annType)
          } else if (argss.length > 1) {
            error(ann.pos, "multiple argument lists on classfile annotation")
          } else {
            val args =
              if (argss.head.length == 1 && !isNamed(argss.head.head))
                List(new AssignOrNamedArg(Ident(nme.value), argss.head.head))
              else argss.head
            val annScope = annType.decls
                .filter(sym => sym.isMethod && !sym.isConstructor && sym.isJavaDefined)
            val names = new collection.mutable.HashSet[Symbol]
            names ++= (if (isJava) annScope.iterator
                       else typedFun.tpe.params.iterator)
            val nvPairs = args map {
              case arg @ AssignOrNamedArg(Ident(name), rhs) =>
                val sym = if (isJava) annScope.lookup(name)
                          else typedFun.tpe.params.find(p => p.name == name).getOrElse(NoSymbol)
                if (sym == NoSymbol) {
                  error(arg.pos, "unknown annotation argument name: " + name)
                  (nme.ERROR, None)
                } else if (!names.contains(sym)) {
                  error(arg.pos, "duplicate value for annotation argument " + name)
                  (nme.ERROR, None)
                } else {
                  names -= sym
                  if (isJava) sym.cookJavaRawInfo() // #3429
                  val annArg = tree2ConstArg(rhs, sym.tpe.resultType)
                  (sym.name, annArg)
                }
              case arg =>
                error(arg.pos, "classfile annotation arguments have to be supplied as named arguments")
                (nme.ERROR, None)
            }
            for (sym <- names) {
              // make sure the flags are up to date before erroring (jvm/t3415 fails otherwise)
              sym.initialize
              if (!sym.hasAnnotation(AnnotationDefaultAttr) && !sym.hasDefaultFlag)
                error(ann.pos, "annotation " + annType.typeSymbol.fullName + " is missing argument " + sym.name)
            }

            if (hasError) annotationError
            else AnnotationInfo(annType, List(), nvPairs map {p => (p._1, p._2.get)}).setPos(ann.pos)
          }
        } else if (requireJava) {
          error(ann.pos, "nested classfile annotations must be defined in java; found: "+ annType)
        } else {
          val typedAnn = if (selfsym == NoSymbol) {
            typed(ann, mode, annClass.tpe)
          } else {
            // Since a selfsym is supplied, the annotation should have
            // an extra "self" identifier in scope for type checking.
            // This is implemented by wrapping the rhs
            // in a function like "self => rhs" during type checking,
            // and then stripping the "self =>" and substituting
            // in the supplied selfsym.
            val funcparm = ValDef(NoMods, nme.self, TypeTree(selfsym.info), EmptyTree)
            val func = Function(List(funcparm), ann.duplicate)
                                         // The .duplicate of annot.constr
                                         // deals with problems that
                                         // accur if this annotation is
                                         // later typed again, which
                                         // the compiler sometimes does.
                                         // The problem is that "self"
                                         // ident's within annot.constr
                                         // will retain the old symbol
                                         // from the previous typing.
            val fun1clazz = FunctionClass(1)
            val funcType = typeRef(fun1clazz.tpe.prefix,
                                   fun1clazz,
                                   List(selfsym.info, annClass.tpe))

            (typed(func, mode, funcType): @unchecked) match {
              case t @ Function(List(arg), rhs) =>
                val subs =
                  new TreeSymSubstituter(List(arg.symbol),List(selfsym))
                subs(rhs)
            }
          }

          def annInfo(t: Tree): AnnotationInfo = t match {
            case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
              AnnotationInfo(annType, args, List()).setPos(t.pos)

            case Block(stats, expr) =>
              context.warning(t.pos, "Usage of named or default arguments transformed this annotation\n"+
                                "constructor call into a block. The corresponding AnnotationInfo\n"+
                                "will contain references to local values and default getters instead\n"+
                                "of the actual argument trees")
              annInfo(expr)

            case Apply(fun, args) =>
              context.warning(t.pos, "Implementation limitation: multiple argument lists on annotations are\n"+
                                     "currently not supported; ignoring arguments "+ args)
              annInfo(fun)

            case _ =>
              error(t.pos, "unexpected tree after typing annotation: "+ typedAnn)
          }

          if (annType.typeSymbol == DeprecatedAttr && argss.flatten.size < 2)
            unit.deprecationWarning(ann.pos, "@deprecated now takes two arguments; see the scaladoc.")

          if ((typedAnn.tpe == null) || typedAnn.tpe.isErroneous) annotationError
          else annInfo(typedAnn)
        }
      }
    }

    def isRawParameter(sym: Symbol) = // is it a type parameter leaked by a raw type?
      sym.isTypeParameter && sym.owner.isJavaDefined

    /** Given a set `rawSyms` of term- and type-symbols, and a type
     *  `tp`, produce a set of fresh type parameters and a type so that
     *  it can be abstracted to an existential type. Every type symbol
     *  `T` in `rawSyms` is mapped to a clone. Every term symbol `x` of
     *  type `T` in `rawSyms` is given an associated type symbol of the
     *  following form:
     *
     *    type x.type <: T with Singleton
     *
     *  The name of the type parameter is `x.type`, to produce nice
     *  diagnostics. The Singleton parent ensures that the type
     *  parameter is still seen as a stable type. Type symbols in
     *  rawSyms are fully replaced by the new symbols. Term symbols are
     *  also replaced, except for term symbols of an Ident tree, where
     *  only the type of the Ident is changed.
     */
    protected def existentialTransform[T](rawSyms: List[Symbol], tp: Type)(creator: (List[Symbol], Type) => T): T = {
      val typeParams: List[Symbol] = rawSyms map { sym =>
        val name = sym.name match {
          case x: TypeName  => x
          case x            => newTypeName(x + ".type")
        }
        val bound      = sym.existentialBound
        val sowner     = if (isRawParameter(sym)) context.owner else sym.owner
        val quantified = sowner.newExistential(sym.pos, name)

        quantified setInfo bound.cloneInfo(quantified)
      }
      // Higher-kinded existentials are not yet supported, but this is
      // tpeHK for when they are: "if a type constructor is expected/allowed,
      // tpeHK must be called instead of tpe."
      val typeParamTypes = typeParams map (_.tpeHK)
      def doSubst(info: Type) = info.subst(rawSyms, typeParamTypes)

      creator(typeParams map (_ modifyInfo doSubst), doSubst(tp))
    }

    /** Compute an existential type from raw hidden symbols `syms` and type `tp`
     */
    def packSymbols(hidden: List[Symbol], tp: Type): Type =
      if (hidden.isEmpty) tp
      else existentialTransform(hidden, tp)(existentialAbstraction)

    def isCapturedExistential(sym: Symbol) =
      sym hasAllFlags (EXISTENTIAL | CAPTURED)  // todo refine this

    def packCaptured(tpe: Type): Type = {
      val captured = mutable.Set[Symbol]()
      for (tp <- tpe)
        if (isCapturedExistential(tp.typeSymbol))
          captured += tp.typeSymbol
      existentialAbstraction(captured.toList, tpe)
    }

    /** convert skolems to existentials */
    def packedType(tree: Tree, owner: Symbol): Type = {
      def defines(tree: Tree, sym: Symbol) =
        sym.isExistentialSkolem && sym.unpackLocation == tree ||
        tree.isDef && tree.symbol == sym
      def isVisibleParameter(sym: Symbol) =
        sym.isParameter && (sym.owner == owner) && (sym.isType || !owner.isAnonymousFunction)
      def containsDef(owner: Symbol, sym: Symbol): Boolean =
        (!sym.hasPackageFlag) && {
          var o = sym.owner
          while (o != owner && o != NoSymbol && !o.hasPackageFlag) o = o.owner
          o == owner && !isVisibleParameter(sym)
        }
      var localSyms = collection.immutable.Set[Symbol]()
      var boundSyms = collection.immutable.Set[Symbol]()
      def isLocal(sym: Symbol): Boolean =
        if (sym == NoSymbol || sym.isRefinementClass || sym.isLocalDummy) false
        else if (owner == NoSymbol) tree exists (defines(_, sym))
        else containsDef(owner, sym) || isRawParameter(sym) || isCapturedExistential(sym)
      def containsLocal(tp: Type): Boolean =
        tp exists (t => isLocal(t.typeSymbol) || isLocal(t.termSymbol))
      val normalizeLocals = new TypeMap {
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) =>
            if (sym.isAliasType && containsLocal(tp)) apply(tp.normalize)
            else {
              if (pre.isVolatile)
                context.error(tree.pos, "Inferred type "+tree.tpe+" contains type selection from volatile type "+pre)
              mapOver(tp)
            }
          case _ =>
            mapOver(tp)
        }
      }
      // add all local symbols of `tp` to `localSyms`
      // TODO: expand higher-kinded types into individual copies for each instance.
      def addLocals(tp: Type) {
        val remainingSyms = new ListBuffer[Symbol]
        def addIfLocal(sym: Symbol, tp: Type) {
          if (isLocal(sym) && !localSyms(sym) && !boundSyms(sym)) {
            if (sym.typeParams.isEmpty) {
              localSyms += sym
              remainingSyms += sym
            } else {
              unit.error(tree.pos,
                "can't existentially abstract over parameterized type " + tp)
            }
          }
        }

        for (t <- tp) {
          t match {
            case ExistentialType(tparams, _) =>
              boundSyms ++= tparams
            case AnnotatedType(annots, _, _) =>
              for (annot <- annots; arg <- annot.args) {
                arg match {
                  case Ident(_) =>
                    // Check the symbol of an Ident, unless the
                    // Ident's type is already over an existential.
                    // (If the type is already over an existential,
                    // then remap the type, not the core symbol.)
                    if (!arg.tpe.typeSymbol.hasFlag(EXISTENTIAL))
                      addIfLocal(arg.symbol, arg.tpe)
                  case _ => ()
                }
              }
            case _ =>
          }
          addIfLocal(t.termSymbol, t)
          addIfLocal(t.typeSymbol, t)
        }
        for (sym <- remainingSyms) addLocals(sym.existentialBound)
      }

      val normalizedTpe = normalizeLocals(tree.tpe)
      addLocals(normalizedTpe)
      packSymbols(localSyms.toList, normalizedTpe)
    }

    /** Replace type parameters with their TypeSkolems, which can later
     *  be deskolemized to the original type param. (A skolem is a
     *  representation of a bound variable when viewed inside its scope)
     *  !!!Adriaan: this does not work for hk types.
     */
    def skolemizeTypeParams(tparams: List[TypeDef]): List[TypeDef] = {
      class Deskolemizer extends LazyType {
        override val typeParams = tparams map (_.symbol)
        val typeSkolems  = typeParams map (_.newTypeSkolem setInfo this)
        // Replace the symbols
        def substitute() = (tparams, typeSkolems).zipped map (_ setSymbol _)
        override def complete(sym: Symbol) {
          // The info of a skolem is the skolemized info of the
          // actual type parameter of the skolem
          sym setInfo sym.deSkolemize.info.substSym(typeParams, typeSkolems)
        }
      }
      (new Deskolemizer).substitute()
    }
    /** Convert to corresponding type parameters all skolems of method
     *  parameters which appear in `tparams`.
     */
    def deskolemizeTypeParams(tparams: List[Symbol])(tp: Type): Type = {
      class DeSkolemizeMap extends TypeMap {
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) if sym.isTypeSkolem && (tparams contains sym.deSkolemize) =>
            mapOver(typeRef(NoPrefix, sym.deSkolemize, args))
          case _ =>
            mapOver(tp)
        }
      }
      new DeSkolemizeMap mapOver tp
    }

    protected def typedExistentialTypeTree(tree: ExistentialTypeTree, mode: Int): Tree = {
      for (wc <- tree.whereClauses)
        if (wc.symbol == NoSymbol) { namer.enterSym(wc); wc.symbol setFlag EXISTENTIAL }
        else context.scope enter wc.symbol
      val whereClauses1 = typedStats(tree.whereClauses, context.owner)
      for (vd @ ValDef(_, _, _, _) <- tree.whereClauses)
        if (vd.symbol.tpe.isVolatile)
          error(vd.pos, "illegal abstraction from value with volatile type "+vd.symbol.tpe)
      val tpt1 = typedType(tree.tpt, mode)
      existentialTransform(tree.whereClauses map (_.symbol), tpt1.tpe)((tparams, tp) =>
        TypeTree(ExistentialType(tparams, tp)) setOriginal tree
      )
    }

    // lifted out of typed1 because it's needed in typedImplicit0
    protected def typedTypeApply(tree: Tree, mode: Int, fun: Tree, args: List[Tree]): Tree = fun.tpe match {
      case OverloadedType(pre, alts) =>
        inferPolyAlternatives(fun, args map (_.tpe))
        val tparams = fun.symbol.typeParams //@M TODO: fun.symbol.info.typeParams ? (as in typedAppliedTypeTree)
        val args1 = if (sameLength(args, tparams)) {
          //@M: in case TypeApply we can't check the kind-arities of the type arguments,
          // as we don't know which alternative to choose... here we do
          map2Conserve(args, tparams) {
            //@M! the polytype denotes the expected kind
            (arg, tparam) => typedHigherKindedType(arg, mode, polyType(tparam.typeParams, AnyClass.tpe))
          }
        } else // @M: there's probably something wrong when args.length != tparams.length... (triggered by bug #320)
         // Martin, I'm using fake trees, because, if you use args or arg.map(typedType),
         // inferPolyAlternatives loops...  -- I have no idea why :-(
         // ...actually this was looping anyway, see bug #278.
          return errorTree(fun, "wrong number of type parameters for "+treeSymTypeMsg(fun))

        typedTypeApply(tree, mode, fun, args1)
      case SingleType(_, _) =>
        typedTypeApply(tree, mode, fun setType fun.tpe.widen, args)
      case PolyType(tparams, restpe) if tparams.nonEmpty =>
        if (sameLength(tparams, args)) {
          val targs = args map (_.tpe)
          checkBounds(tree.pos, NoPrefix, NoSymbol, tparams, targs, "")
          if (fun.symbol == Predef_classOf) {
            checkClassType(args.head, true, false)
            atPos(tree.pos) { gen.mkClassOf(targs.head) }
          } else {
            if (!isPastTyper && fun.symbol == Any_isInstanceOf && !targs.isEmpty)
              checkCheckable(tree.pos, targs.head, "")
            val resultpe = restpe.instantiateTypeParams(tparams, targs)
            //@M substitution in instantiateParams needs to be careful!
            //@M example: class Foo[a] { def foo[m[x]]: m[a] = error("") } (new Foo[Int]).foo[List] : List[Int]
            //@M    --> first, m[a] gets changed to m[Int], then m gets substituted for List,
            //          this must preserve m's type argument, so that we end up with List[Int], and not List[a]
            //@M related bug: #1438
            //println("instantiating type params "+restpe+" "+tparams+" "+targs+" = "+resultpe)
            treeCopy.TypeApply(tree, fun, args) setType resultpe
          }
        } else {
          errorTree(tree, "wrong number of type parameters for "+treeSymTypeMsg(fun))
        }
      case ErrorType =>
        setError(treeCopy.TypeApply(tree, fun, args))
      case _ =>
        errorTree(tree, treeSymTypeMsg(fun)+" does not take type parameters.")
    }

    @inline final def deindentTyping() = context.typingIndentLevel -= 2
    @inline final def indentTyping() = context.typingIndentLevel += 2
    @inline final def printTyping(s: => String) = {
      if (printTypings)
        println(context.typingIndent + s.replaceAll("\n", "\n" + context.typingIndent))
    }
    @inline final def printInference(s: => String) = {
      if (printInfers)
        println(s)
    }

    protected def typed1(tree: Tree, mode: Int, pt: Type): Tree = {
      def isPatternMode = inPatternMode(mode)

      //Console.println("typed1("+tree.getClass()+","+Integer.toHexString(mode)+","+pt+")")
      def ptOrLub(tps: List[Type]) = if (isFullyDefined(pt)) (pt, false) else weakLub(tps map (_.deconst))

      //@M! get the type of the qualifier in a Select tree, otherwise: NoType
      def prefixType(fun: Tree): Type = fun match {
        case Select(qualifier, _) => qualifier.tpe
//        case Ident(name) => ??
        case _ => NoType
      }

      def typedAnnotated(ann: Tree, arg1: Tree): Tree = {
        /** mode for typing the annotation itself */
        val annotMode = mode & ~TYPEmode | EXPRmode

        if (arg1.isType) {
          // make sure the annotation is only typechecked once
          if (ann.tpe == null) {
            // an annotated type
            val selfsym =
              if (!settings.selfInAnnots.value)
                NoSymbol
              else
                arg1.tpe.selfsym orElse {
                  /* Implementation limitation: Currently this
                   * can cause cyclical reference errors even
                   * when the self symbol is not referenced at all.
                   * Surely at least some of these cases can be
                   * fixed by proper use of LazyType's.  Lex tinkered
                   * on this but did not succeed, so is leaving
                   * it alone for now. Example code with the problem:
                   *  class peer extends Annotation
                   *  class NPE[T <: NPE[T] @peer]
                   *
                   * (Note: -Yself-in-annots must be on to see the problem)
                   * */
                  ( context.owner
                      newLocalDummy (ann.pos)
                      newValue (ann.pos, nme.self)
                      setInfo (arg1.tpe.withoutAnnotations)
                  )
                }

            val ainfo = typedAnnotation(ann, annotMode, selfsym)
            val atype0 = arg1.tpe.withAnnotation(ainfo)
            val atype =
              if ((selfsym != NoSymbol) && (ainfo.refsSymbol(selfsym)))
                atype0.withSelfsym(selfsym)
              else
                atype0 // do not record selfsym if
                       // this annotation did not need it

            if (ainfo.isErroneous)
              arg1  // simply drop erroneous annotations
            else {
              ann.tpe = atype
              TypeTree(atype) setOriginal tree
            }
          } else {
            // the annotation was typechecked before
            TypeTree(ann.tpe) setOriginal tree
          }
        } else {
          if (ann.tpe == null) {
            val annotInfo = typedAnnotation(ann, annotMode)
            ann.tpe = arg1.tpe.withAnnotation(annotInfo)
          }
          val atype = ann.tpe
          Typed(arg1, TypeTree(atype) setOriginal tree setPos tree.pos.focus) setPos tree.pos setType atype
        }
      }

      def typedBind(name: Name, body: Tree) = {
        var vble = tree.symbol
        def typedBindType(name: TypeName) = {
          assert(body == EmptyTree, context.unit + " typedBind: " + name.debugString + " " + body + " " + body.getClass)
          if (vble == NoSymbol)
            vble =
              if (isFullyDefined(pt))
                context.owner.newAliasType(tree.pos, name) setInfo pt
              else
                context.owner.newAbstractType(tree.pos, name) setInfo TypeBounds.empty
          val rawInfo = vble.rawInfo
          vble = if (vble.name == tpnme.WILDCARD) context.scope.enter(vble)
                 else namer.enterInScope(vble)
          tree setSymbol vble setType vble.tpe
        }
        def typedBindTerm(name: TermName) = {
          if (vble == NoSymbol)
            vble = context.owner.newValue(tree.pos, name)
          if (vble.name.toTermName != nme.WILDCARD) {
            if ((mode & ALTmode) != 0)
              error(tree.pos, "illegal variable in pattern alternative")
            vble = namer.enterInScope(vble)
          }
          val body1 = typed(body, mode, pt)
          vble.setInfo(
            if (treeInfo.isSequenceValued(body)) seqType(body1.tpe)
            else body1.tpe)
          treeCopy.Bind(tree, name, body1) setSymbol vble setType body1.tpe   // burak, was: pt
        }
        name match {
          case x: TypeName  => typedBindType(x)
          case x: TermName  => typedBindTerm(x)
        }
      }

      def typedArrayValue(elemtpt: Tree, elems: List[Tree]) = {
        val elemtpt1 = typedType(elemtpt, mode)
        val elems1 = elems mapConserve (elem => typed(elem, mode, elemtpt1.tpe))
        treeCopy.ArrayValue(tree, elemtpt1, elems1)
          .setType(
            (if (isFullyDefined(pt) && !phase.erasedTypes) pt
             else appliedType(ArrayClass.typeConstructor, List(elemtpt1.tpe))).notNull)
      }

      def typedAssign(lhs: Tree, rhs: Tree): Tree = {
        val lhs1    = typed(lhs, EXPRmode | LHSmode, WildcardType)
        val varsym  = lhs1.symbol
        def failMsg =
          if (varsym != null && varsym.isValue) "reassignment to val"
          else "assignment to non variable"

        def fail = {
          if (!lhs1.tpe.isError)
            error(tree.pos, failMsg)

          setError(tree)
        }
        if (varsym == null)
          return fail

        if (treeInfo.mayBeVarGetter(varsym)) {
          treeInfo.methPart(lhs1) match {
            case Select(qual, name) =>
              val sel = Select(qual, nme.getterToSetter(name.toTermName)) setPos lhs.pos
              val app = Apply(sel, List(rhs)) setPos tree.pos
              return typed(app, mode, pt)

            case _ =>
          }
        }
        if (varsym.isVariable || varsym.isValue && phase.erasedTypes) {
          val rhs1 = typed(rhs, EXPRmode | BYVALmode, lhs1.tpe)
          treeCopy.Assign(tree, lhs1, checkDead(rhs1)) setType UnitClass.tpe
        }
        else fail
      }

      def typedIf(cond: Tree, thenp: Tree, elsep: Tree) = {
        val cond1 = checkDead(typed(cond, EXPRmode | BYVALmode, BooleanClass.tpe))
        if (elsep.isEmpty) { // in the future, should be unnecessary
          val thenp1 = typed(thenp, UnitClass.tpe)
          treeCopy.If(tree, cond1, thenp1, elsep) setType thenp1.tpe
        } else {
          var thenp1 = typed(thenp, pt)
          var elsep1 = typed(elsep, pt)
          val (owntype, needAdapt) = ptOrLub(List(thenp1.tpe, elsep1.tpe))
          if (needAdapt) { //isNumericValueType(owntype)) {
            thenp1 = adapt(thenp1, mode, owntype)
            elsep1 = adapt(elsep1, mode, owntype)
          }
          treeCopy.If(tree, cond1, thenp1, elsep1) setType owntype
        }
      }

      def typedMatch(tree: Tree, selector: Tree, cases: List[CaseDef]): Tree = {
        if (selector == EmptyTree) {
          val arity = if (isFunctionType(pt)) pt.normalize.typeArgs.length - 1 else 1
          val params = for (i <- List.range(0, arity)) yield
            atPos(tree.pos.focusStart) {
              ValDef(Modifiers(PARAM | SYNTHETIC),
                     unit.freshTermName("x" + i + "$"), TypeTree(), EmptyTree)
            }
          val ids = for (p <- params) yield Ident(p.name)
          val selector1 = atPos(tree.pos.focusStart) { if (arity == 1) ids.head else gen.mkTuple(ids) }
          val body = treeCopy.Match(tree, selector1, cases)
          typed1(atPos(tree.pos) { Function(params, body) }, mode, pt)
        } else {
          val selector1 = checkDead(typed(selector, EXPRmode | BYVALmode, WildcardType))
          var cases1 = typedCases(tree, cases, packCaptured(selector1.tpe.widen), pt)

          if (isPastTyper || !opt.virtPatmat) {
            val (owntype, needAdapt) = ptOrLub(cases1 map (_.tpe))
            if (needAdapt) {
              cases1 = cases1 map (adaptCase(_, owntype))
            }
            treeCopy.Match(tree, selector1, cases1) setType owntype
          } else { // don't run translator after typers (see comments in PatMatVirtualiser)
            val (owntype0, needAdapt) = ptOrLub(cases1 map (x => repackExistential(x.tpe)))
            val owntype = elimAnonymousClass(owntype0)
            if (needAdapt) cases1 = cases1 map (adaptCase(_, owntype))

            val translated = (new MatchTranslator(this)).translateMatch(selector1, cases1, owntype)

            typed1(translated, mode, WildcardType) setType owntype // TODO: get rid of setType owntype -- it should all typecheck
          }
        }
      }

      def typedReturn(expr: Tree) = {
        val enclMethod = context.enclMethod
        if (enclMethod == NoContext ||
            enclMethod.owner.isConstructor ||
            context.enclClass.enclMethod == enclMethod // i.e., we are in a constructor of a local class
            ) {
          errorTree(tree, "return outside method definition")
        } else {
          val DefDef(_, name, _, _, restpt, _) = enclMethod.tree
          if (restpt.tpe eq null)
            errorTree(tree, enclMethod.owner + " has return statement; needs result type")
          else {
            context.enclMethod.returnsSeen = true
            val expr1: Tree = typed(expr, EXPRmode | BYVALmode, restpt.tpe)
            // Warn about returning a value if no value can be returned.
            if (restpt.tpe.typeSymbol == UnitClass) {
              // The typing in expr1 says expr is Unit (it has already been coerced if
              // it is non-Unit) so we have to retype it.  Fortunately it won't come up much
              // unless the warning is legitimate.
              if (typed(expr).tpe.typeSymbol != UnitClass)
                unit.warning(tree.pos, "enclosing method " + name + " has result type Unit: return value discarded")
            }
            treeCopy.Return(tree, checkDead(expr1)) setSymbol enclMethod.owner setType NothingClass.tpe
          }
        }
      }

      def typedNew(tpt: Tree) = {
        val tpt1 = {
          val tpt0 = typedTypeConstructor(tpt)
          checkClassType(tpt0, false, true)
          if (tpt0.hasSymbol && !tpt0.symbol.typeParams.isEmpty) {
            context.undetparams = cloneSymbols(tpt0.symbol.typeParams)
            TypeTree().setOriginal(tpt0)
                      .setType(appliedType(tpt0.tpe, context.undetparams map (_.tpeHK))) // @PP: tpeHK! #3343, #4018, #4347.
          } else tpt0
        }

        /** If current tree <tree> appears in <val x(: T)? = <tree>>
         *  return `tp with x.type' else return `tp`.
         */
        def narrowRhs(tp: Type) = { val sym = context.tree.symbol
          context.tree match {
            case ValDef(mods, _, _, Apply(Select(`tree`, _), _)) if !mods.isMutable && sym != null && sym != NoSymbol =>
              val sym1 = if (sym.owner.isClass && sym.getter(sym.owner) != NoSymbol) sym.getter(sym.owner)
                else sym.lazyAccessorOrSelf
              val pre = if (sym1.owner.isClass) sym1.owner.thisType else NoPrefix
              intersectionType(List(tp, singleType(pre, sym1)))
            case _ => tp
          }}

        val tp = tpt1.tpe
        val sym = tp.typeSymbol.initialize
        if (sym.isAbstractType || sym.hasAbstractFlag)
          error(tree.pos, sym + " is abstract; cannot be instantiated")
        else if (!(  tp == sym.thisSym.tpe // when there's no explicit self type -- with (#3612) or without self variable
                     // sym.thisSym.tpe == tp.typeOfThis (except for objects)
                  || narrowRhs(tp) <:< tp.typeOfThis
                  || phase.erasedTypes
                  )) {
          error(tree.pos, sym +
                " cannot be instantiated because it does not conform to its self-type "+
                tp.typeOfThis)
        }
        treeCopy.New(tree, tpt1).setType(tp)
      }

      def typedEta(expr1: Tree): Tree = expr1.tpe match {
        case TypeRef(_, ByNameParamClass, _) =>
          val expr2 = Function(List(), expr1) setPos expr1.pos
          new ChangeOwnerTraverser(context.owner, expr2.symbol).traverse(expr2)
          typed1(expr2, mode, pt)
        case NullaryMethodType(restpe) =>
          val expr2 = Function(List(), expr1) setPos expr1.pos
          new ChangeOwnerTraverser(context.owner, expr2.symbol).traverse(expr2)
          typed1(expr2, mode, pt)
        case PolyType(_, MethodType(formals, _)) =>
          if (isFunctionType(pt)) expr1
          else adapt(expr1, mode, functionType(formals map (t => WildcardType), WildcardType))
        case MethodType(formals, _) =>
          if (isFunctionType(pt)) expr1
          else expr1 match {
            case Select(qual, name) if (forMSIL &&
                                        pt != WildcardType &&
                                        pt != ErrorType &&
                                        isSubType(pt, DelegateClass.tpe)) =>
              val scalaCaller = newScalaCaller(pt)
              addScalaCallerInfo(scalaCaller, expr1.symbol)
              val n: Name = scalaCaller.name
              val del = Ident(DelegateClass) setType DelegateClass.tpe
              val f = Select(del, n)
              //val f1 = TypeApply(f, List(Ident(pt.symbol) setType pt))
              val args: List[Tree] = if(expr1.symbol.isStatic) List(Literal(Constant(null)))
                                     else List(qual) // where the scala-method is located
              val rhs = Apply(f, args)
              typed(rhs)
            case _ =>
              adapt(expr1, mode, functionType(formals map (t => WildcardType), WildcardType))
          }
        case ErrorType =>
          expr1
        case _ =>
          errorTree(expr1, "_ must follow method; cannot follow " + expr1.tpe)
      }

      /**
       *  @param args ...
       *  @return     ...
       */
      def tryTypedArgs(args: List[Tree], mode: Int, other: TypeError): List[Tree] = {
        val c = context.makeSilent(false)
        c.retyping = true
        try {
          newTyper(c).typedArgs(args, mode)
        } catch {
          case ex: CyclicReference => throw ex
          case ex: TypeError =>
            null
        }
      }

      /** Try to apply function to arguments; if it does not work, try to convert Java raw to existentials, or try to
       *  insert an implicit conversion.
       */
      def tryTypedApply(fun: Tree, args: List[Tree]): Tree = {
        val start = startTimer(failedApplyNanos)
        silent(_.doTypedApply(tree, fun, args, mode, pt)) match {
          case t: Tree =>
            t
          case ex: TypeError =>
            stopTimer(failedApplyNanos, start)

            // If the problem is with raw types, copnvert to existentials and try again.
            // See #4712 for a case where this situation arises,
            if ((fun.symbol ne null) && fun.symbol.isJavaDefined) {
              val newtpe = rawToExistential(fun.tpe)
              if (fun.tpe ne newtpe) {
                // println("late cooking: "+fun+":"+fun.tpe) // DEBUG
                return tryTypedApply(fun setType newtpe, args)
              }
            }

            def treesInResult(tree: Tree): List[Tree] = tree :: (tree match {
              case Block(_, r)                        => treesInResult(r)
              case Match(_, cases)                    => cases
              case CaseDef(_, _, r)                   => treesInResult(r)
              case Annotated(_, r)                    => treesInResult(r)
              case If(_, t, e)                        => treesInResult(t) ++ treesInResult(e)
              case Try(b, catches, _)                 => treesInResult(b) ++ catches
              case Typed(r, Function(Nil, EmptyTree)) => treesInResult(r)
              case _                                  => Nil
            })
            def errorInResult(tree: Tree) = treesInResult(tree) exists (_.pos == ex.pos)
            val retry = fun :: tree :: args exists errorInResult
            printTyping {
              val funStr = ptTree(fun) + " and " + (args map ptTree mkString ", ")
              if (retry) "second try: " + funStr
              else "no second try: " + funStr + " because error not in result: " + ex.pos+"!="+tree.pos
            }
            if (retry) {
              val Select(qual, name) = fun
              val args1 = tryTypedArgs(args, forArgMode(fun, mode), ex)
              val qual1 =
                if ((args1 ne null) && !pt.isError) adaptToArguments(qual, name, args1, pt)
                else qual
              if (qual1 ne qual) {
                val tree1 = Apply(Select(qual1, name) setPos fun.pos, args1) setPos tree.pos
                return typed1(tree1, mode | SNDTRYmode, pt)
              }
            }
            reportTypeError(tree.pos, ex)
            setError(treeCopy.Apply(tree, fun, args))
        }
      }

      def typedApply(fun: Tree, args: List[Tree]) = {
        val stableApplication = (fun.symbol ne null) && fun.symbol.isMethod && fun.symbol.isStable
        if (stableApplication && isPatternMode) {
          // treat stable function applications f() as expressions.
          typed1(tree, mode & ~PATTERNmode | EXPRmode, pt)
        } else {
          val funpt = if (isPatternMode) pt else WildcardType
          val appStart = startTimer(failedApplyNanos)
          val opeqStart = startTimer(failedOpEqNanos)
          silent(_.typed(fun, forFunMode(mode), funpt),
                 if ((mode & EXPRmode) != 0) false else context.reportAmbiguousErrors,
                 if ((mode & EXPRmode) != 0) tree else context.tree) match {
            case fun1: Tree =>
              val fun2 = if (stableApplication) stabilizeFun(fun1, mode, pt) else fun1
              incCounter(typedApplyCount)
              def isImplicitMethod(tpe: Type) = tpe match {
                case mt: MethodType => mt.isImplicit
                case _ => false
              }
              val useTry = (
                   !isPastTyper
                && fun2.isInstanceOf[Select]
                && !isImplicitMethod(fun2.tpe)
                && ((fun2.symbol eq null) || !fun2.symbol.isConstructor)
                && (mode & (EXPRmode | SNDTRYmode)) == EXPRmode
              )
              val res =
                if (useTry) tryTypedApply(fun2, args)
                else doTypedApply(tree, fun2, args, mode, pt)

            /*
              if (fun2.hasSymbol && fun2.symbol.isConstructor && (mode & EXPRmode) != 0) {
                res.tpe = res.tpe.notNull
              }
              */
              // TODO: In theory we should be able to call:
              //if (fun2.hasSymbol && fun2.symbol.name == nme.apply && fun2.symbol.owner == ArrayClass) {
              // But this causes cyclic reference for Array class in Cleanup. It is easy to overcome this
              // by calling ArrayClass.info here (or some other place before specialize).
              if (fun2.symbol == Array_apply) {
                val checked = gen.mkCheckInit(res)
                // this check is needed to avoid infinite recursion in Duplicators
                // (calling typed1 more than once for the same tree)
                if (checked ne res) typed { atPos(tree.pos)(checked) }
                else res
              } else if ((mode & FUNmode) == 0 && fun2.hasSymbol && fun2.symbol.isMacro)
                typed1(macroExpand(res), mode, pt)
              else
                res
            case ex: TypeError =>
              fun match {
                case Select(qual, name)
                if !isPatternMode && nme.isOpAssignmentName(name.decode) =>
                  val qual1 = typedQualifier(qual)
                  if (treeInfo.isVariableOrGetter(qual1)) {
                    stopTimer(failedOpEqNanos, opeqStart)
                    convertToAssignment(fun, qual1, name, args, ex)
                  } 
                  else {
                    stopTimer(failedApplyNanos, appStart)
                    reportTypeError(fun.pos, ex)
                    setError(tree)
                  }
                case _ =>
                  stopTimer(failedApplyNanos, appStart)
                  reportTypeError(fun.pos, ex)
                  setError(tree)
              }
          }
        }
      }

      def convertToAssignment(fun: Tree, qual: Tree, name: Name, args: List[Tree], ex: TypeError): Tree = {
        val prefix = name.subName(0, name.length - nme.EQL.length)
        def mkAssign(vble: Tree): Tree =
          Assign(
            vble,
            Apply(
              Select(vble.duplicate, prefix) setPos fun.pos.focus, args) setPos tree.pos.makeTransparent
          ) setPos tree.pos

        def mkUpdate(table: Tree, indices: List[Tree]) = {
          gen.evalOnceAll(table :: indices, context.owner, context.unit) { ts =>
            val tab = ts.head
            val is = ts.tail
            Apply(
               Select(tab(), nme.update) setPos table.pos,
               ((is map (i => i())) ::: List(
                 Apply(
                   Select(
                     Apply(
                       Select(tab(), nme.apply) setPos table.pos,
                       is map (i => i())) setPos qual.pos,
                     prefix) setPos fun.pos,
                   args) setPos tree.pos)
               )
             ) setPos tree.pos
           }
        }

        val tree1 = qual match {
          case Ident(_) =>
            mkAssign(qual)

          case Select(qualqual, vname) =>
            gen.evalOnce(qualqual, context.owner, context.unit) { qq =>
              val qq1 = qq()
              mkAssign(Select(qq1, vname) setPos qual.pos)
            }

          case Apply(fn, indices) =>
            treeInfo.methPart(fn) match {
              case Select(table, nme.apply) => mkUpdate(table, indices)
              case _                        => errorTree(qual, "Unexpected tree during assignment conversion.")
            }
        }
        typed1(tree1, mode, pt)
/*
        debuglog("retry assign: "+tree1)
        silent(_.typed1(tree1, mode, pt)) match {
          case t: Tree =>
            t
          case _ =>
            reportTypeError(tree.pos, ex)
            setError(tree)
        }
*/
      }

      def qualifyingClassSym(qual: Name): Symbol =
        if (tree.symbol != NoSymbol) tree.symbol else qualifyingClass(tree, qual, false)

      def typedSuper(qual: Tree, mix: TypeName) = {
        val qual1 = typed(qual)

        val clazz = qual1 match {
          case This(_) => qual1.symbol
          case _ => qual1.tpe.typeSymbol
        }
        //println(clazz+"/"+qual1.tpe.typeSymbol+"/"+qual1)

        def findMixinSuper(site: Type): Type = {
          var ps = site.parents filter (_.typeSymbol.name == mix)
          if (ps.isEmpty)
            ps = site.parents filter (_.typeSymbol.toInterface.name == mix)
          if (ps.isEmpty) {
            if (settings.debug.value)
              Console.println(site.parents map (_.typeSymbol.name))//debug
            if (phase.erasedTypes && context.enclClass.owner.isImplClass) {
              // println(qual1)
              // println(clazz)
              // println(site)
              // println(site.parents)
              // println(mix)
              // the reference to super class got lost during erasure
              restrictionError(tree.pos, unit, "traits may not select fields or methods from super[C] where C is a class")
            } else {
              error(tree.pos, mix+" does not name a parent class of "+clazz)
            }
            ErrorType
          } else if (!ps.tail.isEmpty) {
            error(tree.pos, "ambiguous parent class qualifier")
            ErrorType
          } else {
            ps.head
          }
        }

        val owntype =
          if (mix.isEmpty) {
            if ((mode & SUPERCONSTRmode) != 0)
              if (clazz.info.parents.isEmpty) AnyRefClass.tpe // can happen due to cyclic references ==> #1036
              else clazz.info.parents.head
            else intersectionType(clazz.info.parents)
          } else {
            findMixinSuper(clazz.tpe)
          }

          treeCopy.Super(tree, qual1, mix) setType SuperType(clazz.thisType, owntype)
        }

      def typedThis(qual: Name) = {
        val clazz = qualifyingClassSym(qual)
        if (clazz == NoSymbol) setError(tree)
        else {
          tree setSymbol clazz setType clazz.thisType.underlying
          if (isStableContext(tree, mode, pt)) tree setType clazz.thisType
          tree
        }
      }

      /** Attribute a selection where <code>tree</code> is <code>qual.name</code>.
       *  <code>qual</code> is already attributed.
       *
       *  @param qual ...
       *  @param name ...
       *  @return     ...
       */
      def typedSelect(qual: Tree, name: Name): Tree = {
        val sym =
          if (tree.symbol != NoSymbol) {
            if (phase.erasedTypes && qual.isInstanceOf[Super])
              qual.tpe = tree.symbol.owner.tpe
            if (false && settings.debug.value) { // todo: replace by settings.check.value?
              val alts = qual.tpe.member(tree.symbol.name).alternatives
              if (!(alts exists (alt =>
                alt == tree.symbol || alt.isTerm && (alt.tpe matches tree.symbol.tpe))))
                assert(false, "symbol "+tree.symbol+tree.symbol.locationString+" not in "+alts+" of "+qual.tpe+
                       "\n members = "+qual.tpe.members+
                       "\n type history = "+qual.tpe.termSymbol.infosString+
                       "\n phase = "+phase)
            }
            tree.symbol
          } else {
            member(qual, name)
          }
        if (sym == NoSymbol && name != nme.CONSTRUCTOR && (mode & EXPRmode) != 0) {
          val qual1 =
            if (member(qual, name) != NoSymbol) qual
            else adaptToMemberWithArgs(tree, qual, name, mode)
          if (qual1 ne qual) return typed(treeCopy.Select(tree, qual1, name), mode, pt)
        }

        if (!reallyExists(sym)) {
          if (context.owner.toplevelClass.isJavaDefined && name.isTypeName) {
            val tree1 = atPos(tree.pos) { gen.convertToSelectFromType(qual, name)  }
            if (tree1 != EmptyTree) return typed1(tree1, mode, pt)
          }

          // try to expand according to Dynamic rules.

          if (settings.Xexperimental.value && (qual.tpe.widen.typeSymbol isNonBottomSubClass DynamicClass)) {
            var dynInvoke = Apply(Select(qual, nme.applyDynamic), List(Literal(Constant(name.decode))))
            context.tree match {
              case Apply(tree1, args) if tree1 eq tree =>
                ;
              case _ =>
                dynInvoke = Apply(dynInvoke, List())
            }
            return typed1(util.trace("dynatype: ")(dynInvoke), mode, pt)
          }

          if (settings.debug.value) {
            log(
              "qual = "+qual+":"+qual.tpe+
              "\nSymbol="+qual.tpe.termSymbol+"\nsymbol-info = "+qual.tpe.termSymbol.info+
              "\nscope-id = "+qual.tpe.termSymbol.info.decls.hashCode()+"\nmembers = "+qual.tpe.members+
              "\nname = "+name+"\nfound = "+sym+"\nowner = "+context.enclClass.owner
            )
          }

          def makeErrorTree = {
            val tree1 = tree match {
              case Select(_, _) => treeCopy.Select(tree, qual, name)
              case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
            }
            setError(tree1)
          }

          if (name == nme.ERROR && forInteractive)
            return makeErrorTree

          if (!qual.tpe.widen.isErroneous) {
            if ((mode & QUALmode) != 0) {
              val lastTry = missingHook(qual.tpe.typeSymbol, name)
              if (lastTry != NoSymbol) return typed1(tree setSymbol lastTry, mode, pt)
            }
            notAMemberError(tree.pos, qual, name)
          }

          if (forInteractive) makeErrorTree else setError(tree)
        } else {
          val tree1 = tree match {
            case Select(_, _) => treeCopy.Select(tree, qual, name)
            case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
          }
          val (tree2, pre2) = makeAccessible(tree1, sym, qual.tpe, qual)
          val result = stabilize(tree2, pre2, mode, pt)

          def isPotentialNullDeference() = {
            !isPastTyper &&
            !sym.isConstructor &&
            !(qual.tpe <:< NotNullClass.tpe) && !qual.tpe.isNotNull &&
            !(List(Any_isInstanceOf, Any_asInstanceOf) contains result.symbol)  // null.is/as is not a dereference
          }
          // unit is null here sometimes; how are we to know when unit might be null? (See bug #2467.)
          if (settings.warnSelectNullable.value && isPotentialNullDeference && unit != null)
            unit.warning(tree.pos, "potential null pointer dereference: "+tree)

          val selection = result match {
            // could checkAccessible (called by makeAccessible) potentially have skipped checking a type application in qual?
            case SelectFromTypeTree(qual@TypeTree(), name) if qual.tpe.typeArgs nonEmpty => // TODO: somehow the new qual is not checked in refchecks
              treeCopy.SelectFromTypeTree(
                result,
                (TypeTreeWithDeferredRefCheck(){ () => val tp = qual.tpe; val sym = tp.typeSymbolDirect
                  // will execute during refchecks -- TODO: make private checkTypeRef in refchecks public and call that one?
                  checkBounds(qual.pos, tp.prefix, sym.owner, sym.typeParams, tp.typeArgs, "")
                  qual // you only get to see the wrapped tree after running this check :-p
                }) setType qual.tpe setPos qual.pos,
                name)
            case accErr: Inferencer#AccessError =>
              val qual1 =
                try adaptToMemberWithArgs(tree, qual, name, mode)
                catch { case _: TypeError => qual }
              if (qual1 ne qual) typed(Select(qual1, name) setPos tree.pos, mode, pt)
              else accErr.emit()
            case _ =>
              result
          }
          // To fully benefit from special casing the return type of
          // getClass, we have to catch it immediately so expressions
          // like x.getClass().newInstance() are typed with the type of x.
          val isRefinableGetClass = (
               selection.symbol.name == nme.getClass_
            && selection.tpe.params.isEmpty
            // TODO: If the type of the qualifier is inaccessible, we can cause private types
            // to escape scope here, e.g. pos/t1107.  I'm not sure how to properly handle this
            // so for now it requires the type symbol be public.
            && qual.tpe.typeSymbol.isPublic
          )
          if (isRefinableGetClass)
              selection setType MethodType(Nil, erasure.getClassReturnType(qual.tpe))
          else
            selection
        }
      }

      /** Attribute an identifier consisting of a simple name or an outer reference.
       *
       *  @param tree      The tree representing the identifier.
       *  @param name      The name of the identifier.
       *  Transformations: (1) Prefix class members with this.
       *                   (2) Change imported symbols to selections
       */
      def typedIdent(name: Name): Tree = {
        def ambiguousError(msg: String) =
          error(tree.pos, "reference to " + name + " is ambiguous;\n" + msg)

        var defSym: Symbol = tree.symbol  // the directly found symbol
        var pre: Type = NoPrefix          // the prefix type of defSym, if a class member
        var qual: Tree = EmptyTree        // the qualifier tree if transformed tree is a select
        var inaccessibleSym: Symbol = NoSymbol // the first symbol that was found but that was discarded
                                          // for being inaccessible; used for error reporting
        var inaccessibleExplanation: String = ""

        // If a special setting is given, the empty package will be checked as a
        // last ditch effort before failing.  This method sets defSym and returns
        // true if a member of the given name exists.
        def checkEmptyPackage(): Boolean = {
          defSym = EmptyPackageClass.tpe.nonPrivateMember(name)
          defSym != NoSymbol
        }
        def startingIdentContext = (
          // ignore current variable scope in patterns to enforce linearity
          if ((mode & (PATTERNmode | TYPEPATmode)) == 0) context
          else context.outer
        )
        // A symbol qualifies if it exists and is not stale. Stale symbols
        // are made to disappear here. In addition,
        // if we are in a constructor of a pattern, we ignore all definitions
        // which are methods (note: if we don't do that
        // case x :: xs in class List would return the :: method)
        // unless they are stable or are accessors (the latter exception is for better error messages).
        def qualifies(sym: Symbol): Boolean = {
          sym.hasRawInfo &&       // this condition avoids crashing on self-referential pattern variables
          reallyExists(sym) &&
          ((mode & PATTERNmode | FUNmode) != (PATTERNmode | FUNmode) || !sym.isSourceMethod || sym.hasFlag(ACCESSOR))
        }

        if (defSym == NoSymbol) {
          var defEntry: ScopeEntry = null // the scope entry of defSym, if defined in a local scope

          var cx = startingIdentContext
          while (defSym == NoSymbol && cx != NoContext) {
            currentRun.compileSourceFor(context.asInstanceOf[analyzer.Context], name)
            pre = cx.enclClass.prefix
            defEntry = cx.scope.lookupEntry(name)
            if ((defEntry ne null) && qualifies(defEntry.sym)) {
              defSym = defEntry.sym
            }
            else {
              cx = cx.enclClass
              val foundSym = pre.member(name) filter qualifies
              defSym = foundSym filter (context.isAccessible(_, pre, false))
              if (defSym == NoSymbol) {
                if ((foundSym ne NoSymbol) && (inaccessibleSym eq NoSymbol)) {
                  inaccessibleSym = foundSym
                  inaccessibleExplanation = analyzer.lastAccessCheckDetails
                }
                cx = cx.outer
              }
            }
          }

          val symDepth = if (defEntry eq null) cx.depth
                         else cx.depth - (cx.scope.nestingLevel - defEntry.owner.nestingLevel)
          var impSym: Symbol = NoSymbol      // the imported symbol
          var imports = context.imports      // impSym != NoSymbol => it is imported from imports.head
          while (!reallyExists(impSym) && !imports.isEmpty && imports.head.depth > symDepth) {
            impSym = imports.head.importedSymbol(name)
            if (!impSym.exists) imports = imports.tail
          }

          // detect ambiguous definition/import,
          // update `defSym` to be the final resolved symbol,
          // update `pre` to be `sym`s prefix type in case it is an imported member,
          // and compute value of:

          if (defSym.exists && impSym.exists) {
            // imported symbols take precedence over package-owned symbols in different
            // compilation units. Defined symbols take precedence over erroneous imports.
            if (defSym.isDefinedInPackage &&
                (!currentRun.compiles(defSym) ||
                 context.unit.exists && defSym.sourceFile != context.unit.source.file))
              defSym = NoSymbol
            else if (impSym.isError || impSym.name == nme.CONSTRUCTOR)
              impSym = NoSymbol
          }
          if (defSym.exists) {
            if (impSym.exists)
              ambiguousError(
                "it is both defined in "+defSym.owner +
                " and imported subsequently by \n"+imports.head)
            else if (!defSym.owner.isClass || defSym.owner.isPackageClass || defSym.isTypeParameterOrSkolem)
              pre = NoPrefix
            else
              qual = atPos(tree.pos.focusStart)(gen.mkAttributedQualifier(pre))
          } else {
            if (impSym.exists) {
              var impSym1 = NoSymbol
              var imports1 = imports.tail
              def ambiguousImport() = {
                if (!(imports.head.qual.tpe =:= imports1.head.qual.tpe))
                  ambiguousError(
                    "it is imported twice in the same scope by\n"+imports.head +  "\nand "+imports1.head)
              }
              while (!imports1.isEmpty &&
                     (!imports.head.isExplicitImport(name) ||
                      imports1.head.depth == imports.head.depth)) {
                var impSym1 = imports1.head.importedSymbol(name)
                if (reallyExists(impSym1)) {
                  if (imports1.head.isExplicitImport(name)) {
                    if (imports.head.isExplicitImport(name) ||
                        imports1.head.depth != imports.head.depth) ambiguousImport()
                    impSym = impSym1
                    imports = imports1
                  } else if (!imports.head.isExplicitImport(name) &&
                             imports1.head.depth == imports.head.depth) ambiguousImport()
                }
                imports1 = imports1.tail
              }
              defSym = impSym
              val qual0 = imports.head.qual
              if (!(shortenImports && qual0.symbol.isPackage)) // optimization: don't write out package prefixes
                qual = atPos(tree.pos.focusStart)(resetPos(qual0.duplicate))
              pre = qual.tpe
            }
            else if (settings.exposeEmptyPackage.value && checkEmptyPackage())
              log("Allowing empty package member " + name + " due to settings.")
            else {
              if ((mode & QUALmode) != 0) {
                val lastTry = missingHook(RootClass, name)
                if (lastTry != NoSymbol) return typed1(tree setSymbol lastTry, mode, pt)
              }
              if (settings.debug.value) {
                log(context.imports)//debug
              }
              if (inaccessibleSym eq NoSymbol) {
                // Avoiding some spurious error messages: see SI-2388.
                if (reporter.hasErrors && (name startsWith tpnme.ANON_CLASS_NAME)) ()
                else {
                  val similar = (
                    // name length check to limit unhelpful suggestions for e.g. "x" and "b1"
                    if (name.length > 2) {
                      val allowed = (
                        startingIdentContext.enclosingContextChain
                            flatMap (ctx => ctx.scope.toList ++ ctx.imports.flatMap(_.allImportedSymbols))
                             filter (sym => sym.isTerm == name.isTermName)
                          filterNot (sym => sym.isPackage || sym.isSynthetic || sym.hasMeaninglessName)
                      )
                      val allowedStrings = (
                        allowed.map("" + _.name).distinct.sorted
                          filterNot (s => (s contains '$') || (s contains ' '))
                      )
                      similarString("" + name, allowedStrings)
                    }
                    else ""
                  )
                  error(tree.pos, "not found: "+decodeWithKind(name, context.owner) + similar)
                }
              }
              else new AccessError(
                tree, inaccessibleSym, context.enclClass.owner.thisType,
                inaccessibleExplanation
              ).emit()
              defSym = context.owner.newErrorSymbol(name)
            }
          }
        }
        if (defSym.owner.isPackageClass) pre = defSym.owner.thisType
        if (defSym.isThisSym) {
          typed1(This(defSym.owner) setPos tree.pos, mode, pt)
        } else {
          val tree1 = if (qual == EmptyTree) tree
                      else atPos(tree.pos)(Select(qual, name))
                    // atPos necessary because qualifier might come from startContext
          val (tree2, pre2) = makeAccessible(tree1, defSym, pre, qual)
          // assert(pre.typeArgs isEmpty) // no need to add #2416-style check here, right?
          stabilize(tree2, pre2, mode, pt) match {
            case accErr: Inferencer#AccessError => accErr.emit()
            case result => result
          }
        }
      }

      def typedCompoundTypeTree(templ: Template) = {
        val parents1 = templ.parents mapConserve (typedType(_, mode))
        if (parents1 exists (_.tpe.isError)) tree setType ErrorType
        else {
          val decls = new Scope
          //Console.println("Owner: " + context.enclClass.owner + " " + context.enclClass.owner.id)
          val self = refinedType(parents1 map (_.tpe), context.enclClass.owner, decls, templ.pos)
          newTyper(context.make(templ, self.typeSymbol, decls)).typedRefinement(templ.body)
          tree setType self
        }
      }

      def typedAppliedTypeTree(tpt: Tree, args: List[Tree]) = {
        val tpt1 = typed1(tpt, mode | FUNmode | TAPPmode, WildcardType)
        if (tpt1.tpe.isError) {
          setError(tree)
        } else if (!tpt1.hasSymbol) {
          errorTree(tree, tpt1.tpe+" does not take type parameters")
        } else {
          val tparams = tpt1.symbol.typeParams
          if (sameLength(tparams, args)) {
            // @M: kind-arity checking is done here and in adapt, full kind-checking is in checkKindBounds (in Infer)
            val args1 =
              if (!tpt1.symbol.rawInfo.isComplete)
                args mapConserve (typedHigherKindedType(_, mode))
                // if symbol hasn't been fully loaded, can't check kind-arity
              else map2Conserve(args, tparams) { (arg, tparam) =>
                //@M! the polytype denotes the expected kind
                typedHigherKindedType(arg, mode, polyType(tparam.typeParams, AnyClass.tpe))
              }
            val argtypes = args1 map (_.tpe)

            (args, tparams).zipped foreach { (arg, tparam) => arg match {
              // note: can't use args1 in selector, because Bind's got replaced
              case Bind(_, _) =>
                if (arg.symbol.isAbstractType)
                  arg.symbol setInfo // XXX, feedback. don't trackSymInfo here!
                    TypeBounds(
                      lub(List(arg.symbol.info.bounds.lo, tparam.info.bounds.lo.subst(tparams, argtypes))),
                      glb(List(arg.symbol.info.bounds.hi, tparam.info.bounds.hi.subst(tparams, argtypes))))
              case _ =>
            }}
            val original = treeCopy.AppliedTypeTree(tree, tpt1, args1)
            val result = TypeTree(appliedType(tpt1.tpe, argtypes)) setOriginal original
            if(tpt1.tpe.isInstanceOf[PolyType]) // did the type application (performed by appliedType) involve an unchecked beta-reduction?
              TypeTreeWithDeferredRefCheck(){ () =>
                // wrap the tree and include the bounds check -- refchecks will perform this check (that the beta reduction was indeed allowed) and unwrap
                // we can't simply use original in refchecks because it does not contains types
                // (and the only typed trees we have have been mangled so they're not quite the original tree anymore)
                checkBounds(result.pos, tpt1.tpe.prefix, tpt1.symbol.owner, tpt1.symbol.typeParams, argtypes, "")
                result // you only get to see the wrapped tree after running this check :-p
              } setType (result.tpe) setPos(result.pos)
            else result
          } else if (tparams.isEmpty) {
            errorTree(tree, tpt1.tpe+" does not take type parameters")
          } else {
            //Console.println("\{tpt1}:\{tpt1.symbol}:\{tpt1.symbol.info}")
            if (settings.debug.value) Console.println(tpt1+":"+tpt1.symbol+":"+tpt1.symbol.info)//debug
            errorTree(tree, "wrong number of type arguments for "+tpt1.tpe+", should be "+tparams.length)
          }
        }
      }

      def adaptCase(cdef: CaseDef, tpe: Type): CaseDef =
        treeCopy.CaseDef(cdef, cdef.pat, cdef.guard, adapt(cdef.body, mode, tpe))

      // begin typed1
      val sym: Symbol = tree.symbol
      if ((sym ne null) && (sym ne NoSymbol)) sym.initialize
      //if (settings.debug.value && tree.isDef) log("typing definition of "+sym);//DEBUG
      tree match {
        case PackageDef(pid, stats) =>
          val pid1 = typedQualifier(pid).asInstanceOf[RefTree]
          assert(sym.moduleClass ne NoSymbol, sym)
          // complete lazy annotations
          val annots = sym.annotations
          val stats1 = newTyper(context.make(tree, sym.moduleClass, sym.info.decls))
            .typedStats(stats, NoSymbol)
          treeCopy.PackageDef(tree, pid1, stats1) setType NoType

        case tree @ ClassDef(_, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).typedClassDef(tree)

        case tree @ ModuleDef(_, _, _) =>
          newTyper(context.makeNewScope(tree, sym.moduleClass)).typedModuleDef(tree)

        case vdef @ ValDef(_, _, _, _) =>
          typedValDef(vdef)

        case ddef @ DefDef(_, _, _, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).typedDefDef(ddef)

        case tdef @ TypeDef(_, _, _, _) =>
          typedTypeDef(tdef)

        case ldef @ LabelDef(_, _, _) =>
          labelTyper(ldef).typedLabelDef(ldef)

        case ddef @ DocDef(comment, defn) =>
          if (forScaladoc && (sym ne null) && (sym ne NoSymbol)) {
            docComments(sym) = comment
            comment.defineVariables(sym)
            val typer1 = newTyper(context.makeNewScope(tree, context.owner))
            for (useCase <- comment.useCases) {
              typer1.silent(_.typedUseCase(useCase)) match {
                case ex: TypeError =>
                  unit.warning(useCase.pos, ex.msg)
                case _ =>
              }
              for (useCaseSym <- useCase.defined) {
                if (sym.name != useCaseSym.name)
                  unit.warning(useCase.pos, "@usecase " + useCaseSym.name.decode + " does not match commented symbol: " + sym.name.decode)
              }
            }
          }
          typed(defn, mode, pt)

        case Annotated(constr, arg) =>
          typedAnnotated(constr, typed(arg, mode, pt))

        case tree @ Block(_, _) =>
          newTyper(context.makeNewScope(tree, context.owner))
            .typedBlock(tree, mode, pt)

        case Alternative(alts) =>
          val alts1 = alts mapConserve (alt => typed(alt, mode | ALTmode, pt))
          treeCopy.Alternative(tree, alts1) setType pt

        case Star(elem) =>
          checkStarPatOK(tree.pos, mode)
          val elem1 = typed(elem, mode, pt)
          treeCopy.Star(tree, elem1) setType makeFullyDefined(pt)

        case Bind(name, body) =>
          typedBind(name, body)

        case UnApply(fun, args) =>
          val fun1 = typed(fun)
          val tpes = formalTypes(unapplyTypeList(fun.symbol, fun1.tpe), args.length)
          val args1 = (args, tpes).zipped map typedPattern
          treeCopy.UnApply(tree, fun1, args1) setType pt

        case ArrayValue(elemtpt, elems) =>
          typedArrayValue(elemtpt, elems)

        case tree @ Function(_, _) =>
          if (tree.symbol == NoSymbol)
            tree.symbol = context.owner.newValue(tree.pos, nme.ANON_FUN_NAME)
              .setFlag(SYNTHETIC).setInfo(NoType)
          newTyper(context.makeNewScope(tree, tree.symbol)).typedFunction(tree, mode, pt)

        case Assign(lhs, rhs) =>
          typedAssign(lhs, rhs)

        case AssignOrNamedArg(lhs, rhs) => // called by NamesDefaults in silent typecheck
          typedAssign(lhs, rhs)

        case If(cond, thenp, elsep) =>
          typedIf(cond, thenp, elsep)

        case tree @ Match(selector, cases) =>
          typedMatch(tree, selector, cases)

        case Return(expr) =>
          typedReturn(expr)

        case Try(block, catches, finalizer) =>
          var block1 = typed(block, pt)
          var catches1 = typedCases(tree, catches, ThrowableClass.tpe, pt)
          val finalizer1 = if (finalizer.isEmpty) finalizer
                           else typed(finalizer, UnitClass.tpe)
          val (owntype, needAdapt) = ptOrLub(block1.tpe :: (catches1 map (_.tpe)))
          if (needAdapt) {
            block1 = adapt(block1, mode, owntype)
            catches1 = catches1 map (adaptCase(_, owntype))
          }
          treeCopy.Try(tree, block1, catches1, finalizer1) setType owntype

        case Throw(expr) =>
          val expr1 = typed(expr, EXPRmode | BYVALmode, ThrowableClass.tpe)
          treeCopy.Throw(tree, expr1) setType NothingClass.tpe

        case New(tpt: Tree) =>
          typedNew(tpt)

        case Typed(expr, Function(List(), EmptyTree)) =>
          typedEta(checkDead(typed1(expr, mode, pt)))

        case Typed(expr, tpt @ Ident(tpnme.WILDCARD_STAR))  =>
          val expr0 = typed(expr, onlyStickyModes(mode), WildcardType)
          def subArrayType(pt: Type) =
            if (isValueClass(pt.typeSymbol) || !isFullyDefined(pt)) arrayType(pt)
            else {
              val tparam = context.owner freshExistential "" setInfo TypeBounds.upper(pt)
              ExistentialType(List(tparam), arrayType(tparam.tpe))
            }
          val (expr1, baseClass) = expr0.tpe.typeSymbol match {
            case ArrayClass => (adapt(expr0, onlyStickyModes(mode), subArrayType(pt)), ArrayClass)
            case _          => (adapt(expr0, onlyStickyModes(mode), seqType(pt)), SeqClass)
          }
          expr1.tpe.baseType(baseClass) match {
            case TypeRef(_, _, List(elemtp)) =>
              treeCopy.Typed(tree, expr1, tpt setType elemtp) setType elemtp
            case _ =>
              setError(tree)
          }

        case Typed(expr, tpt) =>
          val tpt1 = typedType(tpt, mode)
          val expr1 = typed(expr, onlyStickyModes(mode), tpt1.tpe.deconst)
          val owntype =
            if (isPatternMode) inferTypedPattern(tpt1.pos, tpt1.tpe, pt)
            else tpt1.tpe
          //Console.println(typed pattern: "+tree+":"+", tp = "+tpt1.tpe+", pt = "+pt+" ==> "+owntype)//DEBUG
          treeCopy.Typed(tree, expr1, tpt1) setType owntype

        case TypeApply(fun, args) =>
          // @M: kind-arity checking is done here and in adapt, full kind-checking is in checkKindBounds (in Infer)
          //@M! we must type fun in order to type the args, as that requires the kinds of fun's type parameters.
          // However, args should apparently be done first, to save context.undetparams. Unfortunately, the args
          // *really* have to be typed *after* fun. We escape from this classic Catch-22 by simply saving&restoring undetparams.

          // @M TODO: the compiler still bootstraps&all tests pass when this is commented out..
          //val undets = context.undetparams

          // @M: fun is typed in TAPPmode because it is being applied to its actual type parameters
          val fun1 = typed(fun, forFunMode(mode) | TAPPmode, WildcardType)
          val tparams = fun1.symbol.typeParams

          //@M TODO: val undets_fun = context.undetparams  ?
          // "do args first" (by restoring the context.undetparams) in order to maintain context.undetparams on the function side.

          // @M TODO: the compiler still bootstraps when this is commented out.. TODO: run tests
          //context.undetparams = undets

          // @M maybe the well-kindedness check should be done when checking the type arguments conform to the type parameters' bounds?
          val args1 = if (sameLength(args, tparams)) map2Conserve(args, tparams) {
                        //@M! the polytype denotes the expected kind
                        (arg, tparam) => typedHigherKindedType(arg, mode, polyType(tparam.typeParams, AnyClass.tpe))
                      } else {
                      //@M  this branch is correctly hit for an overloaded polymorphic type. It also has to handle erroneous cases.
                      // Until the right alternative for an overloaded method is known, be very liberal,
                      // typedTypeApply will find the right alternative and then do the same check as
                      // in the then-branch above. (see pos/tcpoly_overloaded.scala)
                      // this assert is too strict: be tolerant for errors like trait A { def foo[m[x], g]=error(""); def x[g] = foo[g/*ERR: missing argument type*/] }
                      //assert(fun1.symbol.info.isInstanceOf[OverloadedType] || fun1.symbol.isError) //, (fun1.symbol,fun1.symbol.info,fun1.symbol.info.getClass,args,tparams))
                        args mapConserve (typedHigherKindedType(_, mode))
                      }

          //@M TODO: context.undetparams = undets_fun ?
          typedTypeApply(tree, mode, fun1, args1)

        case Apply(Block(stats, expr), args) =>
          typed1(atPos(tree.pos)(Block(stats, Apply(expr, args))), mode, pt)

        case Apply(fun, args) =>
          typedApply(fun, args) match {
            case Apply(Select(New(tpt), name), args)
            if (tpt.tpe != null &&
                tpt.tpe.typeSymbol == ArrayClass &&
                args.length == 1 &&
                erasure.GenericArray.unapply(tpt.tpe).isDefined) => // !!! todo simplify by using extractor
              // convert new Array[T](len) to evidence[ClassManifest[T]].newArray(len)
              // convert new Array^N[T](len) for N > 1 to evidence[ClassManifest[T]].newArrayN(len)
              val Some((level, manifType)) = erasure.GenericArray.unapply(tpt.tpe)
              if (level > MaxArrayDims)
                error(tree.pos, "cannot create a generic multi-dimensional array of more than "+MaxArrayDims+" dimensions")
              val newArrayApp = atPos(tree.pos) {
                val manif = getManifestTree(tree.pos, manifType, false)
                new ApplyToImplicitArgs(Select(manif, if (level == 1) "newArray" else "newArray"+level), args)
              }
              typed(newArrayApp, mode, pt)
            case tree1 =>
              tree1
          }

        case ApplyDynamic(qual, args) =>
          val reflectiveCalls = !(settings.refinementMethodDispatch.value == "invoke-dynamic")
          val qual1 = typed(qual, AnyRefClass.tpe)
          val args1 = args mapConserve (arg => if (reflectiveCalls) typed(arg, AnyRefClass.tpe) else typed(arg))
          treeCopy.ApplyDynamic(tree, qual1, args1) setType (if (reflectiveCalls) AnyRefClass.tpe else tree.symbol.info.resultType)

        case Super(qual, mix) =>
          typedSuper(qual, mix)

        case This(qual) =>
          typedThis(qual)

        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          val qual1 =
            typed(qual, EXPRmode | QUALmode | POLYmode | SUPERCONSTRmode, WildcardType)
          // the qualifier type of a supercall constructor is its first parent class
          typedSelect(qual1, nme.CONSTRUCTOR)

        case Select(qual, name) =>
          incCounter(typedSelectCount)
          var qual1 = checkDead(typedQualifier(qual, mode))
          if (name.isTypeName) qual1 = checkStable(qual1)

          val tree1 = // temporarily use `filter` and an alternative for `withFilter`
            if (name == nme.withFilter)
              silent(_ => typedSelect(qual1, name)) match {
                case result1: Tree =>
                  result1
                case ex1: TypeError =>
                  silent(_ => typed1(Select(qual1, nme.filter) setPos tree.pos, mode, pt)) match {
                    case result2: Tree =>
                      unit.deprecationWarning(
                        tree.pos, "`withFilter' method does not yet exist on "+qual1.tpe.widen+
                        ", using `filter' method instead")
                      result2
                    case ex2: TypeError =>
                      reportTypeError(tree.pos, ex1)
                      setError(tree)
                  }
              }
            else
              typedSelect(qual1, name)

          if (qual1.symbol == RootPackage) treeCopy.Ident(tree1, name)
          else tree1

        case Ident(name) =>
          incCounter(typedIdentCount)
          if ((name == nme.WILDCARD && (mode & (PATTERNmode | FUNmode)) == PATTERNmode) ||
              (name == tpnme.WILDCARD && (mode & TYPEmode) != 0))
            tree setType makeFullyDefined(pt)
          else
            typedIdent(name)

        case Literal(value) =>
          tree setType (
            if (value.tag == UnitTag) UnitClass.tpe
            else ConstantType(value))

        case SingletonTypeTree(ref) =>
          val ref1 = checkStable(
            typed(ref, EXPRmode | QUALmode | (mode & TYPEPATmode), AnyRefClass.tpe))
          tree setType ref1.tpe.resultType

        case SelectFromTypeTree(qual, selector) =>
          val qual1 = typedType(qual, mode)
          if (qual1.tpe.isVolatile) error(tree.pos, "illegal type selection from volatile type "+qual.tpe)
          typedSelect(qual1, selector)

        case CompoundTypeTree(templ) =>
          typedCompoundTypeTree(templ)

        case AppliedTypeTree(tpt, args) =>
          typedAppliedTypeTree(tpt, args)

        case TypeBoundsTree(lo, hi) =>
          val lo1 = typedType(lo, mode)
          val hi1 = typedType(hi, mode)
          treeCopy.TypeBoundsTree(tree, lo1, hi1) setType TypeBounds(lo1.tpe, hi1.tpe)

        case etpt @ ExistentialTypeTree(_, _) =>
          newTyper(context.makeNewScope(tree, context.owner)).typedExistentialTypeTree(etpt, mode)

        case dc@TypeTreeWithDeferredRefCheck() => dc // TODO: should we re-type the wrapped tree? then we need to change TypeTreeWithDeferredRefCheck's representation to include the wrapped tree explicitly (instead of in its closure)
        case tpt @ TypeTree() =>
          if (tpt.original != null)
            tree setType typedType(tpt.original, mode).tpe
          else
            // we should get here only when something before failed
            // and we try again (@see tryTypedApply). In that case we can assign
            // whatever type to tree; we just have to survive until a real error message is issued.
            tree setType AnyClass.tpe
        case Import(expr, selectors) =>
          assert(forInteractive) // should not happen in normal circumstances.
          tree setType tree.symbol.tpe
        case _ =>
          abort("unexpected tree: " + tree.getClass + "\n" + tree)//debug
      }
    }

    /**
     *  @param tree ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      indentTyping()

      var alreadyTyped = false
      try {
        if (Statistics.enabled) {
          val t = currentTime()
          if (pendingTreeTypes.nonEmpty) {
            microsByType(pendingTreeTypes.head) += ((t - typerTime) / 1000).toInt
          }
          typerTime = t
          pendingTreeTypes = tree.getClass :: pendingTreeTypes
        }
        if (context.retyping &&
            (tree.tpe ne null) && (tree.tpe.isErroneous || !(tree.tpe <:< pt))) {
          tree.tpe = null
          if (tree.hasSymbol) tree.symbol = NoSymbol
        }

        alreadyTyped = tree.tpe ne null
        var tree1: Tree = if (alreadyTyped) tree else {
          printTyping(
            ptLine("typing %s: pt = %s".format(ptTree(tree), pt),
              "undetparams"      -> context.undetparams,
              "implicitsEnabled" -> context.implicitsEnabled,
              "silent"           -> !context.reportGeneralErrors,
              "context.owner"    -> context.owner
            )
          )
          val tree1 = typed1(tree, mode, dropExistential(pt))
          printTyping("typed %s: %s%s".format(
            ptTree(tree1), tree1.tpe,
            if (isSingleType(tree1.tpe)) " with underlying "+tree1.tpe.widen else "")
          )
          tree1
        }

        tree1.tpe = addAnnotations(tree1, tree1.tpe)
        val result = if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt, tree)

        if (!alreadyTyped) {
          printTyping("adapted %s: %s to %s, %s".format(
            tree1, tree1.tpe.widen, pt, context.undetparamsString)
          ) //DEBUG
        }

//      for (t <- tree1.tpe) assert(t != WildcardType)
//      if ((mode & TYPEmode) != 0) println("type: "+tree1+" has type "+tree1.tpe)
        if (!isPastTyper) signalDone(context.asInstanceOf[analyzer.Context], tree, result)
        result
      } catch {
        case ex: TypeError =>
          tree.tpe = null
          printTyping("caught %s: while typing %s".format(ex, tree)) //DEBUG
          reportTypeError(tree.pos, ex)
          setError(tree)
        case ex: Exception =>
          if (settings.debug.value) // @M causes cyclic reference error
            Console.println("exception when typing "+tree+", pt = "+pt)
          if (context != null && context.unit.exists && tree != null)
            logError("AT: " + (tree.pos).dbgString, ex)
          throw ex
      }
      finally {
        deindentTyping()

        if (Statistics.enabled) {
          val t = currentTime()
          microsByType(pendingTreeTypes.head) += ((t - typerTime) / 1000).toInt
          visitsByType(pendingTreeTypes.head) += 1
          typerTime = t
          pendingTreeTypes = pendingTreeTypes.tail
        }
      }
    }

    def atOwner(owner: Symbol): Typer =
      newTyper(context.make(context.tree, owner))

    def atOwner(tree: Tree, owner: Symbol): Typer =
      newTyper(context.make(tree, owner))

    /** Types expression or definition <code>tree</code>.
     *
     *  @param tree ...
     *  @return     ...
     */
    def typed(tree: Tree): Tree = {
      val ret = typed(tree, EXPRmode, WildcardType)
      ret
    }

    def typedPos(pos: Position)(tree: Tree) = typed(atPos(pos)(tree))
    // TODO: see if this formulation would impose any penalty, since
    // it makes for a lot less casting.
    // def typedPos[T <: Tree](pos: Position)(tree: T): T = typed(atPos(pos)(tree)).asInstanceOf[T]

    /** Types expression <code>tree</code> with given prototype <code>pt</code>.
     *
     *  @param tree ...
     *  @param pt   ...
     *  @return     ...
     */
    def typed(tree: Tree, pt: Type): Tree =
      typed(tree, EXPRmode, pt)

    /** Types qualifier <code>tree</code> of a select node.
     *  E.g. is tree occurs in a context like <code>tree.m</code>.
     */
    def typedQualifier(tree: Tree, mode: Int, pt: Type): Tree =
      typed(tree, EXPRmode | QUALmode | POLYmode | mode & TYPEPATmode, pt) // TR: don't set BYVALmode, since qualifier might end up as by-name param to an implicit

    /** Types qualifier <code>tree</code> of a select node.
     *  E.g. is tree occurs in a context like <code>tree.m</code>.
     */
    def typedQualifier(tree: Tree, mode: Int): Tree =
      typedQualifier(tree, mode, WildcardType)

    def typedQualifier(tree: Tree): Tree = typedQualifier(tree, NOmode, WildcardType)

    /** Types function part of an application */
    def typedOperator(tree: Tree): Tree =
      typed(tree, EXPRmode | FUNmode | POLYmode | TAPPmode, WildcardType)

    /** Types a pattern with prototype <code>pt</code> */
    def typedPattern(tree: Tree, pt: Type): Tree = {
      // We disable implicits because otherwise some constructs will
      // type check which should not.  The pattern matcher does not
      // perform implicit conversions in an attempt to consummate a match.
      context.withImplicitsDisabled(typed(tree, PATTERNmode, pt))
    }

    /** Types a (fully parameterized) type tree */
    def typedType(tree: Tree, mode: Int): Tree =
      typed(tree, forTypeMode(mode), WildcardType)

    /** Types a (fully parameterized) type tree */
    def typedType(tree: Tree): Tree = typedType(tree, NOmode)

    /** Types a higher-kinded type tree -- pt denotes the expected kind*/
    def typedHigherKindedType(tree: Tree, mode: Int, pt: Type): Tree =
      if (pt.typeParams.isEmpty) typedType(tree, mode) // kind is known and it's *
      else typed(tree, HKmode, pt)

    def typedHigherKindedType(tree: Tree, mode: Int): Tree =
      typed(tree, HKmode, WildcardType)

    def typedHigherKindedType(tree: Tree): Tree = typedHigherKindedType(tree, NOmode)

    /** Types a type constructor tree used in a new or supertype */
    def typedTypeConstructor(tree: Tree, mode: Int): Tree = {
      val result = typed(tree, forTypeMode(mode) | FUNmode, WildcardType)

      val restpe = result.tpe.normalize // normalize to get rid of type aliases for the following check (#1241)
      if (!phase.erasedTypes && restpe.isInstanceOf[TypeRef] && !restpe.prefix.isStable && !context.unit.isJava) {
        // The isJava exception if OK only because the only type constructors scalac gets
        // to see are those in the signatures. These do not need a unique object as a prefix.
        // The situation is different for new's and super's, but scalac does not look deep
        // enough to see those. See #3938
        error(tree.pos, restpe.prefix+" is not a legal prefix for a constructor")
      }

      //@M fix for #2208
      // if there are no type arguments, normalization does not bypass any checks, so perform it to get rid of AnyRef
      if(result.tpe.typeArgs.isEmpty) {
        // minimal check: if(result.tpe.typeSymbolDirect eq AnyRefClass) {
        // must expand the fake AnyRef type alias, because bootstrapping (init in Definitions) is not
        // designed to deal with the cycles in the scala package (ScalaObject extends
        // AnyRef, but the AnyRef type alias is entered after the scala package is
        // loaded and completed, so that ScalaObject is unpickled while AnyRef is not
        // yet defined )
        result setType(restpe)
      } else { // must not normalize: type application must be (bounds-)checked (during RefChecks), see #2208
        // during uncurry (after refchecks), all types are normalized
        result
      }
    }

    def typedTypeConstructor(tree: Tree): Tree = typedTypeConstructor(tree, NOmode)

    def computeType(tree: Tree, pt: Type): Type = {
      val tree1 = typed(tree, pt)
      transformed(tree) = tree1
      packedType(tree1, context.owner)
    }

    def transformedOrTyped(tree: Tree, mode: Int, pt: Type): Tree = transformed.get(tree) match {
      case Some(tree1) => transformed -= tree; tree1
      case None => typed(tree, mode, pt)
    }

    def findManifest(tp: Type, full: Boolean) = atPhase(currentRun.typerPhase) {
      inferImplicit(
        EmptyTree,
        appliedType((if (full) FullManifestClass else PartialManifestClass).typeConstructor, List(tp)),
        true, false, context)
    }

    def getManifestTree(pos: Position, tp: Type, full: Boolean): Tree = {
      val manifestOpt = findManifest(tp, full)
      if (manifestOpt.tree.isEmpty) {
        error(pos, "cannot find "+(if (full) "" else "class ")+"manifest for element type "+tp)
        Literal(Constant(null))
      } else {
        manifestOpt.tree
      }
    }
/*
    def convertToTypeTree(tree: Tree): Tree = tree match {
      case TypeTree() => tree
      case _ => TypeTree(tree.tpe)
    }
*/
  }
}

