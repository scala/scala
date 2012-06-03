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

import scala.collection.mutable
import scala.tools.nsc.util.BatchSourceFile
import mutable.ListBuffer
import symtab.Flags._
import util.Statistics
import util.Statistics._

// Suggestion check whether we can do without priming scopes with symbols of outer scopes,
// like the IDE does.
/** This trait provides methods to assign types to trees.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Typers extends Modes with Adaptations with Taggings {
  self: Analyzer =>

  import global._
  import definitions._
  import patmat.DefaultOverrideMatchAttachment

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

  // [Eugene] shouldn't this be converted to resetAllAttrs?
  object UnTyper extends Traverser {
    override def traverse(tree: Tree) = {
      if (tree != EmptyTree) tree.tpe = null
      if (tree.hasSymbol) tree.symbol = NoSymbol
      super.traverse(tree)
    }
  }
/* needed for experimental version where early types can be type arguments
  class EarlyMap(clazz: Symbol) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(NoPrefix, sym, List()) if (sym hasFlag PRESUPER) =>
        TypeRef(ThisType(clazz), sym, List())
      case _ =>
        mapOver(tp)
    }
  }
*/

  sealed abstract class SilentResult[+T]
  case class SilentTypeError(err: AbsTypeError) extends SilentResult[Nothing] { }
  case class SilentResultValue[+T](value: T) extends SilentResult[T] { }

  def newTyper(context: Context): Typer = new NormalTyper(context)
  private class NormalTyper(context : Context) extends Typer(context)

  // A transient flag to mark members of anonymous classes
  // that are turned private by typedBlock
  private final val SYNTHETIC_PRIVATE = TRANS_FLAG

  private def isPastTyper = phase.id > currentRun.typerPhase.id

  // To enable decent error messages when the typer crashes.
  // TODO - this only catches trees which go through def typed,
  // but there are all kinds of back ways - typedClassDef, etc. etc.
  // Funnel everything through one doorway.
  var lastTreeToTyper: Tree = EmptyTree

  // when true:
  //  - we may virtualize matches (if -Xexperimental and there's a suitable __match in scope)
  //  - we synthesize PartialFunction implementations for `x => x match {...}` and `match {...}` when the expected type is PartialFunction
  // this is disabled by: -Xoldpatmat, scaladoc or interactive compilation
  @inline private def newPatternMatching = opt.virtPatmat && !forScaladoc && !forInteractive // && (phase.id < currentRun.uncurryPhase.id)

  abstract class Typer(context0: Context) extends TyperDiagnostics with Adaptation with Tagging with TyperContextErrors {
    import context0.unit
    import typeDebug.{ ptTree, ptBlock, ptLine }
    import TyperErrorGen._

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
        // paramFailed cannot be initialized with params.exists(_.tpe.isError) because that would
        // hide some valid errors for params preceding the erroneous one.
        var paramFailed = false

        def mkPositionalArg(argTree: Tree, paramName: Name) = argTree
        def mkNamedArg(argTree: Tree, paramName: Name) = atPos(argTree.pos)(new AssignOrNamedArg(Ident(paramName), (argTree)))
        var mkArg: (Tree, Name) => Tree = mkPositionalArg

        // DEPMETTODO: instantiate type vars that depend on earlier implicit args (see adapt (4.1))
        //
        // apply the substitutions (undet type param -> type) that were determined
        // by implicit resolution of implicit arguments on the left of this argument
        for(param <- params) {
          var paramTp = param.tpe
          for(ar <- argResultsBuff)
            paramTp = paramTp.subst(ar.subst.from, ar.subst.to)

          val res = if (paramFailed || (paramTp.isError && {paramFailed = true; true})) SearchFailure else inferImplicit(fun, paramTp, context.reportErrors, false, context)
          argResultsBuff += res

          if (res != SearchFailure) {
            argBuff += mkArg(res.tree, param.name)
          } else {
            mkArg = mkNamedArg // don't pass the default argument (if any) here, but start emitting named arguments for the following args
            if (!param.hasDefault && !paramFailed) {
              context.errBuffer.find(_.kind == ErrorKinds.Divergent) match {
                case Some(divergentImplicit) =>
                  // DivergentImplicit error has higher priority than "no implicit found"
                  // no need to issue the problem again if we are still in silent mode
                  if (context.reportErrors) {
                    context.issue(divergentImplicit)
                    context.condBufferFlush(_.kind  == ErrorKinds.Divergent)
                  }
                case None =>
                  NoImplicitFoundError(fun, param)
              }
              paramFailed = true
            }
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

    def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean): Tree =
      inferView(tree, from, to, reportAmbiguous, true)

    /** Infer an implicit conversion (``view'') between two types.
     *  @param tree             The tree which needs to be converted.
     *  @param from             The source type of the conversion
     *  @param to               The target type of the conversion
     *  @param reportAmbiguous  Should ambiguous implicit errors be reported?
     *                          False iff we search for a view to find out
     *                          whether one type is coercible to another.
     *  @param saveErrors       Should ambiguous and divergent implicit errors that were buffered
     *                          during the inference of a view be put into the original buffer.
     *                          False iff we don't care about them.
     */
    def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean, saveErrors: Boolean): Tree = {
      debuglog("infer view from "+from+" to "+to)//debug
      if (isPastTyper) EmptyTree
      else from match {
        case MethodType(_, _) => EmptyTree
        case OverloadedType(_, _) => EmptyTree
        case PolyType(_, _) => EmptyTree
        case _ =>
          def wrapImplicit(from: Type): Tree = {
            val result = inferImplicit(tree, functionType(List(from), to), reportAmbiguous, true, context, saveErrors)
            if (result.subst != EmptyTreeTypeSubstituter) {
              result.subst traverse tree
              notifyUndetparamsInferred(result.subst.from, result.subst.to)
            }
            result.tree
          }
          val result = wrapImplicit(from)
          if (result != EmptyTree) result
          else wrapImplicit(byNameType(from))
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
      if (treeInfo.isExprSafeToInline(tree)) tree else UnstableTreeError(tree)

    /** Would tree be a stable (i.e. a pure expression) if the type
     *  of its symbol was not volatile?
     */
    protected def isStableExceptVolatile(tree: Tree) = {
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
    def isNonRefinementClassType(tpe: Type) = tpe match {
      case SingleType(_, sym) => sym.isModuleClass
      case TypeRef(_, sym, _) => sym.isClass && !sym.isRefinementClass
      case ErrorType          => true
      case _                  => false
    }
    private def errorNotClass(tpt: Tree, found: Type)  = { ClassTypeRequiredError(tpt, found); false }
    private def errorNotStable(tpt: Tree, found: Type) = { TypeNotAStablePrefixError(tpt, found); false }

    /** Check that `tpt` refers to a non-refinement class type */
    def checkClassType(tpt: Tree): Boolean = {
      val tpe = unwrapToClass(tpt.tpe)
      isNonRefinementClassType(tpe) || errorNotClass(tpt, tpe)
    }

    /** Check that `tpt` refers to a class type with a stable prefix. */
    def checkStablePrefixClassType(tpt: Tree): Boolean = {
      val tpe = unwrapToStableClass(tpt.tpe)
      def prefixIsStable = {
        def checkPre = tpe match {
          case TypeRef(pre, _, _) => pre.isStable || errorNotStable(tpt, pre)
          case _                  => false
        }
        // A type projection like X#Y can get by the stable check if the
        // prefix is singleton-bounded, so peek at the tree too.
        def checkTree = tpt match {
          case SelectFromTypeTree(qual, _)  => isSingleType(qual.tpe) || errorNotClass(tpt, tpe)
          case _                            => true
        }
        checkPre && checkTree
      }

      (    (isNonRefinementClassType(tpe) || errorNotClass(tpt, tpe))
        && (isPastTyper || prefixIsStable)
      )
    }

    /** Check that type <code>tp</code> is not a subtype of itself.
     *
     *  @param pos ...
     *  @param tp  ...
     *  @return    <code>true</code> if <code>tp</code> is not a subtype of itself.
     */
    def checkNonCyclic(pos: Position, tp: Type): Boolean = {
      def checkNotLocked(sym: Symbol) = {
        sym.initialize
        sym.lockOK || { CyclicAliasingOrSubtypingError(pos, sym); false }
      }
      tp match {
        case TypeRef(pre, sym, args) =>
          checkNotLocked(sym) &&
          ((!sym.isNonClassType) || checkNonCyclic(pos, appliedType(pre.memberInfo(sym), args), sym))
          // @M! info for a type ref to a type parameter now returns a polytype
          // @M was: checkNonCyclic(pos, pre.memberInfo(sym).subst(sym.typeParams, args), sym)

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
      if (!lockedSym.lock(CyclicReferenceError(pos, lockedSym))) false
      else checkNonCyclic(pos, tp)
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

    def checkParamsConvertible(tree: Tree, tpe0: Type) {
      def checkParamsConvertible0(tpe: Type) =
        tpe match {
          case MethodType(formals, restpe) =>
            /*
            if (formals.exists(_.typeSymbol == ByNameParamClass) && formals.length != 1)
              error(pos, "methods with `=>`-parameter can be converted to function values only if they take no other parameters")
            if (formals exists (isRepeatedParamType(_)))
              error(pos, "methods with `*`-parameters cannot be converted to function values");
            */
            if (restpe.isDependent)
              DependentMethodTpeConversionToFunctionError(tree, tpe)
            checkParamsConvertible(tree, restpe)
          case _ =>
        }
      checkParamsConvertible0(tpe0)
    }

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

      private def check[T <: Tree](owner: Symbol, scope: Scope, pt: Type, tree: T): T = {
        this.owner = owner
        this.scope = scope
        hiddenSymbols = List()
        val tp1 = apply(tree.tpe)
        if (hiddenSymbols.isEmpty) tree setType tp1
        else if (hiddenSymbols exists (_.isErroneous)) HiddenSymbolWithError(tree)
        else if (isFullyDefined(pt)) tree setType pt
        else if (tp1.typeSymbol.isAnonymousClass)
          check(owner, scope, pt, tree setType tp1.typeSymbol.classBound)
        else if (owner == NoSymbol)
          tree setType packSymbols(hiddenSymbols.reverse, tp1)
        else if (!phase.erasedTypes) { // privates
          val badSymbol = hiddenSymbols.head
          SymbolEscapesScopeError(tree, badSymbol)
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
     *  packageOk is equal false when qualifying class symbol
     */
    def qualifyingClass(tree: Tree, qual: Name, packageOK: Boolean) =
      context.enclClass.owner.ownerChain.find(o => qual.isEmpty || o.isClass && o.name == qual) match {
        case Some(c) if packageOK || !c.isPackageClass => c
        case _                                         => QualifyingClassError(tree, qual) ; NoSymbol
      }

    /** The typer for an expression, depending on where we are. If we are before a superclass
     *  call, this is a typer over a constructor context; otherwise it is the current typer.
     */
    @inline
    final def constrTyperIf(inConstr: Boolean): Typer =
      if (inConstr) {
        assert(context.undetparams.isEmpty, context.undetparams)
        newTyper(context.makeConstructorContext)
      } else this

    @inline
    final def withCondConstrTyper[T](inConstr: Boolean)(f: Typer => T): T =
      if (inConstr) {
        assert(context.undetparams.isEmpty, context.undetparams)
        val c = context.makeConstructorContext
        typerWithLocalContext(c)(f)
      } else {
        f(this)
      }

    @inline
    final def typerWithCondLocalContext[T](c: => Context)(cond: Boolean)(f: Typer => T): T =
      if (cond) typerWithLocalContext(c)(f) else f(this)

    @inline
    final def typerWithLocalContext[T](c: Context)(f: Typer => T): T = {
      val res = f(newTyper(c))
      if (c.hasErrors)
        context.updateBuffer(c.flushAndReturnBuffer())
      res
    }

    @inline
    final def typerReportAnyContextErrors[T](c: Context)(f: Typer => T): T = {
      val res = f(newTyper(c))
      if (c.hasErrors)
        context.issue(c.errBuffer.head)
      res
    }

    @inline
    final def withSavedContext[T](c: Context)(f: => T) = {
      val savedErrors = c.flushAndReturnBuffer()
      val res = f
      c.updateBuffer(savedErrors)
      res
    }

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
     *  4. Give getClass calls a more precise type based on the type of the target of the call.
     */
    private def stabilize(tree: Tree, pre: Type, mode: Int, pt: Type): Tree = {
      if (tree.symbol.isOverloaded && !inFunMode(mode))
        inferExprAlternative(tree, pt)

      val sym = tree.symbol
      def fail() = NotAValueError(tree, sym)

      if (tree.isErrorTyped) tree
      else if ((mode & (PATTERNmode | FUNmode)) == PATTERNmode && tree.isTerm) { // (1)
        if (sym.isValue) {
          val tree1 = checkStable(tree)
          // A module reference in a pattern has type Foo.type, not "object Foo"
          if (sym.isModule && !sym.isMethod) tree1 setType singleType(pre, sym)
          else tree1
        }
        else fail()
      } else if ((mode & (EXPRmode | QUALmode)) == EXPRmode && !sym.isValue && !phase.erasedTypes) { // (2)
        fail()
      } else {
        if (sym.isStable && pre.isStable && !isByNameParamType(tree.tpe) &&
            (isStableContext(tree, mode, pt) || sym.isModule && !sym.isMethod))
          tree.setType(singleType(pre, sym))
        // To fully benefit from special casing the return type of
        // getClass, we have to catch it immediately so expressions
        // like x.getClass().newInstance() are typed with the type of x.
        else if (  tree.symbol.name == nme.getClass_
                && tree.tpe.params.isEmpty
                // TODO: If the type of the qualifier is inaccessible, we can cause private types
                // to escape scope here, e.g. pos/t1107.  I'm not sure how to properly handle this
                // so for now it requires the type symbol be public.
                && pre.typeSymbol.isPublic)
          tree setType MethodType(Nil, getClassReturnType(pre))
        else
          tree
      }
    }

    private def isNarrowable(tpe: Type): Boolean = unwrapWrapperTypes(tpe) match {
      case TypeRef(_, _, _) | RefinedType(_, _) => true
      case _                                    => !phase.erasedTypes
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
        tree.setType(MethodType(List(), singleType(pre, sym))) // TODO: should this be a NullaryMethodType?
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
                  reportAmbiguousErrors: Boolean = context.ambiguousErrors,
                  newtree: Tree = context.tree): SilentResult[T] = {
      val rawTypeStart = startCounter(rawTypeFailed)
      val findMemberStart = startCounter(findMemberFailed)
      val subtypeStart = startCounter(subtypeFailed)
      val failedSilentStart = startTimer(failedSilentNanos)
      try {
        if (context.reportErrors ||
            reportAmbiguousErrors != context.ambiguousErrors ||
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
          if (context1.hasErrors) SilentTypeError(context1.errBuffer.head)
          else SilentResultValue(result)
        } else {
          assert(context.bufferErrors || isPastTyper, "silent mode is not available past typer")
          withSavedContext(context){
            val res = op(this)
            val errorsToReport = context.flushAndReturnBuffer()
            if (errorsToReport.isEmpty) SilentResultValue(res) else SilentTypeError(errorsToReport.head)
          }
        }
      } catch {
        case ex: CyclicReference => throw ex
        case ex: TypeError =>
          // fallback in case TypeError is still thrown
          // @H this happens for example in cps annotation checker
          stopCounter(rawTypeFailed, rawTypeStart)
          stopCounter(findMemberFailed, findMemberStart)
          stopCounter(subtypeFailed, subtypeStart)
          stopTimer(failedSilentNanos, failedSilentStart)
          SilentTypeError(TypeErrorWrapper(ex))
      }
    }

    /** Check whether feature given by `featureTrait` is enabled.
     *  If it is not, issue an error or a warning depending on whether the feature is required.
     *  @param  construct  A string expression that is substituted for "#" in the feature description string
     *  @param  immediate  When set, feature check is run immediately, otherwise it is run
     *                     at the end of the typechecking run for the enclosing unit. This
     *                     is done to avoid potential cyclic reference errors by implicits
     *                     that are forced too early.
     *  @return if feature check is run immediately: true if feature is enabled, false otherwise
     *          if feature check is delayed or suppressed because we are past typer: true
     */
    def checkFeature(pos: Position, featureTrait: Symbol, construct: => String = "", immediate: Boolean = false): Boolean =
      if (isPastTyper) true
      else {
        val nestedOwners =
          featureTrait.owner.ownerChain.takeWhile(_ != languageFeatureModule.moduleClass).reverse
        val featureName = (nestedOwners map (_.name + ".")).mkString + featureTrait.name
        def action(): Boolean = {
          def hasImport = inferImplicit(EmptyTree: Tree, featureTrait.tpe, true, false, context) != SearchFailure
          def hasOption = settings.language.value contains featureName
          val OK = hasImport || hasOption
          if (!OK) {
            val Some(AnnotationInfo(_, List(Literal(Constant(featureDesc: String)), Literal(Constant(required: Boolean))), _)) =
              featureTrait getAnnotation LanguageFeatureAnnot
            val req = if (required) "needs to" else "should"
            var raw = featureDesc + " " + req + " be enabled\n" +
              "by making the implicit value language." + featureName + " visible."
            if (!(currentRun.reportedFeature contains featureTrait))
              raw += "\nThis can be achieved by adding the import clause 'import language." + featureName + "'\n" +
                "or by setting the compiler option -language:" + featureName + ".\n" +
                "See the Scala docs for value scala.language." + featureName + " for a discussion\n" +
                "why the feature " + req + " be explicitly enabled."
            currentRun.reportedFeature += featureTrait
            val msg = raw replace ("#", construct)
            if (required) unit.error(pos, msg)
            else currentRun.featureWarnings.warn(pos, msg)
          }
          OK
        }
        if (immediate) {
          action()
        } else {
          unit.toCheck += action
          true
        }
      }

    def checkExistentialsFeature(pos: Position, tpe: Type, prefix: String) = tpe match {
      case extp: ExistentialType if !extp.isRepresentableWithWildcards =>
        checkFeature(pos, ExistentialsFeature, prefix+" "+tpe)
      case _ =>
    }

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
        if (context.undetparams.nonEmpty) { // (9) -- should revisit dropped condition `(mode & POLYmode) == 0`
          // dropped so that type args of implicit method are inferred even if polymorphic expressions are allowed
          // needed for implicits in 2.8 collection library -- maybe once #3346 is fixed, we can reinstate the condition?
            context.undetparams = inferExprInstance(tree, context.extractUndetparams(), pt,
              // approximate types that depend on arguments since dependency on implicit argument is like dependency on type parameter
              mt.approximate,
              keepNothings = false,
              useWeaklyCompatible = true) // #3808
        }

        // avoid throwing spurious DivergentImplicit errors
        if (context.hasErrors)
          return setError(tree)

        withCondConstrTyper(treeInfo.isSelfOrSuperConstrCall(tree)){ typer1 =>
          if (original != EmptyTree && pt != WildcardType)
            typer1.silent(tpr => {
              val withImplicitArgs = tpr.applyImplicitArgs(tree)
              if (tpr.context.hasErrors) tree // silent will wrap it in SilentTypeError anyway
              else tpr.typed(withImplicitArgs, mode, pt)
            }) match {
              case SilentResultValue(result) =>
                result
              case _ =>
                debuglog("fallback on implicits: " + tree + "/" + resetAllAttrs(original))
                val tree1 = typed(resetAllAttrs(original), mode, WildcardType)
                tree1.tpe = addAnnotations(tree1, tree1.tpe)
                if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt, EmptyTree)
            }
          else
            typer1.typed(typer1.applyImplicitArgs(tree), mode, pt)
        }
      }

      def instantiateToMethodType(mt: MethodType): Tree = {
        val meth = tree match {
          // a partial named application is a block (see comment in EtaExpansion)
          case Block(_, tree1) => tree1.symbol
          case _               => tree.symbol
        }
        if (!meth.isConstructor && !meth.isTermMacro && isFunctionType(pt)) { // (4.2)
          debuglog("eta-expanding " + tree + ":" + tree.tpe + " to " + pt)
          checkParamsConvertible(tree, tree.tpe)
          val tree0 = etaExpand(context.unit, tree)
          // println("eta "+tree+" ---> "+tree0+":"+tree0.tpe+" undet: "+context.undetparams+ " mode: "+Integer.toHexString(mode))

          if (context.undetparams.nonEmpty) {
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
          MissingArgsForMethodTpeError(tree, meth)
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
          MissingTypeParametersError(tree)
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
          KindArityMismatchError(tree, pt)
        } else tree match { // (6)
          case TypeTree() => tree
          case _          => TypeTree(tree.tpe) setOriginal tree
        }
      }

      /**
       * To deal with the type slack between actual (run-time) types and statically known types, for each abstract type T,
       * reflect its variance as a skolem that is upper-bounded by T (covariant position), or lower-bounded by T (contravariant).
       *
       * Consider the following example:
       *
       *  class AbsWrapperCov[+A]
       *  case class Wrapper[B](x: Wrapped[B]) extends AbsWrapperCov[B]
       *
       *  def unwrap[T](x: AbsWrapperCov[T]): Wrapped[T] = x match {
       *    case Wrapper(wrapped) => // Wrapper's type parameter must not be assumed to be equal to T, it's *upper-bounded* by it
       *      wrapped // : Wrapped[_ <: T]
       *  }
       *
       * this method should type check if and only if Wrapped is covariant in its type parameter
       *
       * when inferring Wrapper's type parameter B from x's type AbsWrapperCov[T],
       * we must take into account that x's actual type is AbsWrapperCov[Tactual] forSome {type Tactual <: T}
       * as AbsWrapperCov is covariant in A -- in other words, we must not assume we know T exactly, all we know is its upper bound
       *
       * since method application is the only way to generate this slack between run-time and compile-time types (TODO: right!?),
       * we can simply replace skolems that represent method type parameters as seen from the method's body
       * by other skolems that are (upper/lower)-bounded by that type-parameter skolem
       * (depending on the variance position of the skolem in the statically assumed type of the scrutinee, pt)
       *
       * see test/files/../t5189*.scala
       */
      def adaptConstrPattern(): Tree = { // (5)
        def isExtractor(sym: Symbol) = reallyExists(unapplyMember(sym.tpe))
        val extractor = tree.symbol filter isExtractor
        if (extractor != NoSymbol) {
          tree setSymbol extractor
          tree.tpe match {
            case OverloadedType(pre, alts) => tree.tpe = overloadedType(pre, alts filter isExtractor)
            case _ =>
          }
          val unapply = unapplyMember(extractor.tpe)
          val clazz = unapplyParameterType(unapply)

          if (unapply.isCase && clazz.isCase && !(clazz.ancestors exists (_.isCase))) {
            // convert synthetic unapply of case class to case class constructor
            val prefix = tree.tpe.prefix
            val tree1 = TypeTree(clazz.primaryConstructor.tpe.asSeenFrom(prefix, clazz.owner))
              .setOriginal(tree)

            val skolems = new mutable.ListBuffer[TypeSymbol]
            object variantToSkolem extends VariantTypeMap {
              def apply(tp: Type) = mapOver(tp) match {
                case TypeRef(NoPrefix, tpSym, Nil) if variance != 0 && tpSym.isTypeParameterOrSkolem && tpSym.owner.isTerm =>
                  val bounds = if (variance == 1) TypeBounds.upper(tpSym.tpe) else TypeBounds.lower(tpSym.tpe)
                  // origin must be the type param so we can deskolemize
                  val skolem = context.owner.newGADTSkolem(unit.freshTypeName("?"+tpSym.name), tpSym, bounds)
                  // println("mapping "+ tpSym +" to "+ skolem + " : "+ bounds +" -- pt= "+ pt +" in "+ context.owner +" at "+ context.tree )
                  skolems += skolem
                  skolem.tpe
                case tp1 => tp1
              }
            }

            // have to open up the existential and put the skolems in scope
            // can't simply package up pt in an ExistentialType, because that takes us back to square one (List[_ <: T] == List[T] due to covariance)
            val ptSafe   = variantToSkolem(pt) // TODO: pt.skolemizeExistential(context.owner, tree) ?
            val freeVars = skolems.toList

            // use "tree" for the context, not context.tree: don't make another CaseDef context,
            // as instantiateTypeVar's bounds would end up there
            val ctorContext = context.makeNewScope(tree, context.owner)
            freeVars foreach ctorContext.scope.enter
            newTyper(ctorContext).infer.inferConstructorInstance(tree1, clazz.typeParams, ptSafe)

            // simplify types without losing safety,
            // so that error messages don't unnecessarily refer to skolems
            val extrapolate = new ExistentialExtrapolation(freeVars) extrapolate (_: Type)
            val extrapolated = tree1.tpe match {
              case MethodType(ctorArgs, res) => // ctorArgs are actually in a covariant position, since this is the type of the subpatterns of the pattern represented by this Apply node
                ctorArgs foreach (p => p.info = extrapolate(p.info)) // no need to clone, this is OUR method type
                copyMethodType(tree1.tpe, ctorArgs, extrapolate(res))
              case tp => tp
            }

            // once the containing CaseDef has been type checked (see typedCase),
            // tree1's remaining type-slack skolems will be deskolemized (to the method type parameter skolems)
            tree1 setType extrapolated
          } else {
            tree
          }
        } else {
          CaseClassConstructorError(tree)
        }
      }

      def insertApply(): Tree = {
        assert(!inHKMode(mode), modeString(mode)) //@M
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
            TypeTree(tparam.tpeHK) setPos tree.pos.focus)) setPos tree.pos
          context.undetparams ++= tparams1
          notifyUndetparamsAdded(tparams1)
          adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt, original)
        case mt: MethodType if mt.isImplicit && ((mode & (EXPRmode | FUNmode | LHSmode)) == EXPRmode) => // (4.1)
          adaptToImplicitMethod(mt)

        case mt: MethodType if (((mode & (EXPRmode | FUNmode | LHSmode)) == EXPRmode) &&
          (context.undetparams.isEmpty || inPolyMode(mode))) && !(tree.symbol != null && tree.symbol.isTermMacro) =>
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
          else if (
              inExprModeButNot(mode, FUNmode) && !tree.isDef &&   // typechecking application
              tree.symbol != null && tree.symbol.isTermMacro)     // of a macro
            macroExpand(this, tree, mode, pt)
          else if ((mode & (PATTERNmode | FUNmode)) == (PATTERNmode | FUNmode))
            adaptConstrPattern()
          else if (inAllModes(mode, EXPRmode | FUNmode) &&
            !tree.tpe.isInstanceOf[MethodType] &&
            !tree.tpe.isInstanceOf[OverloadedType] &&
            applyPossible)
            insertApply()
          else if (!context.undetparams.isEmpty && !inPolyMode(mode)) { // (9)
            assert(!inHKMode(mode), modeString(mode)) //@M
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
                if (context.implicitsEnabled && !pt.isError && !tree.isErrorTyped) {
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
                    def msg = "inferred view from " + tree.tpe + " to " + pt + " = " + coercion + ":" + coercion.tpe
                    if (settings.logImplicitConv.value)
                      unit.echo(tree.pos, msg)

                    debuglog(msg)
                    val silentContext = context.makeImplicit(context.ambiguousErrors)
                    val res = newTyper(silentContext).typed(
                      new ApplyImplicitView(coercion, List(tree)) setPos tree.pos, mode, pt)
                    if (silentContext.hasErrors) context.issue(silentContext.errBuffer.head) else return res
                  }
                }
              }
              if (settings.debug.value) {
                log("error tree = " + tree)
                if (settings.explaintypes.value) explainTypes(tree.tpe, pt)
              }

              val found = tree.tpe
              val req = pt
              if (!found.isErroneous && !req.isErroneous) {
                if (!context.reportErrors && isPastTyper && req.skolemsExceptMethodTypeParams.nonEmpty) {
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
                  adapt(tree, mode, deriveTypeWithWildcards(pt.skolemsExceptMethodTypeParams)(pt))
                } else {
                  // create an actual error
                  AdaptTypeError(tree, found, req)
                }
              }
              setError(tree)
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
        case SilentResultValue(t) => t
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
        && !qual.isInstanceOf[Super]
        && ((qual.symbol eq null) || !qual.symbol.isTerm || qual.symbol.isValue)
        && !qtpe.isError
        && !qtpe.typeSymbol.isBottomClass
        && qtpe != WildcardType
        && !qual.isInstanceOf[ApplyImplicitView] // don't chain views
        && (context.implicitsEnabled || context.enrichmentEnabled)
        // Elaborating `context.implicitsEnabled`:
        // don't try to adapt a top-level type that's the subject of an implicit search
        // this happens because, if isView, typedImplicit tries to apply the "current" implicit value to
        // a value that needs to be coerced, so we check whether the implicit value has an `apply` method.
        // (If we allow this, we get divergence, e.g., starting at `conforms` during ant quick.bin)
        // Note: implicit arguments are still inferred (this kind of "chaining" is allowed)
      )
    }

    def adaptToMember(qual: Tree, searchTemplate: Type, reportAmbiguous: Boolean = true, saveErrors: Boolean = true): Tree = {
      if (isAdaptableWithView(qual)) {
        qual.tpe.widen.normalize match {
          case et: ExistentialType =>
            qual setType et.skolemizeExistential(context.owner, qual) // open the existential
          case _ =>
        }
        inferView(qual, qual.tpe, searchTemplate, reportAmbiguous, saveErrors) match {
          case EmptyTree  => qual
          case coercion   =>
            if (settings.logImplicitConv.value)
              unit.echo(qual.pos,
                "applied implicit conversion from %s to %s = %s".format(
                  qual.tpe, searchTemplate, coercion.symbol.defString))

            typedQualifier(atPos(qual.pos)(new ApplyImplicitView(coercion, List(qual))))
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
    def adaptToArguments(qual: Tree, name: Name, args: List[Tree], pt: Type, reportAmbiguous: Boolean, saveErrors: Boolean): Tree = {
      def doAdapt(restpe: Type) =
        //util.trace("adaptToArgs "+qual+", name = "+name+", argtpes = "+(args map (_.tpe))+", pt = "+pt+" = ")
        adaptToMember(qual, HasMethodMatching(name, args map (_.tpe), restpe), reportAmbiguous, saveErrors)
      if (pt != WildcardType) {
        silent(_ => doAdapt(pt)) match {
          case SilentResultValue(result) if result != qual =>
            result
          case _ =>
            debuglog("fallback on implicits in adaptToArguments: "+qual+" . "+name)
            doAdapt(WildcardType)
        }
      } else
        doAdapt(pt)
    }

    /** Try to apply an implicit conversion to `qual` so that it contains
     *  a method `name`. If that's ambiguous try taking arguments into
     *  account using `adaptToArguments`.
     */
    def adaptToMemberWithArgs(tree: Tree, qual: Tree, name: Name, mode: Int, reportAmbiguous: Boolean, saveErrors: Boolean): Tree = {
      def onError(reportError: => Tree): Tree = {
        context.tree match {
          case Apply(tree1, args) if (tree1 eq tree) && args.nonEmpty =>
            silent(_.typedArgs(args, mode)) match {
              case SilentResultValue(xs) =>
                val args = xs.asInstanceOf[List[Tree]]
                if (args exists (_.isErrorTyped))
                  reportError
                else
                  adaptToArguments(qual, name, args, WildcardType, reportAmbiguous, saveErrors)
              case _            =>
                reportError
            }
          case _ =>
            reportError
        }
      }
      silent(_.adaptToMember(qual, HasMember(name), false)) match {
          case SilentResultValue(res) => res
          case SilentTypeError(err) => onError({if (reportAmbiguous) { context.issue(err) }; setError(tree)})
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

    private def validateDerivedValueClass(clazz: Symbol, body: List[Tree]) = {
      if (clazz.isTrait)
        unit.error(clazz.pos, "only classes (not traits) are allowed to extend AnyVal")
      if (!clazz.isStatic)
        unit.error(clazz.pos, "value class may not be a "+
          (if (clazz.owner.isTerm) "local class" else "member of another class"))
      val constr = clazz.primaryConstructor
      clazz.info.decls.toList.filter(acc => acc.isMethod && (acc hasFlag PARAMACCESSOR)) match {
        case List(acc) =>
          def isUnderlyingAcc(sym: Symbol) =
            sym == acc || acc.hasAccessorFlag && sym == acc.accessed
          if (acc.accessBoundary(clazz) != RootClass)
            unit.error(acc.pos, "value class needs to have a publicly accessible val parameter")
          for (stat <- body)
            if (!treeInfo.isAllowedInUniversalTrait(stat) && !isUnderlyingAcc(stat.symbol))
              unit.error(stat.pos,
                if (stat.symbol hasFlag PARAMACCESSOR) "illegal parameter for value class"
                else "this statement is not allowed in value class: "+stat)
        case x =>
          unit.error(clazz.pos, "value class needs to have exactly one public val parameter")
      }
      for (tparam <- clazz.typeParams)
        if (tparam hasAnnotation definitions.SpecializedClass)
          unit.error(tparam.pos, "type parameter of value class may not be specialized")
    }

    def parentTypes(templ: Template): List[Tree] =
      if (templ.parents.isEmpty) List(atPos(templ.pos)(TypeTree(AnyRefClass.tpe)))
      else try {
        val clazz = context.owner
        // Normalize supertype and mixins so that supertype is always a class, not a trait.
        var supertpt = typedTypeConstructor(templ.parents.head)
        val firstParent = supertpt.tpe.typeSymbol
        var mixins = templ.parents.tail map typedType
        // If first parent is a trait, make it first mixin and add its superclass as first parent
        while ((supertpt.tpe.typeSymbol ne null) && supertpt.tpe.typeSymbol.initialize.isTrait) {
          val supertpt1 = typedType(supertpt)
          if (!supertpt1.isErrorTyped) {
            mixins = supertpt1 :: mixins
            supertpt = TypeTree(supertpt1.tpe.firstParent) setPos supertpt.pos.focus
          }
        }
        if (supertpt.tpe.typeSymbol == AnyClass && firstParent.isTrait)
          supertpt.tpe = AnyRefClass.tpe

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

            assert(clazz != NoSymbol, templ)
            val cscope = outercontext.makeNewScope(constr, outercontext.owner)
            val cbody2 = newTyper(cscope) // called both during completion AND typing.
                .typePrimaryConstrBody(clazz,
                  cbody1, supertparams, clazz.unsafeTypeParams, vparamss map (_.map(_.duplicate)))

            superCall match {
              case Apply(_, _) =>
                val sarg = treeInfo.firstArgument(superCall)
                if (sarg != EmptyTree && supertpe.typeSymbol != firstParent)
                  ConstrArgsInTraitParentTpeError(sarg, firstParent)
                if (!supertparams.isEmpty)
                  supertpt = TypeTree(cbody2.tpe) setPos supertpt.pos.focus
              case _ =>
                if (!supertparams.isEmpty)
                  MissingTypeArgumentsParentTpeError(supertpt)
            }

            val preSuperVals = treeInfo.preSuperFields(templ.body)
            if (preSuperVals.isEmpty && preSuperStats.nonEmpty)
              debugwarn("Wanted to zip empty presuper val list with " + preSuperStats)
            else
              map2(preSuperStats, preSuperVals)((ldef, gdef) => gdef.tpt.tpe = ldef.symbol.tpe)

          case _ =>
            if (!supertparams.isEmpty)
              MissingTypeArgumentsParentTpeError(supertpt)
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
          // fallback in case of cyclic errors
          // @H none of the tests enter here but I couldn't rule it out
          log("Type error calculating parents in template " + templ)
          log("Error: " + ex)
          ParentTypesError(templ, ex)
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
      val pending = ListBuffer[AbsTypeError]()
      def validateParentClass(parent: Tree, superclazz: Symbol) {
        if (!parent.isErrorTyped) {
          val psym = parent.tpe.typeSymbol.initialize
          checkStablePrefixClassType(parent)
          if (psym != superclazz) {
            if (psym.isTrait) {
              val ps = psym.info.parents
              if (!ps.isEmpty && !superclazz.isSubClass(ps.head.typeSymbol))
                pending += ParentSuperSubclassError(parent, superclazz, ps.head.typeSymbol, psym)
            } else {
              pending += ParentNotATraitMixinError(parent, psym)
            }
          }
          if (psym.isFinal)
            pending += ParentFinalInheritanceError(parent, psym)

          if (psym.isSealed && !phase.erasedTypes)
            if (context.unit.source.file == psym.sourceFile)
              psym addChild context.owner
            else
              pending += ParentSealedInheritanceError(parent, psym)

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
            pending += ParentSelfTypeConformanceError(parent, selfType)
            if (settings.explaintypes.value) explainTypes(selfType, parent.tpe.typeOfThis)
          }
          if (parents exists (p => p != parent && p.tpe.typeSymbol == psym && !psym.isError))
            pending += ParentInheritedTwiceError(parent, psym)
        }
      }
      if (!parents.isEmpty && parents.forall(!_.isErrorTyped))
        for (p <- parents) validateParentClass(p, parents.head.tpe.typeSymbol)

/*
      if (settings.Xshowcls.value != "" &&
          settings.Xshowcls.value == context.owner.fullName)
        println("INFO "+context.owner+
                ", baseclasses = "+(context.owner.info.baseClasses map (_.fullName))+
                ", lin = "+(context.owner.info.baseClasses map (context.owner.thisType.baseType)))
*/
      pending.foreach(ErrorUtils.issueTypeError)
    }

    def checkFinitary(classinfo: ClassInfoType) {
      val clazz = classinfo.typeSymbol

      for (tparam <- clazz.typeParams) {
        if (classinfo.expansiveRefs(tparam) contains tparam) {
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
          FinitaryError(tparam)
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
      val typedMods = typedModifiers(cdef.mods)
      assert(clazz != NoSymbol, cdef)
      reenterTypeParams(cdef.tparams)
      val tparams1 = cdef.tparams mapConserve (typedTypeDef)
      val impl1 = typerReportAnyContextErrors(context.make(cdef.impl, clazz, newScope)) {
        _.typedTemplate(cdef.impl, parentTypes(cdef.impl))
      }
      val impl2 = finishMethodSynthesis(impl1, clazz, context)
      if (clazz.isTrait && clazz.info.parents.nonEmpty && clazz.info.firstParent.normalize.typeSymbol == AnyClass)
        for (stat <- impl2.body)
          if (!treeInfo.isAllowedInUniversalTrait(stat))
            unit.error(stat.pos, "this statement is not allowed in universal trait extending from class Any: "+stat)
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
      val typedMods = typedModifiers(mdef.mods)
      assert(clazz != NoSymbol, mdef)
      val noSerializable = (
           (linkedClass eq NoSymbol)
        || linkedClass.isErroneous
        || !linkedClass.isSerializable
        || clazz.isSerializable
      )
      val impl1 = typerReportAnyContextErrors(context.make(mdef.impl, clazz, newScope)) {
        _.typedTemplate(mdef.impl, {
          parentTypes(mdef.impl) ++ (
            if (noSerializable) Nil
            else {
              clazz.makeSerializable()
              List(TypeTree(SerializableClass.tpe) setPos clazz.pos.focus)
            }
          )
        })
      }
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
        case vd @ ValDef(_, _, tpt, EmptyTree) =>
          val tpt1 = checkNoEscaping.privates(
            clazz.thisSym,
            treeCopy.TypeTree(tpt).setOriginal(tpt) setType vd.symbol.tpe
          )
          copyValDef(vd)(tpt = tpt1, rhs = EmptyTree) setType NoType
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
        else templ.body flatMap rewrappingWrapperTrees(namer.addDerivedTrees(Typer.this, _))

      val body1 = typedStats(body, templ.symbol)

      if (clazz.isDerivedValueClass)
        validateDerivedValueClass(clazz, body1)

      treeCopy.Template(templ, parents1, self1, body1) setType clazz.tpe
    }

    /** Remove definition annotations from modifiers (they have been saved
     *  into the symbol's ``annotations'' in the type completer / namer)
     *
     *  However reification does need annotation definitions to proceed.
     *  Unfortunately, AnnotationInfo doesn't provide enough info to reify it in general case.
     *  The biggest problem is with the "atp: Type" field, which cannot be reified in some situations
     *  that involve locally defined annotations. See more about that in Reifiers.scala.
     *
     *  That's why the original tree gets saved into ``original'' field of AnnotationInfo (happens elsewhere).
     *  The field doesn't get pickled/unpickled and exists only during a single compilation run.
     *  This simultaneously allows us to reify annotations and to preserve backward compatibility.
     */
    def typedModifiers(mods: Modifiers): Modifiers =
      mods.copy(annotations = Nil) setPositions mods.positions

    /**
     *  @param vdef ...
     *  @return     ...
     */
    def typedValDef(vdef: ValDef): ValDef = {
//      attributes(vdef)
      val sym = vdef.symbol.initialize
      val typer1 = constrTyperIf(sym.isParameter && sym.owner.isConstructor)
      val typedMods = typedModifiers(vdef.mods)

      // complete lazy annotations
      val annots = sym.annotations
      var tpt1 = checkNoEscaping.privates(sym, typer1.typedType(vdef.tpt))
      checkNonCyclic(vdef, tpt1)

      if (sym.hasAnnotation(definitions.VolatileAttr)) {
        if (!sym.isMutable)
          VolatileValueError(vdef)
        else if (sym.isFinal)
          FinalVolatileVarError(vdef)
      }
      val rhs1 =
        if (vdef.rhs.isEmpty) {
          if (sym.isVariable && sym.owner.isTerm && !isPastTyper)
            LocalVarUninitializedError(vdef)
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
      log("computing param aliases for "+clazz+":"+clazz.primaryConstructor.tpe+":"+rhs)//debug
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
      assert(superConstr.symbol ne null, superConstr)//debug

      val pending = ListBuffer[AbsTypeError]()
      // an object cannot be allowed to pass a reference to itself to a superconstructor
      // because of initialization issues; bug #473
      for (arg <- superArgs ; tree <- arg) {
        val sym = tree.symbol
        if (sym != null && (sym.info.baseClasses contains clazz)) {
          if (sym.isModule)
            pending +=  SuperConstrReferenceError(tree)
          tree match {
            case This(qual) =>
              pending += SuperConstrArgsThisReferenceError(tree)
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
      pending.foreach(ErrorUtils.issueTypeError)
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
                case SilentResultValue(tpt) =>
                  val alias = enclClass.newAliasType(name.toTypeName, useCase.pos)
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
          StarParamNotLastError(vparam1)

      var tpt1 = checkNoEscaping.privates(meth, typedType(ddef.tpt))
      checkNonCyclic(ddef, tpt1)
      ddef.tpt.setType(tpt1.tpe)
      val typedMods = typedModifiers(ddef.mods)
      var rhs1 =
        if (ddef.name == nme.CONSTRUCTOR && !ddef.symbol.hasStaticFlag) { // need this to make it possible to generate static ctors
          if (!meth.isPrimaryConstructor &&
              (!meth.owner.isClass ||
               meth.owner.isModuleClass ||
               meth.owner.isAnonOrRefinementClass))
            InvalidConstructorDefError(ddef)
          typed(ddef.rhs)
        } else if (meth.isTermMacro) {
          // typechecking macro bodies is sort of unconventional
          // that's why we employ our custom typing scheme orchestrated outside of the typer
          transformedOr(ddef.rhs, typedMacroBody(this, ddef))
        } else {
          transformedOrTyped(ddef.rhs, EXPRmode, tpt1.tpe)
        }

        if (meth.isPrimaryConstructor && meth.isClassConstructor && !isPastTyper && !reporter.hasErrors && !meth.owner.isSubClass(AnyValClass)) {
          // At this point in AnyVal there is no supercall, which will blow up
          // in computeParamAliases; there's nothing to be computed for Anyval anyway.
          computeParamAliases(meth.owner, vparamss1, rhs1)
        }

      if (tpt1.tpe.typeSymbol != NothingClass && !context.returnsSeen && rhs1.tpe.typeSymbol != NothingClass)
        rhs1 = checkDead(rhs1)

      if (!isPastTyper && meth.owner.isClass &&
          meth.paramss.exists(ps => ps.exists(_.hasDefault) && isRepeatedParamType(ps.last.tpe)))
        StarWithDefaultError(meth)

      if (!isPastTyper) {
        val allParams = meth.paramss.flatten
        for (p <- allParams) {
          for (n <- p.deprecatedParamName) {
            if (allParams.exists(p1 => p1.name == n || (p != p1 && p1.deprecatedParamName.exists(_ == n))))
              DeprecatedParamNameError(p, n)
          }
        }
      }
      if (meth.isStructuralRefinementMember)
        checkMethodStructuralCompatible(meth)

      if (meth.isImplicit && !meth.isSynthetic) meth.info.paramss match {
        case List(param) :: _ if !param.isImplicit =>
          checkFeature(ddef.pos, ImplicitConversionsFeature, meth.toString)
        case _ =>
      }

      treeCopy.DefDef(ddef, typedMods, ddef.name, tparams1, vparamss1, tpt1, rhs1) setType NoType
    }

    def typedTypeDef(tdef: TypeDef): TypeDef =
      typerWithCondLocalContext(context.makeNewScope(tdef, tdef.symbol))(tdef.tparams.nonEmpty){
        _.typedTypeDef0(tdef)
      }

    // call typedTypeDef instead
    // a TypeDef with type parameters must always be type checked in a new scope
    private def typedTypeDef0(tdef: TypeDef): TypeDef = {
      tdef.symbol.initialize
      reenterTypeParams(tdef.tparams)
      val tparams1 = tdef.tparams mapConserve typedTypeDef
      val typedMods = typedModifiers(tdef.mods)
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
          case TypeBounds(lo1, hi1) if (!(lo1 <:< hi1)) => LowerBoundError(tdef, lo1, hi1)
          case _                                        => ()
        }

      if (tdef.symbol.isDeferred && tdef.symbol.info.isHigherKinded)
        checkFeature(tdef.pos, HigherKindsFeature)

      treeCopy.TypeDef(tdef, typedMods, tdef.name, tparams1, rhs1) setType NoType
    }

    private def enterLabelDef(stat: Tree) {
      stat match {
        case ldef @ LabelDef(_, _, _) =>
          if (ldef.symbol == NoSymbol)
            ldef.symbol = namer.enterInScope(
              context.owner.newLabel(ldef.name, ldef.pos) setInfo MethodType(List(), UnitClass.tpe))
        case _ =>
      }
    }

    def typedLabelDef(ldef: LabelDef): LabelDef = {
      if (!nme.isLoopHeaderLabel(ldef.symbol.name) || isPastTyper) {
        val restpe = ldef.symbol.tpe.resultType
        val rhs1 = typed(ldef.rhs, restpe)
        ldef.params foreach (param => param.tpe = param.symbol.tpe)
        deriveLabelDef(ldef)(_ => rhs1) setType restpe
      }
      else {
        val initpe = ldef.symbol.tpe.resultType
        val rhs1 = typed(ldef.rhs)
        val restpe = rhs1.tpe
        if (restpe == initpe) { // stable result, no need to check again
          ldef.params foreach (param => param.tpe = param.symbol.tpe)
          treeCopy.LabelDef(ldef, ldef.name, ldef.params, rhs1) setType restpe
        } else {
          context.scope.unlink(ldef.symbol)
          val sym2 = namer.enterInScope(
            context.owner.newLabel(ldef.name, ldef.pos) setInfo MethodType(List(), restpe))
          val rhs2 = typed(resetAllAttrs(ldef.rhs), restpe)
          ldef.params foreach (param => param.tpe = param.symbol.tpe)
          deriveLabelDef(ldef)(_ => rhs2) setSymbol sym2 setType restpe
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
        StarPositionInPatternError(x)

      // withoutAnnotations - see continuations-run/z1673.scala
      // This adjustment is awfully specific to continuations, but AFAICS the
      // whole AnnotationChecker framework is.
      val pat1 = typedPattern(cdef.pat, pattpe.withoutAnnotations)
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

      val contextWithTypeBounds = context.nextEnclosing(_.tree.isInstanceOf[CaseDef])
      if (contextWithTypeBounds.savedTypeBounds.nonEmpty) {
        body1.tpe = contextWithTypeBounds restoreTypeBounds body1.tpe

        // insert a cast if something typechecked under the GADT constraints,
        // but not in real life (i.e., now that's we've reset the method's type skolems'
        //   infos back to their pre-GADT-constraint state)
        if (isFullyDefined(pt) && !(body1.tpe <:< pt))
          body1 = typedPos(body1.pos)(gen.mkCast(body1, pt))

      }

//    body1 = checkNoEscaping.locals(context.scope, pt, body1)
      val treeWithSkolems = treeCopy.CaseDef(cdef, pat1, guard1, body1) setType body1.tpe

      new TypeMapTreeSubstituter(deskolemizeGADTSkolems).traverse(treeWithSkolems)

      treeWithSkolems // now without skolems, actually
    }

    // undo adaptConstrPattern's evil deeds, as they confuse the old pattern matcher
    // the flags are used to avoid accidentally deskolemizing unrelated skolems of skolems
    object deskolemizeGADTSkolems extends TypeMap {
      def apply(tp: Type): Type = mapOver(tp) match {
        case TypeRef(pre, sym, args) if sym.isGADTSkolem =>
          typeRef(NoPrefix, sym.deSkolemize, args)
        case tp1 => tp1
      }
    }

    def typedCases(cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef] =
      cases mapConserve { cdef =>
        newTyper(context.makeNewScope(cdef, context.owner)).typedCase(cdef, pattp, pt)
      }

    def adaptCase(cdef: CaseDef, mode: Int, tpe: Type): CaseDef = deriveCaseDef(cdef)(adapt(_, mode, tpe))

    def ptOrLub(tps: List[Type], pt: Type  )       = if (isFullyDefined(pt)) (pt, false) else weakLub(tps map (_.deconst))
    def ptOrLubPacked(trees: List[Tree], pt: Type) = if (isFullyDefined(pt)) (pt, false) else weakLub(trees map (c => packedType(c, context.owner).deconst))

    // takes untyped sub-trees of a match and type checks them
    def typedMatch(selector: Tree, cases: List[CaseDef], mode: Int, pt: Type, tree: Tree = EmptyTree): Match = {
      val selector1  = checkDead(typed(selector, EXPRmode | BYVALmode, WildcardType))
      val selectorTp = packCaptured(selector1.tpe.widen).skolemizeExistential(context.owner, selector)
      val casesTyped = typedCases(cases, selectorTp, pt)

      val (resTp, needAdapt) =
        if (opt.virtPatmat) ptOrLubPacked(casesTyped, pt)
        else ptOrLub(casesTyped map (_.tpe), pt)

      val casesAdapted = if (!needAdapt) casesTyped else casesTyped map (adaptCase(_, mode, resTp))

      treeCopy.Match(tree, selector1, casesAdapted) setType resTp
    }

    // match has been typed -- virtualize it if we're feeling experimental
    // (virtualized matches are expanded during type checking so they have the full context available)
    // otherwise, do nothing: matches are translated during phase `patmat` (unless -Xoldpatmat)
    def virtualizedMatch(match_ : Match, mode: Int, pt: Type) = {
      import patmat.{vpmName, PureMatchTranslator, OptimizingMatchTranslator}

      // TODO: add fallback __match sentinel to predef
      val matchStrategy: Tree =
        if (!(newPatternMatching && opt.experimental && context.isNameInScope(vpmName._match))) null    // fast path, avoiding the next line if there's no __match to be seen
        else newTyper(context.makeImplicit(reportAmbiguousErrors = false)).silent(_.typed(Ident(vpmName._match), EXPRmode, WildcardType), reportAmbiguousErrors = false) match {
          case SilentResultValue(ms) => ms
          case _                     => null
        }

      if (matchStrategy ne null) // virtualize
        typed((new PureMatchTranslator(this.asInstanceOf[patmat.global.analyzer.Typer] /*TODO*/, matchStrategy)).translateMatch(match_), mode, pt)
      else
        match_ // will be translated in phase `patmat`
    }

    // synthesize and type check a PartialFunction implementation based on a match specified by `cases`
    // Match(EmptyTree, cases) ==> new PartialFunction { def apply<OrElse>(params) = `translateMatch('`(param1,...,paramN)` match { cases }')` }
    // for fresh params, the selector of the match we'll translated simply gathers those in a tuple
    // NOTE: restricted to PartialFunction -- leave Function trees if the expected type does not demand a partial function
    class MatchFunTyper(tree: Tree, cases: List[CaseDef], mode: Int, pt0: Type)  {
      // TODO: remove FunctionN support -- this is currently designed so that it can emit FunctionN and PartialFunction subclasses
      // however, we should leave Function nodes until Uncurry so phases after typer can still detect normal Function trees
      // we need to synthesize PartialFunction impls, though, to avoid nastiness in Uncurry in transforming&duplicating generated pattern matcher trees
      // TODO: remove PartialFunction support from UnCurry
      private val pt    = deskolemizeGADTSkolems(pt0)
      private val targs = pt.normalize.typeArgs
      private val arity = if (isFunctionType(pt)) targs.length - 1 else 1 // TODO pt should always be a (Partial)Function, right?
      private val ptRes = if (targs.isEmpty) WildcardType else targs.last // may not be fully defined

      private val isPartial = pt.typeSymbol == PartialFunctionClass
      assert(isPartial)

      private val anonClass = context.owner.newAnonymousFunctionClass(tree.pos)
      private val funThis   = This(anonClass)

      anonClass addAnnotation AnnotationInfo(SerialVersionUIDAttr.tpe, List(Literal(Constant(0))), List())

      def deriveFormals =
        if (targs.isEmpty) Nil
        else targs.init

      def mkParams(methodSym: Symbol, formals: List[Type] = deriveFormals) =
        if (formals.isEmpty) { MissingParameterTypeAnonMatchError(tree, pt); Nil }
        else methodSym newSyntheticValueParams formals

      def mkSel(params: List[Symbol]) =
        if (params.isEmpty) EmptyTree
        else {
          val ids = params map (p => Ident(p.name))
          atPos(tree.pos.focusStart) { if (arity == 1) ids.head else gen.mkTuple(ids) }
        }

      import CODE._

      // need to duplicate the cases before typing them to generate the apply method, or the symbols will be all messed up
      val casesTrue = if (isPartial) cases map (c => deriveCaseDef(c)(x => atPos(x.pos.focus)(TRUE_typed)).duplicate) else Nil
      // println("casesTrue "+ casesTrue)
      def parentsPartial(targs: List[Type]) = addSerializable(appliedType(AbstractPartialFunctionClass.typeConstructor, targs))

      def applyMethod = {
        // rig the show so we can get started typing the method body -- later we'll correct the infos...
        anonClass setInfo ClassInfoType(addSerializable(ObjectClass.tpe, pt), newScope, anonClass)
        val methodSym = anonClass.newMethod(nme.apply, tree.pos, if(isPartial) (FINAL | OVERRIDE) else FINAL)
        val paramSyms = mkParams(methodSym)
        val selector  = mkSel(paramSyms)

        if (selector eq EmptyTree) EmptyTree
        else {
          methodSym setInfoAndEnter MethodType(paramSyms, AnyClass.tpe)

          val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym)) // should use the DefDef for the context's tree, but it doesn't exist yet (we need the typer we're creating to create it)
          paramSyms foreach (methodBodyTyper.context.scope enter _)

          val match_ = methodBodyTyper.typedMatch(gen.mkUnchecked(selector), cases, mode, ptRes)
          val resTp = match_.tpe

          val methFormals = paramSyms map (_.tpe)
          val parents = (
            if (isPartial) parentsPartial(List(methFormals.head, resTp))
            else addSerializable(abstractFunctionType(methFormals, resTp))
          )
          anonClass setInfo ClassInfoType(parents, newScope, anonClass)
          methodSym setInfoAndEnter MethodType(paramSyms, resTp)

          DefDef(methodSym, methodBodyTyper.virtualizedMatch(match_, mode, resTp))
        }
      }

      // def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
      def applyOrElseMethodDef = {
        // rig the show so we can get started typing the method body -- later we'll correct the infos...
        // targs were type arguments for PartialFunction, so we know they will work for AbstractPartialFunction as well
        anonClass setInfo ClassInfoType(parentsPartial(targs), newScope, anonClass)
        val methodSym = anonClass.newMethod(nme.applyOrElse, tree.pos, FINAL | OVERRIDE)

        // create the parameter that corresponds to the function's parameter
        val List(argTp)       = deriveFormals
        val A1                = methodSym newTypeParameter(newTypeName("A1")) setInfo TypeBounds.upper(argTp)
        val paramSyms@List(x) = mkParams(methodSym, List(A1.tpe))
        val selector          = mkSel(paramSyms)

        if (selector eq EmptyTree) EmptyTree
        else {
          // applyOrElse's default parameter:
          val B1        = methodSym newTypeParameter(newTypeName("B1")) setInfo TypeBounds.empty //lower(resTp)
          val default   = methodSym newValueParameter(newTermName("default"), tree.pos.focus, SYNTHETIC) setInfo functionType(List(A1.tpe), B1.tpe)

          val paramSyms = List(x, default)
          methodSym setInfoAndEnter polyType(List(A1, B1), MethodType(paramSyms, B1.tpe))

          val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym)) // should use the DefDef for the context's tree, but it doesn't exist yet (we need the typer we're creating to create it)
          paramSyms foreach (methodBodyTyper.context.scope enter _)

          val match_ = methodBodyTyper.typedMatch(gen.mkUnchecked(selector), cases, mode, ptRes)
          val resTp = match_.tpe

          anonClass setInfo ClassInfoType(parentsPartial(List(argTp, resTp)), newScope, anonClass)
          B1 setInfo TypeBounds.lower(resTp)
          anonClass.info.decls enter methodSym // methodSym's info need not change (B1's bound has been updated instead)

          match_ setType B1.tpe

          // the default uses applyOrElse's first parameter since the scrut's type has been widened
          val body = methodBodyTyper.virtualizedMatch(match_ withAttachment DefaultOverrideMatchAttachment(REF(default) APPLY (REF(x))), mode, B1.tpe)

          DefDef(methodSym, body)
        }
      }

      def isDefinedAtMethod = {
        val methodSym = anonClass.newMethod(nme.isDefinedAt, tree.pos.makeTransparent, FINAL)
        val paramSyms = mkParams(methodSym)
        val selector  = mkSel(paramSyms)

        if (selector eq EmptyTree) EmptyTree
        else {
          val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym)) // should use the DefDef for the context's tree, but it doesn't exist yet (we need the typer we're creating to create it)
          paramSyms foreach (methodBodyTyper.context.scope enter _)
          methodSym setInfoAndEnter MethodType(paramSyms, BooleanClass.tpe)

          val match_ = methodBodyTyper.typedMatch(gen.mkUnchecked(selector), casesTrue, mode, BooleanClass.tpe)
          val body   = methodBodyTyper.virtualizedMatch(match_ withAttachment DefaultOverrideMatchAttachment(FALSE_typed), mode, BooleanClass.tpe)

          DefDef(methodSym, body)
        }
      }

      lazy val members = if (isPartial) {
        // somehow @cps annotations upset the typer when looking at applyOrElse's signature, but not apply's
        // TODO: figure out the details (T @cps[U] is not a subtype of Any, but then why does it work for the apply method?)
        if (targs forall (_ <:< AnyClass.tpe)) List(applyOrElseMethodDef, isDefinedAtMethod)
        else List(applyMethod, isDefinedAtMethod)
      } else List(applyMethod)

      def translated =
        if (members.head eq EmptyTree) setError(tree)
        else typed(atPos(tree.pos)(Block(List(ClassDef(anonClass, NoMods, List(List()), List(List()), members, tree.pos.focus)), atPos(tree.pos.focus)(New(anonClass.tpe)))), mode, pt)
    }

    // Function(params, Match(sel, cases)) ==> new <Partial>Function { def apply<OrElse>(params) = `translateMatch('sel match { cases }')` }
    class MatchFunTyperBetaReduced(fun: Function, sel: Tree, cases: List[CaseDef], mode: Int, pt: Type) extends MatchFunTyper(fun, cases, mode, pt)  {
      override def deriveFormals =
        fun.vparams map { p => if(p.tpt.tpe == null) typedType(p.tpt).tpe else p.tpt.tpe }

      // the only difference from the super class is that we must preserve the names of the parameters
      override def mkParams(methodSym: Symbol, formals: List[Type] = deriveFormals) =
        (fun.vparams, formals).zipped map { (p, tp) =>
          methodSym.newValueParameter(p.name, p.pos.focus, SYNTHETIC) setInfo tp
        }

      override def mkSel(params: List[Symbol]) = sel.duplicate
    }

    /**
     *  @param fun  ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def typedFunction(fun: Function, mode: Int, pt: Type): Tree = {
      val numVparams = fun.vparams.length
      if (numVparams > definitions.MaxFunctionArity)
        return MaxFunctionArityError(fun)

      def decompose(pt: Type): (Symbol, List[Type], Type) =
        if ((isFunctionType(pt) || (pt.typeSymbol == PartialFunctionClass && numVparams == 1 && fun.body.isInstanceOf[Match])) && // see bug901 for a reason why next conditions are needed
            (  pt.normalize.typeArgs.length - 1 == numVparams
            || fun.vparams.exists(_.tpt.isEmpty)
            ))
          (pt.typeSymbol, pt.normalize.typeArgs.init, pt.normalize.typeArgs.last)
        else
          (FunctionClass(numVparams), fun.vparams map (x => NoType), WildcardType)

      val (clazz, argpts, respt) = decompose(pt)
      if (argpts.lengthCompare(numVparams) != 0)
        WrongNumberOfParametersError(fun, argpts)
      else {
        foreach2(fun.vparams, argpts) { (vparam, argpt) =>
          if (vparam.tpt.isEmpty) {
            vparam.tpt.tpe =
              if (isFullyDefined(argpt)) argpt
              else {
                fun match {
                  case etaExpansion(vparams, fn, args) =>
                    silent(_.typed(fn, forFunMode(mode), pt)) match {
                      case SilentResultValue(fn1) if context.undetparams.isEmpty =>
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
                MissingParameterTypeError(fun, vparam, pt)
                ErrorType
              }
            if (!vparam.tpt.pos.isDefined) vparam.tpt setPos vparam.pos.focus
          }
        }

        fun.body match {
          // later phase indicates scaladoc is calling (where shit is messed up, I tell you)
          //  -- so fall back to old patmat, which is more forgiving
          case Match(sel, cases) if (sel ne EmptyTree) && newPatternMatching && (pt.typeSymbol == PartialFunctionClass) =>
            // go to outer context -- must discard the context that was created for the Function since we're discarding the function
            // thus, its symbol, which serves as the current context.owner, is not the right owner
            // you won't know you're using the wrong owner until lambda lift crashes (unless you know better than to use the wrong owner)
            val outerTyper = newTyper(context.outer)
            (new outerTyper.MatchFunTyperBetaReduced(fun, sel, cases, mode, pt)).translated
          case _ =>
            val vparamSyms = fun.vparams map { vparam =>
              enterSym(context, vparam)
              if (context.retyping) context.scope enter vparam.symbol
              vparam.symbol
            }
            val vparams = fun.vparams mapConserve (typedValDef)
    //        for (vparam <- vparams) {
    //          checkNoEscaping.locals(context.scope, WildcardType, vparam.tpt); ()
    //        }
            val formals = vparamSyms map (_.tpe)
            val body1 = typed(fun.body, respt)
            val restpe = packedType(body1, fun.symbol).deconst.resultType
            val funtpe = typeRef(clazz.tpe.prefix, clazz, formals :+ restpe)
    //        body = checkNoEscaping.locals(context.scope, restpe, body)
            treeCopy.Function(fun, vparams, body1).setType(funtpe)
        }
      }
    }

    def typedRefinement(stats: List[Tree]) {
      namer.enterSyms(stats)
      // need to delay rest of typedRefinement to avoid cyclic reference errors
      unit.toCheck += { () =>
        val stats1 = typedStats(stats, NoSymbol)
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
      case _                  => log("unhandled import: "+imp+" in "+unit); imp
    }
    private def isWarnablePureExpression(tree: Tree) = tree match {
      case EmptyTree | Literal(Constant(())) => false
      case _                                 =>
        !tree.isErrorTyped && (treeInfo isExprSafeToInline tree) && {
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
      val statsErrors = scala.collection.mutable.LinkedHashSet[AbsTypeError]()
      def typedStat(stat: Tree): Tree = {
        if (context.owner.isRefinementClass && !treeInfo.isDeclarationOrTypeDef(stat))
          OnlyDeclarationsError(stat)
        else
          stat match {
            case imp @ Import(_, _) =>
              imp.symbol.initialize
              if (!imp.symbol.isError) {
                context = context.makeNewImport(imp)
                typedImport(imp)
              } else EmptyTree
            case _ =>
              if (localTarget && !includesTargetPos(stat)) {
                // skip typechecking of statements in a sequence where some other statement includes
                // the targetposition
                stat
              } else {
                val localTyper = if (inBlock || (stat.isDef && !stat.isInstanceOf[LabelDef])) {
                  context.flushBuffer()
                  this
                } else newTyper(context.make(stat, exprOwner))
                // XXX this creates a spurious dead code warning if an exception is thrown
                // in a constructor, even if it is the only thing in the constructor.
                val result = checkDead(localTyper.typed(stat, EXPRmode | BYVALmode, WildcardType))

                if (treeInfo.isSelfOrSuperConstrCall(result)) {
                  context.inConstructorSuffix = true
                  if (treeInfo.isSelfConstrCall(result) && result.symbol.pos.pointOrElse(0) >= exprOwner.enclMethod.pos.pointOrElse(0))
                    ConstructorsOrderError(stat)
                }

                if (isWarnablePureExpression(result)) context.warning(stat.pos,
                  "a pure expression does nothing in statement position; " +
                  "you may be omitting necessary parentheses"
                )
                statsErrors ++= localTyper.context.errBuffer
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

      def checkNoDoubleDefs(stats: List[Tree]): Unit = {
        val scope = if (inBlock) context.scope else context.owner.info.decls
        var e = scope.elems
        while ((e ne null) && e.owner == scope) {
          var e1 = scope.lookupNextEntry(e)
          while ((e1 ne null) && e1.owner == scope) {
            if (!accesses(e.sym, e1.sym) && !accesses(e1.sym, e.sym) &&
                (e.sym.isType || inBlock || (e.sym.tpe matches e1.sym.tpe)))
              // default getters are defined twice when multiple overloads have defaults. an
              // error for this is issued in RefChecks.checkDefaultsInOverloaded
              if (!e.sym.isErroneous && !e1.sym.isErroneous && !e.sym.hasDefaultFlag &&
                  !e.sym.hasAnnotation(BridgeClass) && !e1.sym.hasAnnotation(BridgeClass)) {
                log("Double definition detected:\n  " +
                    ((e.sym.getClass, e.sym.info, e.sym.ownerChain)) + "\n  " +
                    ((e1.sym.getClass, e1.sym.info, e1.sym.ownerChain)))

                DefDefinedTwiceError(e.sym, e1.sym)
                scope.unlink(e1) // need to unlink to avoid later problems with lub; see #2779
              }
              e1 = scope.lookupNextEntry(e1)
          }
          e = e.next
        }
      }

      def addSynthetics(stats: List[Tree]): List[Tree] = {
        val scope = if (inBlock) context.scope else context.owner.info.decls
        var newStats = new ListBuffer[Tree]
        var moreToAdd = true
        while (moreToAdd) {
          val initElems = scope.elems
          for (sym <- scope)
            for (tree <- context.unit.synthetics get sym) {
              newStats += typedStat(tree) // might add even more synthetics to the scope
              context.unit.synthetics -= sym
            }
          // the type completer of a synthetic might add more synthetics. example: if the
          // factory method of a case class (i.e. the constructor) has a default.
          moreToAdd = scope.elems ne initElems
        }
        if (newStats.isEmpty) stats
        else {
          // put default getters next to the method they belong to,
          // same for companion objects. fixes #2489 and #4036.
          // [Martin] This is pretty ugly. I think we could avoid
          // this code by associating defaults and companion objects
          // with the original tree instead of the new symbol.
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

      val stats1 = withSavedContext(context) {
        val result = stats mapConserve typedStat
        context.flushBuffer()
        result
      }
      context.updateBuffer(statsErrors)
      if (phase.erasedTypes) stats1
      else {
        checkNoDoubleDefs(stats1)
        addSynthetics(stats1)
      }
    }

    def typedArg(arg: Tree, mode: Int, newmode: Int, pt: Type): Tree = {
      val typedMode = onlyStickyModes(mode) | newmode
      val t = withCondConstrTyper((mode & SCCmode) != 0)(_.typed(arg, typedMode, pt))
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
          var tree = typedArg(args.head, mode, typedMode, adapted.head)
          if (hasPendingMacroExpansions) tree = macroExpandAll(this, tree)
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

      exists2(formals, args) {
        case (formal, Function(vparams, _)) =>
          (vparams exists (_.tpt.isEmpty)) &&
          vparams.length <= MaxFunctionArity &&
          (formal baseType FunctionClass(vparams.length) match {
            case TypeRef(_, _, formalargs) =>
              ( exists2(formalargs, vparams)((formal, vparam) =>
                        vparam.tpt.isEmpty && (tparams exists formal.contains))
                && (tparams forall isLowerBounded)
              )
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
      def duplErrTree = setError(treeCopy.Apply(tree, fun0, args))
      def duplErrorTree(err: AbsTypeError) = { issue(err); duplErrTree }

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
          if (context.hasErrors)
            setError(tree)
          else {
            inferMethodAlternative(fun, undetparams, argtpes.toList, pt, varArgsOnly = treeInfo.isWildcardStarArgList(args))
            doTypedApply(tree, adapt(fun, forFunMode(mode), WildcardType), args1, mode, pt)
          }

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
                case SilentResultValue(t) =>
                  // Depending on user options, may warn or error here if
                  // a Unit or tuple was inserted.
                  Some(t) filter (tupledTree =>
                       !inExprModeButNot(mode, FUNmode)
                    || tupledTree.symbol == null
                    || checkValidAdaptation(tupledTree, args)
                  )
                case _ =>
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

            def checkNotMacro() = {
              if (fun.symbol != null && fun.symbol.filter(sym => sym != null && sym.isTermMacro && !sym.isErroneous) != NoSymbol)
                duplErrorTree(NamedAndDefaultArgumentsNotSupportedForMacros(tree, fun))
            }

            if (mt.isErroneous) duplErrTree
            else if (inPatternMode(mode)) {
              // #2064
              duplErrorTree(WrongNumberOfArgsError(tree, fun))
            } else if (lencmp > 0) {
              tryTupleApply getOrElse duplErrorTree(TooManyArgsNamesDefaultsError(tree, fun))
            } else if (lencmp == 0) {
              // we don't need defaults. names were used, so this application is transformed
              // into a block (@see transformNamedApplication in NamesDefaults)
              val (namelessArgs, argPos) = removeNames(Typer.this)(args, params)
              if (namelessArgs exists (_.isErroneous)) {
                duplErrTree
              } else if (!isIdentity(argPos) && !sameLength(formals, params))
                // !isIdentity indicates that named arguments are used to re-order arguments
                duplErrorTree(MultipleVarargError(tree))
              else if (isIdentity(argPos) && !isNamedApplyBlock(fun)) {
                // if there's no re-ordering, and fun is not transformed, no need to transform
                // more than an optimization, e.g. important in "synchronized { x = update-x }"
                checkNotMacro()
                doTypedApply(tree, fun, namelessArgs, mode, pt)
              } else {
                checkNotMacro()
                transformNamedApplication(Typer.this, mode, pt)(
                                          treeCopy.Apply(tree, fun, namelessArgs), argPos)
              }
            } else {
              // defaults are needed. they are added to the argument list in named style as
              // calls to the default getters. Example:
              //  foo[Int](a)()  ==>  foo[Int](a)(b = foo$qual.foo$default$2[Int](a))
              checkNotMacro()
              val fun1 = transformNamedApplication(Typer.this, mode, pt)(fun, x => x)
              if (fun1.isErroneous) duplErrTree
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
                  duplErrorTree(ModuleUsingCompanionClassDefaultArgsErrror(tree))
                } else if (lencmp2 > 0) {
                  removeNames(Typer.this)(allArgs, params) // #3818
                  duplErrTree
                } else if (lencmp2 == 0) {
                  // useful when a default doesn't match parameter type, e.g. def f[T](x:T="a"); f[Int]()
                  val note = "Error occurred in an application involving default arguments."
                  if (!(context.diagnostic contains note)) context.diagnostic = note :: context.diagnostic
                  doTypedApply(tree, if (blockIsEmpty) fun else fun1, allArgs, mode, pt)
                } else {
                  tryTupleApply getOrElse duplErrorTree(NotEnoughArgsError(tree, fun, missing))
                }
              }
            }
          }

          if (!sameLength(formals, args) ||   // wrong nb of arguments
              (args exists isNamed) ||        // uses a named argument
              isNamedApplyBlock(fun)) {       // fun was transformed to a named apply block =>
                                              // integrate this application into the block
            if (dyna.isApplyDynamicNamed(fun)) dyna.typedNamedApply(tree, fun, args, mode, pt)
            else tryNamesDefaults
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
               *  ... this also causes bootstrapping cycles if List_apply is
               *  forced during kind-arity checking, so it is guarded by additional
               *  tests to ensure we're sufficiently far along.
               */
              if (args.isEmpty && !forInteractive && fun.symbol.isInitialized && ListModule.hasCompleteInfo && (fun.symbol == List_apply))
                atPos(tree.pos)(gen.mkNil setType restpe)
              else
                constfold(treeCopy.Apply(tree, fun, args1) setType ifPatternSkipFormals(restpe))
            } else if (needsInstantiation(tparams, formals, args)) {
              //println("needs inst "+fun+" "+tparams+"/"+(tparams map (_.info)))
              inferExprInstance(fun, tparams)
              doTypedApply(tree, fun, args, mode, pt)
            } else {
              assert(!inPatternMode(mode), modeString(mode)) // this case cannot arise for patterns
              val lenientTargs = protoTypeArgs(tparams, formals, mt.resultApprox, pt)
              val strictTargs = map2(lenientTargs, tparams)((targ, tparam) =>
                if (targ == WildcardType) tparam.tpeHK else targ)
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
                  arg1
                } else arg1
              }
              val args1 = map2(args, formals)(typedArgToPoly)
              if (args1 exists {_.isErrorTyped}) duplErrTree
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
          if (!tree.isErrorTyped) setError(tree) else tree
          // @H change to setError(treeCopy.Apply(tree, fun, args))
        /* --- begin unapply  --- */

        case otpe if inPatternMode(mode) && unapplyMember(otpe).exists =>
          if (args.length > MaxTupleArity)
            return duplErrorTree(TooManyArgsPatternError(fun))

          //
          def freshArgType(tp: Type): (List[Symbol], Type) = tp match {
            case MethodType(param :: _, _) =>
              (Nil, param.tpe)
            case PolyType(tparams, restpe) =>
              createFromClonedSymbols(tparams, freshArgType(restpe)._2)((ps, t) => ((ps, t)))
            // No longer used, see test case neg/t960.scala (#960 has nothing to do with it)
            case OverloadedType(_, _) =>
              OverloadedUnapplyError(fun)
              (Nil, ErrorType)
            case _ =>
              UnapplyWithSingleArgError(fun)
              (Nil, ErrorType)
          }

          val unapp     = unapplyMember(otpe)
          val unappType = otpe.memberType(unapp)
          val argDummy  = context.owner.newValue(nme.SELECTOR_DUMMY, fun.pos, SYNTHETIC) setInfo pt
          val arg       = Ident(argDummy) setType pt

          if (!isApplicableSafe(Nil, unappType, List(pt), WildcardType)) {
            //Console.println("UNAPP: need to typetest, arg.tpe = "+arg.tpe+", unappType = "+unappType)
            val (freeVars, unappFormal) = freshArgType(unappType.skolemizeExistential(context.owner, tree))
            val unapplyContext = context.makeNewScope(context.tree, context.owner)
            freeVars foreach unapplyContext.scope.enter

            val typer1 = newTyper(unapplyContext)
            val pattp  = typer1.infer.inferTypedPattern(tree, unappFormal, arg.tpe)

            // turn any unresolved type variables in freevars into existential skolems
            val skolems = freeVars map (fv => unapplyContext.owner.newExistentialSkolem(fv, fv))
            arg.tpe = pattp.substSym(freeVars, skolems)
            argDummy setInfo arg.tpe
          }

          // setType null is necessary so that ref will be stabilized; see bug 881
          val fun1 = typedPos(fun.pos)(Apply(Select(fun setType null, unapp), List(arg)))

          if (fun1.tpe.isErroneous) {
            duplErrTree
          } else {
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
            } else
              duplErrorTree(WrongNumberArgsPatternError(tree, fun))
          }

/* --- end unapply  --- */
        case _ =>
          duplErrorTree(ApplyWithoutArgsError(tree, fun))
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
      val pending = ListBuffer[AbsTypeError]()

      def reportAnnotationError(err: AbsTypeError) = {
        pending += err
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

        if (const == null) {
          reportAnnotationError(AnnotationNotAConstantError(tr)); None
        } else if (const.value == null) {
          reportAnnotationError(AnnotationArgNullError(tr)); None
        } else
          Some(LiteralAnnotArg(const))
      }

      /** Converts an untyped tree to a ClassfileAnnotArg. If the conversion fails,
       *  an error message is reported and None is returned.
       */
      def tree2ConstArg(tree: Tree, pt: Type): Option[ClassfileAnnotArg] = tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) if (pt.typeSymbol == ArrayClass) =>
          reportAnnotationError(ArrayConstantsError(tree)); None

        case ann @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val annInfo = typedAnnotation(ann, mode, NoSymbol, pt.typeSymbol, true)
          if (annInfo.atp.isErroneous) { hasError = true; None }
          else Some(NestedAnnotArg(annInfo))

        // use of Array.apply[T: ArrayTag](xs: T*): Array[T]
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
                reportAnnotationError(ArrayConstantsTypeMismatchError(tree, pt))
                None
            }
          else tryConst(tree, pt)

        case Typed(t, _) =>
          tree2ConstArg(t, pt)

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
              reportAnnotationError(UnexpectedTreeAnnotation(fun))
              (setError(fun), outerArgss)
          }
        extract(ann, List())
      }

      val res = if (fun.isErroneous) annotationError
      else {
        val typedFun @ Select(New(tpt), _) = typed(fun, forFunMode(mode), WildcardType)
        val annType = tpt.tpe

        if (typedFun.isErroneous) annotationError
        else if (annType.typeSymbol isNonBottomSubClass ClassfileAnnotationClass) {
          // annotation to be saved as java classfile annotation
          val isJava = typedFun.symbol.owner.isJavaDefined
          if (!annType.typeSymbol.isNonBottomSubClass(annClass)) {
            reportAnnotationError(AnnotationTypeMismatchError(tpt, annClass.tpe, annType))
          } else if (argss.length > 1) {
            reportAnnotationError(MultipleArgumentListForAnnotationError(ann))
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
                  reportAnnotationError(UnknownAnnotationNameError(arg, name))
                  (nme.ERROR, None)
                } else if (!names.contains(sym)) {
                  reportAnnotationError(DuplicateValueAnnotationError(arg, name))
                  (nme.ERROR, None)
                } else {
                  names -= sym
                  if (isJava) sym.cookJavaRawInfo() // #3429
                  val annArg = tree2ConstArg(rhs, sym.tpe.resultType)
                  (sym.name, annArg)
                }
              case arg =>
                reportAnnotationError(ClassfileAnnotationsAsNamedArgsError(arg))
                (nme.ERROR, None)
            }
            for (sym <- names) {
              // make sure the flags are up to date before erroring (jvm/t3415 fails otherwise)
              sym.initialize
              if (!sym.hasAnnotation(AnnotationDefaultAttr) && !sym.hasDefault)
                reportAnnotationError(AnnotationMissingArgError(ann, annType, sym))
            }

            if (hasError) annotationError
            else AnnotationInfo(annType, List(), nvPairs map {p => (p._1.asInstanceOf[Name], p._2.get)}).setOriginal(Apply(typedFun, args).setPos(ann.pos)) // [Eugene+] why do we need this cast?
          }
        } else if (requireJava) {
          reportAnnotationError(NestedAnnotationError(ann, annType))
        } else {
          val typedAnn = if (selfsym == NoSymbol) {
            // local dummy fixes SI-5544
            val localTyper = newTyper(context.make(ann, context.owner.newLocalDummy(ann.pos)))
            localTyper.typed(ann, mode, annClass.tpe)
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
              AnnotationInfo(annType, args, List()).setOriginal(typedAnn).setPos(t.pos)

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
              reportAnnotationError(UnexpectedTreeAnnotationError(t, typedAnn))
          }

          if (annType.typeSymbol == DeprecatedAttr && argss.flatten.size < 2)
            unit.deprecationWarning(ann.pos, "@deprecated now takes two arguments; see the scaladoc.")

          if ((typedAnn.tpe == null) || typedAnn.tpe.isErroneous) annotationError
          else annInfo(typedAnn)
        }
      }

      if (hasError) {
        pending.foreach(ErrorUtils.issueTypeError)
        annotationError
      } else res
    }

    def isRawParameter(sym: Symbol) = // is it a type parameter leaked by a raw type?
      sym.isTypeParameter && sym.owner.isJavaDefined

    /** If we map a set of hidden symbols to their existential bounds, we
     *  have a problem: the bounds may themselves contain references to the
     *  hidden symbols.  So this recursively calls existentialBound until
     *  the typeSymbol is not amongst the symbols being hidden.
     */
    def existentialBoundsExcludingHidden(hidden: List[Symbol]): Map[Symbol, Type] = {
      def safeBound(t: Type): Type =
        if (hidden contains t.typeSymbol) safeBound(t.typeSymbol.existentialBound.bounds.hi) else t

      def hiBound(s: Symbol): Type = safeBound(s.existentialBound.bounds.hi) match {
        case tp @ RefinedType(parents, decls) =>
          val parents1 = parents mapConserve safeBound
          if (parents eq parents1) tp
          else copyRefinedType(tp, parents1, decls)
        case tp => tp
      }

      // Hanging onto lower bound in case anything interesting
      // happens with it.
      mapFrom(hidden)(s => s.existentialBound match {
        case TypeBounds(lo, hi) => TypeBounds(lo, hiBound(s))
        case _                  => hiBound(s)
      })
    }

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
      val allBounds = existentialBoundsExcludingHidden(rawSyms)
      val typeParams: List[Symbol] = rawSyms map { sym =>
        val name = sym.name match {
          case x: TypeName  => x
          case x            => tpnme.singletonName(x)
        }
        val bound      = allBounds(sym)
        val sowner     = if (isRawParameter(sym)) context.owner else sym.owner
        val quantified = sowner.newExistential(name, sym.pos)

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

    def isReferencedFrom(ctx: Context, sym: Symbol): Boolean =
      ctx.owner.isTerm &&
      (ctx.scope.exists { dcl => dcl.isInitialized && (dcl.info contains sym) }) ||
      {
        var ctx1 = ctx.outer
        while ((ctx1 != NoContext) && (ctx1.scope eq ctx.scope)) ctx1 = ctx1.outer
        (ctx1 != NoContext) && isReferencedFrom(ctx1, sym)
      }

    def isCapturedExistential(sym: Symbol) =
      (sym hasAllFlags (EXISTENTIAL | CAPTURED)) && {
      val start = startTimer(isReferencedNanos)
      try !isReferencedFrom(context, sym)
      finally stopTimer(isReferencedNanos, start)
    }

    def packCaptured(tpe: Type): Type = {
      val captured = mutable.Set[Symbol]()
      for (tp <- tpe)
        if (isCapturedExistential(tp.typeSymbol))
          captured += tp.typeSymbol
      existentialAbstraction(captured.toList, tpe)
    }

    /** convert local symbols and skolems to existentials */
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
                InferTypeWithVolatileTypeSelectionError(tree, pre)
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
              AbstractExistentiallyOverParamerizedTpeError(tree, tp)
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

    def typedClassOf(tree: Tree, tpt: Tree, noGen: Boolean = false) =
      if (!checkClassType(tpt) && noGen) tpt
      else atPos(tree.pos)(gen.mkClassOf(tpt.tpe))

    protected def typedExistentialTypeTree(tree: ExistentialTypeTree, mode: Int): Tree = {
      for (wc <- tree.whereClauses)
        if (wc.symbol == NoSymbol) { namer.enterSym(wc); wc.symbol setFlag EXISTENTIAL }
        else context.scope enter wc.symbol
      val whereClauses1 = typedStats(tree.whereClauses, context.owner)
      for (vd @ ValDef(_, _, _, _) <- tree.whereClauses)
        if (vd.symbol.tpe.isVolatile)
          AbstractionFromVolatileTypeError(vd)
      val tpt1 = typedType(tree.tpt, mode)
      existentialTransform(tree.whereClauses map (_.symbol), tpt1.tpe)((tparams, tp) =>
        TypeTree(newExistentialType(tparams, tp)) setOriginal tree
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
            (arg, tparam) => typedHigherKindedType(arg, mode, GenPolyType(tparam.typeParams, AnyClass.tpe))
          }
        } else // @M: there's probably something wrong when args.length != tparams.length... (triggered by bug #320)
         // Martin, I'm using fake trees, because, if you use args or arg.map(typedType),
         // inferPolyAlternatives loops...  -- I have no idea why :-(
         // ...actually this was looping anyway, see bug #278.
          return TypedApplyWrongNumberOfTpeParametersError(fun, fun)

        typedTypeApply(tree, mode, fun, args1)
      case SingleType(_, _) =>
        typedTypeApply(tree, mode, fun setType fun.tpe.widen, args)
      case PolyType(tparams, restpe) if tparams.nonEmpty =>
        if (sameLength(tparams, args)) {
          val targs = args map (_.tpe)
          checkBounds(tree, NoPrefix, NoSymbol, tparams, targs, "")
          if (fun.symbol == Predef_classOf)
            typedClassOf(tree, args.head, true)
          else {
            if (!isPastTyper && fun.symbol == Any_isInstanceOf && !targs.isEmpty)
              checkCheckable(tree, targs.head, "")
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
          TypedApplyWrongNumberOfTpeParametersError(tree, fun)
        }
      case ErrorType =>
        setError(treeCopy.TypeApply(tree, fun, args))
      case _ =>
        fun match {
          // drop the application for an applyDynamic or selectDynamic call since it has been pushed down
          case treeInfo.DynamicApplication(_, _) => fun
          case _ => TypedApplyDoesNotTakeTpeParametersError(tree, fun)
        }
    }

    object dyna {
      import treeInfo.{isApplyDynamicName, DynamicUpdate, DynamicApplicationNamed}

      def acceptsApplyDynamic(tp: Type) = tp.typeSymbol isNonBottomSubClass DynamicClass

      /** Returns `Some(t)` if `name` can be selected dynamically on `qual`, `None` if not.
       * `t` specifies the type to be passed to the applyDynamic/selectDynamic call (unless it is NoType)
       * NOTE: currently either returns None or Some(NoType) (scala-virtualized extends this to Some(t) for selections on staged Structs)
       */
      def acceptsApplyDynamicWithType(qual: Tree, name: Name): Option[Type] =
        // don't selectDynamic selectDynamic, do select dynamic at unknown type,
        // in scala-virtualized, we may return a Some(tp) where tp ne NoType
        if (!isApplyDynamicName(name) && acceptsApplyDynamic(qual.tpe.widen)) Some(NoType)
        else None

      def isDynamicallyUpdatable(tree: Tree) = tree match {
        case DynamicUpdate(qual, name) =>
          // if the qualifier is a Dynamic, that's all we need to know
          acceptsApplyDynamic(qual.tpe)
        case _ => false
      }

      def isApplyDynamicNamed(fun: Tree): Boolean = fun match {
        case DynamicApplicationNamed(qual, _) if acceptsApplyDynamic(qual.tpe.widen) => true
        case _ => false
          // look deeper?
          // val methPart = treeInfo.methPart(fun)
          // println("methPart of "+ fun +" is "+ methPart)
          // if (methPart ne fun) isApplyDynamicNamed(methPart)
          // else false
      }

      def typedNamedApply(orig: Tree, fun: Tree, args: List[Tree], mode: Int, pt: Type): Tree = {
        def argToBinding(arg: Tree): Tree = arg match {
          case AssignOrNamedArg(Ident(name), rhs) => gen.mkTuple(List(CODE.LIT(name.toString), rhs))
          case _ => gen.mkTuple(List(CODE.LIT(""), arg))
        }
        typed(treeCopy.Apply(orig, fun, args map argToBinding), mode, pt)
      }

      /** Translate selection that does not typecheck according to the normal rules into a selectDynamic/applyDynamic.
       *
       * foo.method("blah")  ~~> foo.applyDynamic("method")("blah")
       * foo.method(x = "blah")  ~~> foo.applyDynamicNamed("method")(("x", "blah"))
       * foo.varia = 10      ~~> foo.updateDynamic("varia")(10)
       * foo.field           ~~> foo.selectDynamic("field")
       * foo.arr(10) = 13    ~~> foo.selectDynamic("arr").update(10, 13)
       *
       * what if we want foo.field == foo.selectDynamic("field") == 1, but `foo.field = 10` == `foo.selectDynamic("field").update(10)` == ()
       * what would the signature for selectDynamic be? (hint: it needs to depend on whether an update call is coming or not)
       *
       * need to distinguish selectDynamic and applyDynamic somehow: the former must return the selected value, the latter must accept an apply or an update
       *  - could have only selectDynamic and pass it a boolean whether more is to come,
       *    so that it can either return the bare value or something that can handle the apply/update
       *      HOWEVER that makes it hard to return unrelated values for the two cases
       *      --> selectDynamic's return type is now dependent on the boolean flag whether more is to come
       *  - simplest solution: have two method calls
       *
       */
      def mkInvoke(cxTree: Tree, tree: Tree, qual: Tree, name: Name): Option[Tree] =
        acceptsApplyDynamicWithType(qual, name) map { tp =>
          // tp eq NoType => can call xxxDynamic, but not passing any type args (unless specified explicitly by the user)
          // in scala-virtualized, when not NoType, tp is passed as type argument (for selection on a staged Struct)

          // strip off type application -- we're not doing much with outer, so don't bother preserving cxTree's attributes etc
          val (outer, explicitTargs) = cxTree match {
              case TypeApply(fun, targs) => (fun, targs)
              case Apply(TypeApply(fun, targs), args) => (Apply(fun, args), targs)
              case t => (t, Nil)
          }

          @inline def hasNamedArg(as: List[Tree]) = as collectFirst {case AssignOrNamedArg(lhs, rhs) =>} nonEmpty

          // note: context.tree includes at most one Apply node
          // thus, we can't use it to detect we're going to receive named args in expressions such as:
          //   qual.sel(a)(a2, arg2 = "a2")
          val oper = outer match {
            case Apply(`tree`, as) =>
              val oper =
                if (hasNamedArg(as))  nme.applyDynamicNamed
                else                  nme.applyDynamic
              // not supported: foo.bar(a1,..., an: _*)
              if (treeInfo.isWildcardStarArgList(as)) {
               DynamicVarArgUnsupported(tree, oper)
               return Some(setError(tree))
              } else oper
            case Assign(`tree`, _) => nme.updateDynamic
            case _                 => nme.selectDynamic
          }

          val dynSel  = Select(qual, oper)
          val tappSel = if (explicitTargs nonEmpty) TypeApply(dynSel, explicitTargs) else dynSel

          atPos(qual.pos)(Apply(tappSel, List(Literal(Constant(name.decode)))))
        }
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

    def typed1(tree: Tree, mode: Int, pt: Type): Tree = {
      def isPatternMode = inPatternMode(mode)

      //Console.println("typed1("+tree.getClass()+","+Integer.toHexString(mode)+","+pt+")")
      //@M! get the type of the qualifier in a Select tree, otherwise: NoType
      def prefixType(fun: Tree): Type = fun match {
        case Select(qualifier, _) => qualifier.tpe
//        case Ident(name) => ??
        case _ => NoType
      }

      def typedAnnotated(ann: Tree, arg1: Tree): Tree = {
        /** mode for typing the annotation itself */
        val annotMode = mode & ~TYPEmode | EXPRmode

        def resultingTypeTree(tpe: Type) = {
          // we need symbol-ful originals for reification
          // hence we go the extra mile to hand-craft tis guy
          val original =
            if (arg1.isType)
              (tree, arg1) match {
                case (Annotated(annot, arg), tt @ TypeTree()) => Annotated(annot, tt.original)
                // this clause is needed to correctly compile stuff like "new C @D" or "@(inline @getter)"
                case (Annotated(annot, arg), _) => Annotated(annot, arg1)
                case _ => throw new Error("unexpected trees in typedAnnotated: tree = %s, arg1 = %s".format(showRaw(tree), showRaw(arg1)))
              }
            else
              tree
          original setType ann.tpe
          TypeTree(tpe) setOriginal original setPos tree.pos.focus
        }

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
                      newValue (nme.self, ann.pos)
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
              // Erroneous annotations were already reported in typedAnnotation
              arg1  // simply drop erroneous annotations
            else {
              ann.tpe = atype
              resultingTypeTree(atype)
            }
          } else {
            // the annotation was typechecked before
            resultingTypeTree(ann.tpe)
          }
        }
        else {
          if (ann.tpe == null) {
            val annotInfo = typedAnnotation(ann, annotMode)
            ann.tpe = arg1.tpe.withAnnotation(annotInfo)
          }
          val atype = ann.tpe
          Typed(arg1, resultingTypeTree(atype)) setPos tree.pos setType atype
        }
      }

      def typedBind(name: Name, body: Tree) = {
        var vble = tree.symbol
        def typedBindType(name: TypeName) = {
          assert(body == EmptyTree, context.unit + " typedBind: " + name.debugString + " " + body + " " + body.getClass)
          if (vble == NoSymbol)
            vble =
              if (isFullyDefined(pt))
                context.owner.newAliasType(name, tree.pos) setInfo pt
              else
                context.owner.newAbstractType(name, tree.pos) setInfo TypeBounds.empty
          val rawInfo = vble.rawInfo
          vble = if (vble.name == tpnme.WILDCARD) context.scope.enter(vble)
                 else namer.enterInScope(vble)
          tree setSymbol vble setType vble.tpe
        }
        def typedBindTerm(name: TermName) = {
          if (vble == NoSymbol)
            vble = context.owner.newValue(name, tree.pos)
          if (vble.name.toTermName != nme.WILDCARD) {
            if ((mode & ALTmode) != 0)
              VariableInPatternAlternativeError(tree)
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
             else arrayType(elemtpt1.tpe)).notNull)
      }

      def typedAssign(lhs: Tree, rhs: Tree): Tree = {
        val lhs1    = typed(lhs, EXPRmode | LHSmode, WildcardType)
        val varsym  = lhs1.symbol

        // see #2494 for double error message example
        def fail() =
          if (lhs1.isErrorTyped) lhs1
          else AssignmentError(tree, varsym)

        if (varsym == null)
          return fail()

        if (treeInfo.mayBeVarGetter(varsym)) {
          treeInfo.methPart(lhs1) match {
            case Select(qual, name) =>
              val sel = Select(qual, nme.getterToSetter(name.toTermName)) setPos lhs.pos
              val app = Apply(sel, List(rhs)) setPos tree.pos
              return typed(app, mode, pt)

            case _ =>
          }
        }
//      if (varsym.isVariable ||
//        // setter-rewrite has been done above, so rule out methods here, but, wait a minute, why are we assigning to non-variables after erasure?!
//        (phase.erasedTypes && varsym.isValue && !varsym.isMethod)) {
        if (varsym.isVariable || varsym.isValue && phase.erasedTypes) {
          val rhs1 = typed(rhs, EXPRmode | BYVALmode, lhs1.tpe)
          treeCopy.Assign(tree, lhs1, checkDead(rhs1)) setType UnitClass.tpe
        }
        else if(dyna.isDynamicallyUpdatable(lhs1)) {
          val rhs1 = typed(rhs, EXPRmode | BYVALmode, WildcardType)
          typed1(Apply(lhs1, List(rhs1)), mode, pt)
        }
        else fail()
      }

      def typedIf(cond: Tree, thenp: Tree, elsep: Tree) = {
        val cond1 = checkDead(typed(cond, EXPRmode | BYVALmode, BooleanClass.tpe))
        if (elsep.isEmpty) { // in the future, should be unnecessary
          val thenp1 = typed(thenp, UnitClass.tpe)
          treeCopy.If(tree, cond1, thenp1, elsep) setType thenp1.tpe
        } else {
          var thenp1 = typed(thenp, pt)
          var elsep1 = typed(elsep, pt)

          lazy val thenTp = packedType(thenp1, context.owner)
          lazy val elseTp = packedType(elsep1, context.owner)
          // println("typedIf: "+(thenp1.tpe, elsep1.tpe, ptOrLub(List(thenp1.tpe, elsep1.tpe)),"\n", thenTp, elseTp, thenTp =:= elseTp))
          val (owntype, needAdapt) =
            // in principle we should pack the types of each branch before lubbing, but lub doesn't really work for existentials anyway
            // in the special (though common) case where the types are equal, it pays to pack before comparing
            // especially virtpatmat needs more aggressive unification of skolemized types
            // this breaks src/library/scala/collection/immutable/TrieIterator.scala
            if ( opt.virtPatmat && !isPastTyper
              && thenp1.tpe.annotations.isEmpty && elsep1.tpe.annotations.isEmpty // annotated types need to be lubbed regardless (at least, continations break if you by pass them like this)
              && thenTp =:= elseTp
               ) (thenp1.tpe, false) // use unpacked type
            // TODO: skolemize (lub of packed types) when that no longer crashes on files/pos/t4070b.scala
            else ptOrLub(List(thenp1.tpe, elsep1.tpe), pt)

          if (needAdapt) { //isNumericValueType(owntype)) {
            thenp1 = adapt(thenp1, mode, owntype)
            elsep1 = adapt(elsep1, mode, owntype)
          }
          treeCopy.If(tree, cond1, thenp1, elsep1) setType owntype
        }
      }

      // under -Xexperimental (and not -Xoldpatmat), and when there's a suitable __match in scope, virtualize the pattern match
      // otherwise, type the Match and leave it until phase `patmat` (immediately after typer)
      // empty-selector matches are transformed into synthetic PartialFunction implementations when the expected type demands it
      def typedVirtualizedMatch(tree: Tree, selector: Tree, cases: List[CaseDef]): Tree =
        if (selector == EmptyTree) {
          if (newPatternMatching && (pt.typeSymbol == PartialFunctionClass)) (new MatchFunTyper(tree, cases, mode, pt)).translated
          else {
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
          }
        } else
          virtualizedMatch(typedMatch(selector, cases, mode, pt, tree), mode, pt)

      def typedReturn(expr: Tree) = {
        val enclMethod = context.enclMethod
        if (enclMethod == NoContext ||
            enclMethod.owner.isConstructor ||
            context.enclClass.enclMethod == enclMethod // i.e., we are in a constructor of a local class
            ) {
          ReturnOutsideOfDefError(tree)
        } else {
          val DefDef(_, name, _, _, restpt, _) = enclMethod.tree
          if (restpt.tpe eq null) {
            ReturnWithoutTypeError(tree, enclMethod.owner)
          } else {
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
          if (checkStablePrefixClassType(tpt0))
            if (tpt0.hasSymbol && !tpt0.symbol.typeParams.isEmpty) {
              context.undetparams = cloneSymbols(tpt0.symbol.typeParams)
              notifyUndetparamsAdded(context.undetparams)
              TypeTree().setOriginal(tpt0)
                        .setType(appliedType(tpt0.tpe, context.undetparams map (_.tpeHK))) // @PP: tpeHK! #3343, #4018, #4347.
            } else tpt0
          else tpt0
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
          IsAbstractError(tree, sym)
        else if (isPrimitiveValueClass(sym)) {
          NotAMemberError(tpt, TypeTree(tp), nme.CONSTRUCTOR)
          setError(tpt)
        }
        else if (!(  tp == sym.thisSym.tpe // when there's no explicit self type -- with (#3612) or without self variable
                     // sym.thisSym.tpe == tp.typeOfThis (except for objects)
                  || narrowRhs(tp) <:< tp.typeOfThis
                  || phase.erasedTypes
                  )) {
          DoesNotConformToSelfTypeError(tree, sym, tp.typeOfThis)
        } else
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
          UnderscoreEtaError(expr1)
      }

      /**
       *  @param args ...
       *  @return     ...
       */
      def tryTypedArgs(args: List[Tree], mode: Int): Option[List[Tree]] = {
        val c = context.makeSilent(false)
        c.retyping = true
        try {
          val res = newTyper(c).typedArgs(args, mode)
          if (c.hasErrors) None else Some(res)
        } catch {
          case ex: CyclicReference =>
            throw ex
          case te: TypeError =>
            // @H some of typer erros can still leak,
            // for instance in continuations
            None
        } finally {
          c.flushBuffer()
        }
      }

      /** Try to apply function to arguments; if it does not work, try to convert Java raw to existentials, or try to
       *  insert an implicit conversion.
       */
      def tryTypedApply(fun: Tree, args: List[Tree]): Tree = {
        val start = startTimer(failedApplyNanos)

        def onError(typeError: AbsTypeError): Tree = {
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
            def errorInResult(tree: Tree) = treesInResult(tree) exists (_.pos == typeError.errPos)

            val retry = (typeError.errPos != null) && (fun :: tree :: args exists errorInResult)
            printTyping {
              val funStr = ptTree(fun) + " and " + (args map ptTree mkString ", ")
              if (retry) "second try: " + funStr
              else "no second try: " + funStr + " because error not in result: " + typeError.errPos+"!="+tree.pos
            }
            if (retry) {
              val Select(qual, name) = fun
              tryTypedArgs(args, forArgMode(fun, mode)) match {
                case Some(args1) =>
                  val qual1 =
                    if (!pt.isError) adaptToArguments(qual, name, args1, pt, true, true)
                    else qual
                  if (qual1 ne qual) {
                    val tree1 = Apply(Select(qual1, name) setPos fun.pos, args1) setPos tree.pos
                    return typed1(tree1, mode | SNDTRYmode, pt)
                  }
                case _ => ()
              }
            }
            issue(typeError)
            setError(treeCopy.Apply(tree, fun, args))
        }

        silent(_.doTypedApply(tree, fun, args, mode, pt)) match {
          case SilentResultValue(t) =>
            t
          case SilentTypeError(err) =>
            onError(err)
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

          def onError(reportError: => Tree): Tree = {
              fun match {
                case Select(qual, name)
                if !isPatternMode && nme.isOpAssignmentName(newTermName(name.decode)) =>
                  val qual1 = typedQualifier(qual)
                  if (treeInfo.isVariableOrGetter(qual1)) {
                    stopTimer(failedOpEqNanos, opeqStart)
                    convertToAssignment(fun, qual1, name, args)
                  } else {
                    stopTimer(failedApplyNanos, appStart)
                      reportError
                  }
                case _ =>
                  stopTimer(failedApplyNanos, appStart)
                  reportError
              }
          }
          silent(_.typed(fun, forFunMode(mode), funpt),
                 if ((mode & EXPRmode) != 0) false else context.ambiguousErrors,
                 if ((mode & EXPRmode) != 0) tree else context.tree) match {
            case SilentResultValue(fun1) =>
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
              if (fun2.symbol == Array_apply && !res.isErrorTyped) {
                val checked = gen.mkCheckInit(res)
                // this check is needed to avoid infinite recursion in Duplicators
                // (calling typed1 more than once for the same tree)
                if (checked ne res) typed { atPos(tree.pos)(checked) }
                else res
              } else
                res
            case SilentTypeError(err) =>
              onError({issue(err); setError(tree)})
          }
        }
      }

      def convertToAssignment(fun: Tree, qual: Tree, name: Name, args: List[Tree]): Tree = {
        val prefix = name stripSuffix nme.EQL
        def mkAssign(vble: Tree): Tree =
          Assign(
            vble,
            Apply(
              Select(vble.duplicate, prefix) setPos fun.pos.focus, args) setPos tree.pos.makeTransparent
          ) setPos tree.pos

        def mkUpdate(table: Tree, indices: List[Tree]) = {
          gen.evalOnceAll(table :: indices, context.owner, context.unit) {
            case tab :: is =>
              def mkCall(name: Name, extraArgs: Tree*) = (
                Apply(
                  Select(tab(), name) setPos table.pos,
                  is.map(i => i()) ++ extraArgs
                ) setPos tree.pos
              )
              mkCall(
                nme.update,
                Apply(Select(mkCall(nme.apply), prefix) setPos fun.pos, args) setPos tree.pos
              )
            case _ => EmptyTree
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
              case _                        => UnexpectedTreeAssignmentConversionError(qual)
            }
        }
        typed1(tree1, mode, pt)
      }

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
            debuglog("Fatal: couldn't find site " + site + " in " + site.parents.map(_.typeSymbol.name))
            if (phase.erasedTypes && context.enclClass.owner.isImplClass) {
              // println(qual1)
              // println(clazz)
              // println(site)
              // println(site.parents)
              // println(mix)
              // the reference to super class got lost during erasure
              restrictionError(tree.pos, unit, "traits may not select fields or methods from super[C] where C is a class")
              ErrorType
            } else {
              MixinMissingParentClassNameError(tree, mix, clazz)
              ErrorType
            }
          } else if (!ps.tail.isEmpty) {
            AmbiguousParentClassError(tree)
            ErrorType
          } else {
            ps.head
          }
        }

        val owntype = (
          if (!mix.isEmpty) findMixinSuper(clazz.tpe)
          else if ((mode & SUPERCONSTRmode) != 0) clazz.info.firstParent
          else intersectionType(clazz.info.parents)
        )
        treeCopy.Super(tree, qual1, mix) setType SuperType(clazz.thisType, owntype)
      }

      def typedThis(qual: Name) = tree.symbol orElse qualifyingClass(tree, qual, packageOK = false) match {
        case NoSymbol => tree
        case clazz    =>
          tree setSymbol clazz setType clazz.thisType.underlying
          if (isStableContext(tree, mode, pt)) tree setType clazz.thisType else tree
      }

      /** Attribute a selection where <code>tree</code> is <code>qual.name</code>.
       *  <code>qual</code> is already attributed.
       *
       *  @param qual ...
       *  @param name ...
       *  @return     ...
       */
      def typedSelect(qual: Tree, name: Name): Tree = {
        def asDynamicCall = dyna.mkInvoke(context.tree, tree, qual, name) map (typed1(_, mode, pt))

        val sym = tree.symbol orElse member(qual, name) orElse {
          // symbol not found? --> try to convert implicitly to a type that does have the required
          // member.  Added `| PATTERNmode` to allow enrichment in patterns (so we can add e.g., an
          // xml member to StringContext, which in turn has an unapply[Seq] method)
          if (name != nme.CONSTRUCTOR && inExprModeOr(mode, PATTERNmode)) {
            val qual1 = adaptToMemberWithArgs(tree, qual, name, mode, true, true)
            if (qual1 ne qual)
              return typed(treeCopy.Select(tree, qual1, name), mode, pt)
          }
          NoSymbol
        }
        if (phase.erasedTypes && qual.isInstanceOf[Super] && tree.symbol != NoSymbol)
          qual.tpe = tree.symbol.owner.tpe

        if (!reallyExists(sym)) {
          if (context.owner.enclosingTopLevelClass.isJavaDefined && name.isTypeName) {
            val tree1 = atPos(tree.pos) { gen.convertToSelectFromType(qual, name)  }
            if (tree1 != EmptyTree) return typed1(tree1, mode, pt)
          }

          // try to expand according to Dynamic rules.
          asDynamicCall foreach (x => return x)

          debuglog(
            "qual = "+qual+":"+qual.tpe+
            "\nSymbol="+qual.tpe.termSymbol+"\nsymbol-info = "+qual.tpe.termSymbol.info+
            "\nscope-id = "+qual.tpe.termSymbol.info.decls.hashCode()+"\nmembers = "+qual.tpe.members+
            "\nname = "+name+"\nfound = "+sym+"\nowner = "+context.enclClass.owner
          )

          def makeInteractiveErrorTree = {
            val tree1 = tree match {
              case Select(_, _) => treeCopy.Select(tree, qual, name)
              case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
            }
            setError(tree1)
          }

          if (name == nme.ERROR && forInteractive)
            return makeInteractiveErrorTree

          if (!qual.tpe.widen.isErroneous) {
            if ((mode & QUALmode) != 0) {
              val lastTry = missingHook(qual.tpe.typeSymbol, name)
              if (lastTry != NoSymbol) return typed1(tree setSymbol lastTry, mode, pt)
            }
            NotAMemberError(tree, qual, name)
          }

          if (forInteractive) makeInteractiveErrorTree else setError(tree)
        } else {
          val tree1 = tree match {
            case Select(_, _) => treeCopy.Select(tree, qual, name)
            case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
          }
          val (result, accessibleError) = silent(_.makeAccessible(tree1, sym, qual.tpe, qual)) match {
            case SilentTypeError(err) =>
              if (err.kind != ErrorKinds.Access) {
                context issue err
                return setError(tree)
              }
              else (tree1, Some(err))
            case SilentResultValue(treeAndPre) =>
              (stabilize(treeAndPre._1, treeAndPre._2, mode, pt), None)
          }

          def isPotentialNullDeference() = {
            !isPastTyper &&
            !sym.isConstructor &&
            !(qual.tpe <:< NotNullClass.tpe) && !qual.tpe.isNotNull &&
            !(List(Any_isInstanceOf, Any_asInstanceOf) contains result.symbol)  // null.is/as is not a dereference
          }
          // unit is null here sometimes; how are we to know when unit might be null? (See bug #2467.)
          if (settings.warnSelectNullable.value && isPotentialNullDeference && unit != null)
            unit.warning(tree.pos, "potential null pointer dereference: "+tree)

          result match {
            // could checkAccessible (called by makeAccessible) potentially have skipped checking a type application in qual?
            case SelectFromTypeTree(qual@TypeTree(), name) if qual.tpe.typeArgs.nonEmpty => // TODO: somehow the new qual is not checked in refchecks
              treeCopy.SelectFromTypeTree(
                result,
                (TypeTreeWithDeferredRefCheck(){ () => val tp = qual.tpe; val sym = tp.typeSymbolDirect
                  // will execute during refchecks -- TODO: make private checkTypeRef in refchecks public and call that one?
                  checkBounds(qual, tp.prefix, sym.owner, sym.typeParams, tp.typeArgs, "")
                  qual // you only get to see the wrapped tree after running this check :-p
                }) setType qual.tpe setPos qual.pos,
                name)
            case _ if accessibleError.isDefined =>
              val qual1 = adaptToMemberWithArgs(tree, qual, name, mode, false, false)
              if (!qual1.isErrorTyped && (qual1 ne qual))
                typed(Select(qual1, name) setPos tree.pos, mode, pt)
              else
                // before failing due to access, try a dynamic call.
                asDynamicCall getOrElse {
                  issue(accessibleError.get)
                  setError(tree)
                }
            case _ =>
              result
          }
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
        var errorContainer: AbsTypeError = null
        @inline
        def ambiguousError(msg: String) = {
          assert(errorContainer == null, "Cannot set ambiguous error twice for identifier")
          errorContainer = AmbiguousIdentError(tree, name, msg)
        }
        @inline
        def identError(tree: AbsTypeError) = {
          assert(errorContainer == null, "Cannot set ambiguous error twice for identifier")
          errorContainer = tree
        }

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
          while (defSym == NoSymbol && cx != NoContext && (cx.scope ne null)) { // cx.scope eq null arises during FixInvalidSyms in Duplicators
            pre = cx.enclClass.prefix
            defEntry = cx.scope.lookupEntry(name)
            if ((defEntry ne null) && qualifies(defEntry.sym)) {
              // Right here is where SI-1987, overloading in package objects, can be
              // seen to go wrong. There is an overloaded symbol, but when referring
              // to the unqualified identifier from elsewhere in the package, only
              // the last definition is visible. So overloading mis-resolves and is
              // definition-order dependent, bad things. See run/t1987.scala.
              //
              // I assume the actual problem involves how/where these symbols are entered
              // into the scope. But since I didn't figure out how to fix it that way, I
              // catch it here by looking up package-object-defined symbols in the prefix.
              if (isInPackageObject(defEntry.sym, pre.typeSymbol)) {
                defSym = pre.member(defEntry.sym.name)
                if (defSym ne defEntry.sym) {
                  log("!!! Overloaded package object member resolved incorrectly.\n  Discarded: " +
                    defEntry.sym.defString + "\n  Using: " + defSym.defString)
                }
              }
              else
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
              while (errorContainer == null && !imports1.isEmpty &&
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
                else identError(SymbolNotFoundError(tree, name, context.owner, startingIdentContext))
              } else
                identError(InferErrorGen.AccessError(
                  tree, inaccessibleSym, context.enclClass.owner.thisType, context.enclClass.owner,
                  inaccessibleExplanation
                ))
              defSym = context.owner.newErrorSymbol(name)
            }
          }
        }
        if (errorContainer != null) {
          ErrorUtils.issueTypeError(errorContainer)
          setError(tree)
        } else {
          if (defSym.owner.isPackageClass)
            pre = defSym.owner.thisType

          // Inferring classOf type parameter from expected type.
          if (defSym.isThisSym) {
            typed1(This(defSym.owner) setPos tree.pos, mode, pt)
          }
          // Inferring classOf type parameter from expected type.  Otherwise an
          // actual call to the stubbed classOf method is generated, returning null.
          else if (isPredefMemberNamed(defSym, nme.classOf) && pt.typeSymbol == ClassClass && pt.typeArgs.nonEmpty)
            typedClassOf(tree, TypeTree(pt.typeArgs.head))
          else {
            val tree1 = (
              if (qual == EmptyTree) tree
              // atPos necessary because qualifier might come from startContext
              else atPos(tree.pos)(Select(qual, name))
            )
            val (tree2, pre2) = makeAccessible(tree1, defSym, pre, qual)
            // assert(pre.typeArgs isEmpty) // no need to add #2416-style check here, right?
            stabilize(tree2, pre2, mode, pt)
          }
        }
      }

      def typedCompoundTypeTree(templ: Template) = {
        val parents1 = templ.parents mapConserve (typedType(_, mode))
        if (parents1 exists (_.isErrorTyped)) tree setType ErrorType
        else {
          val decls = newScope
          //Console.println("Owner: " + context.enclClass.owner + " " + context.enclClass.owner.id)
          val self = refinedType(parents1 map (_.tpe), context.enclClass.owner, decls, templ.pos)
          newTyper(context.make(templ, self.typeSymbol, decls)).typedRefinement(templ.body)
          tree setType self
        }
      }

      def typedAppliedTypeTree(tpt: Tree, args: List[Tree]) = {
        val tpt1 = typed1(tpt, mode | FUNmode | TAPPmode, WildcardType)
        if (tpt1.isErrorTyped) {
          tpt1
        } else if (!tpt1.hasSymbol) {
          AppliedTypeNoParametersError(tree, tpt1.tpe)
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
                typedHigherKindedType(arg, mode, GenPolyType(tparam.typeParams, AnyClass.tpe))
              }
            val argtypes = args1 map (_.tpe)

            foreach2(args, tparams)((arg, tparam) => arg match {
              // note: can't use args1 in selector, because Bind's got replaced
              case Bind(_, _) =>
                if (arg.symbol.isAbstractType)
                  arg.symbol setInfo // XXX, feedback. don't trackSymInfo here!
                    TypeBounds(
                      lub(List(arg.symbol.info.bounds.lo, tparam.info.bounds.lo.subst(tparams, argtypes))),
                      glb(List(arg.symbol.info.bounds.hi, tparam.info.bounds.hi.subst(tparams, argtypes))))
              case _ =>
            })
            val original = treeCopy.AppliedTypeTree(tree, tpt1, args1)
            val result = TypeTree(appliedType(tpt1.tpe, argtypes)) setOriginal original
            if(tpt1.tpe.isInstanceOf[PolyType]) // did the type application (performed by appliedType) involve an unchecked beta-reduction?
              TypeTreeWithDeferredRefCheck(){ () =>
                // wrap the tree and include the bounds check -- refchecks will perform this check (that the beta reduction was indeed allowed) and unwrap
                // we can't simply use original in refchecks because it does not contains types
                // (and the only typed trees we have have been mangled so they're not quite the original tree anymore)
                checkBounds(result, tpt1.tpe.prefix, tpt1.symbol.owner, tpt1.symbol.typeParams, argtypes, "")
                result // you only get to see the wrapped tree after running this check :-p
              } setType (result.tpe) setPos(result.pos)
            else result
          } else if (tparams.isEmpty) {
            AppliedTypeNoParametersError(tree, tpt1.tpe)
          } else {
            //Console.println("\{tpt1}:\{tpt1.symbol}:\{tpt1.symbol.info}")
            if (settings.debug.value) Console.println(tpt1+":"+tpt1.symbol+":"+tpt1.symbol.info)//debug
            AppliedTypeWrongNumberOfArgsError(tree, tpt1, tparams)
          }
        }
      }

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
          // flag default getters for constructors. An actual flag would be nice. See SI-5543.
          //val flag = ddef.mods.hasDefaultFlag && ddef.mods.hasFlag(PRESUPER)
          val flag = ddef.mods.hasDefaultFlag && sym.owner.isModuleClass &&
                     nme.defaultGetterToMethod(sym.name) == nme.CONSTRUCTOR
          newTyper(context.makeNewScope(tree, sym)).constrTyperIf(flag).typedDefDef(ddef)

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
                case SilentTypeError(err) =>
                  unit.warning(useCase.pos, err.errMsg)
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
          typerWithLocalContext(context.makeNewScope(tree, context.owner)){
            _.typedBlock(tree, mode, pt)
          }

        case Alternative(alts) =>
          val alts1 = alts mapConserve (alt => typed(alt, mode | ALTmode, pt))
          treeCopy.Alternative(tree, alts1) setType pt

        case Star(elem) =>
          if ((mode & STARmode) == 0 && !isPastTyper)
            StarPatternWithVarargParametersError(tree)
          treeCopy.Star(tree, typed(elem, mode, pt)) setType makeFullyDefined(pt)

        case Bind(name, body) =>
          typedBind(name, body)

        case UnApply(fun, args) =>
          val fun1 = typed(fun)
          val tpes = formalTypes(unapplyTypeList(fun.symbol, fun1.tpe), args.length)
          val args1 = map2(args, tpes)(typedPattern)
          treeCopy.UnApply(tree, fun1, args1) setType pt

        case ArrayValue(elemtpt, elems) =>
          typedArrayValue(elemtpt, elems)

        case tree @ Function(_, _) =>
          if (tree.symbol == NoSymbol)
            tree.symbol = context.owner.newAnonymousFunctionValue(tree.pos)
          typerWithLocalContext(context.makeNewScope(tree, tree.symbol))(_.typedFunction(tree, mode, pt))

        case Assign(lhs, rhs) =>
          typedAssign(lhs, rhs)

        case AssignOrNamedArg(lhs, rhs) => // called by NamesDefaults in silent typecheck
          typedAssign(lhs, rhs)

        case If(cond, thenp, elsep) =>
          typedIf(cond, thenp, elsep)

        case tree @ Match(selector, cases) =>
          typedVirtualizedMatch(tree, selector, cases)

        case Return(expr) =>
          typedReturn(expr)

        case Try(block, catches, finalizer) =>
          var block1 = typed(block, pt)
          var catches1 = typedCases(catches, ThrowableClass.tpe, pt)
          val finalizer1 = if (finalizer.isEmpty) finalizer
                           else typed(finalizer, UnitClass.tpe)
          val (owntype, needAdapt) = ptOrLub(block1.tpe :: (catches1 map (_.tpe)), pt)
          if (needAdapt) {
            block1 = adapt(block1, mode, owntype)
            catches1 = catches1 map (adaptCase(_, mode, owntype))
          }

          treeCopy.Try(tree, block1, catches1, finalizer1) setType owntype

        case Throw(expr) =>
          val expr1 = typed(expr, EXPRmode | BYVALmode, ThrowableClass.tpe)
          treeCopy.Throw(tree, expr1) setType NothingClass.tpe

        case New(tpt: Tree) =>
          typedNew(tpt)

        case Typed(expr, Function(List(), EmptyTree)) =>
          // find out whether the programmer is trying to eta-expand a macro def
          // to do that we need to typecheck the tree first (we need a symbol of the eta-expandee)
          // that typecheck must not trigger macro expansions, so we explicitly prohibit them
          // Q: "but, " - you may ask - ", `typed1` doesn't call adapt, which does macro expansion, so why explicit check?"
          // A: solely for robustness reasons. this mechanism might change in the future, which might break unprotected code
          val expr1 = context.withMacrosDisabled(typed1(expr, mode, pt))
          expr1 match {
            case macroDef if macroDef.symbol != null && macroDef.symbol.isTermMacro && !macroDef.symbol.isErroneous =>
              MacroEtaError(expr1)
            case _ =>
              typedEta(checkDead(expr1))
          }

        case Typed(expr0, tpt @ Ident(tpnme.WILDCARD_STAR))  =>
          val expr = typed(expr0, onlyStickyModes(mode), WildcardType)
          def subArrayType(pt: Type) =
            if (isPrimitiveValueClass(pt.typeSymbol) || !isFullyDefined(pt)) arrayType(pt)
            else {
              val tparam = context.owner freshExistential "" setInfo TypeBounds.upper(pt)
              newExistentialType(List(tparam), arrayType(tparam.tpe))
            }

          val (expr1, baseClass) = expr.tpe.typeSymbol match {
            case ArrayClass => (adapt(expr, onlyStickyModes(mode), subArrayType(pt)), ArrayClass)
            case _          => (adapt(expr, onlyStickyModes(mode), seqType(pt)), SeqClass)
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
          val ownType = if (isPatternMode) inferTypedPattern(tpt1, tpt1.tpe, pt) else tpt1.tpe
          treeCopy.Typed(tree, expr1, tpt1) setType ownType

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
                        (arg, tparam) => typedHigherKindedType(arg, mode, GenPolyType(tparam.typeParams, AnyClass.tpe))
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
              // convert new Array[T](len) to evidence[ClassTag[T]].newArray(len)
              // convert new Array^N[T](len) for N > 1 to evidence[ClassTag[Array[...Array[T]...]]].newArray(len), where Array HK gets applied (N-1) times
              // [Eugene] no more MaxArrayDims. ClassTags are flexible enough to allow creation of arrays of arbitrary dimensionality (w.r.t JVM restrictions)
              val Some((level, componentType)) = erasure.GenericArray.unapply(tpt.tpe)
              val tagType = List.iterate(componentType, level)(tpe => appliedType(ArrayClass.asType, List(tpe))).last
              val newArrayApp = atPos(tree.pos) {
                val tag = resolveArrayTag(tagType, tree.pos)
                if (tag.isEmpty) MissingArrayTagError(tree, tagType)
                else new ApplyToImplicitArgs(Select(tag, nme.newArray), args)
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
                case SilentResultValue(result) =>
                  result
                case _ =>
                  silent(_ => typed1(Select(qual1, nme.filter) setPos tree.pos, mode, pt)) match {
                    case SilentResultValue(result2) =>
                      unit.deprecationWarning(
                        tree.pos, "`withFilter' method does not yet exist on "+qual1.tpe.widen+
                        ", using `filter' method instead")
                      result2
                    case SilentTypeError(err) =>
                      WithFilterError(tree, err)
                  }
              }
            else
              typedSelect(qual1, name)

          if (tree.isInstanceOf[PostfixSelect])
            checkFeature(tree.pos, PostfixOpsFeature, name.decode)
          if (tree1.symbol != null && tree1.symbol.isOnlyRefinementMember)
            checkFeature(tree1.pos, ReflectiveCallsFeature, tree1.symbol.toString)

          if (qual1.symbol == RootPackage) treeCopy.Ident(tree1, name)
          else tree1

        case Ident(name) =>
          incCounter(typedIdentCount)
          if ((name == nme.WILDCARD && (mode & (PATTERNmode | FUNmode)) == PATTERNmode) ||
              (name == tpnme.WILDCARD && (mode & TYPEmode) != 0))
            tree setType makeFullyDefined(pt)
          else
            typedIdent(name)

        case ReferenceToBoxed(idt @ Ident(_)) =>
          val id1 = typed1(idt, mode, pt) match { case id: Ident => id }
          // [Eugene] am I doing it right?
          val erasedTypes = phaseId(currentPeriod) >= currentRun.erasurePhase.id
          val tpe = capturedVariableType(idt.symbol, erasedTypes = erasedTypes)
          treeCopy.ReferenceToBoxed(tree, id1) setType tpe

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
          if (qual1.tpe.isVolatile) TypeSelectionFromVolatileTypeError(tree, qual)
          else typedSelect(qual1, selector)

        case CompoundTypeTree(templ) =>
          typedCompoundTypeTree(templ)

        case AppliedTypeTree(tpt, args) =>
          typedAppliedTypeTree(tpt, args)

        case TypeBoundsTree(lo, hi) =>
          val lo1 = typedType(lo, mode)
          val hi1 = typedType(hi, mode)
          treeCopy.TypeBoundsTree(tree, lo1, hi1) setType TypeBounds(lo1.tpe, hi1.tpe)

        case etpt @ ExistentialTypeTree(_, _) =>
          val tree1 = typerWithLocalContext(context.makeNewScope(tree, context.owner)){
            _.typedExistentialTypeTree(etpt, mode)
          }
          checkExistentialsFeature(tree1.pos, tree1.tpe, "the existential type")
          tree1

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
          assert(forInteractive, "!forInteractive") // should not happen in normal circumstances.
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
      lastTreeToTyper = tree
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
              "enrichmentEnabled"   -> context.enrichmentEnabled,
              "mode"             -> modeString(mode),
              "silent"           -> context.bufferErrors,
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
        if (!isPastTyper) signalDone(context.asInstanceOf[analyzer.Context], tree, result)
        result
      } catch {
        case ex: TypeError =>
          tree.tpe = null
          // The only problematic case are (recoverable) cyclic reference errors which can pop up almost anywhere.
          printTyping("caught %s: while typing %s".format(ex, tree)) //DEBUG

          reportTypeError(context, tree.pos, ex)
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

      // on the one hand,
      //   "abc" match { case Seq('a', 'b', 'c') => true }
      // should be ruled out statically, otherwise this is a runtime
      // error both because there is an implicit from String to Seq
      // (even though such implicits are not used by the matcher) and
      // because the typer is fine with concluding that "abc" might
      // be of type "String with Seq[T]" and thus eligible for a call
      // to unapplySeq.

      // on the other hand, we want to be able to use implicits to add members retro-actively (e.g., add xml to StringContext)

      // as a compromise, context.enrichmentEnabled tells adaptToMember to go ahead and enrich,
      // but arbitrary conversions (in adapt) are disabled
      // TODO: can we achieve the pattern matching bit of the string interpolation SIP without this?
      context.withImplicitsDisabledAllowEnrichment(typed(tree, PATTERNmode, pt))
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
        ConstructorPrefixError(tree, restpe)
      } else {
	      //@M fix for #2208
	      // if there are no type arguments, normalization does not bypass any checks, so perform it to get rid of AnyRef
	      if (result.tpe.typeArgs.isEmpty) {
	        // minimal check: if(result.tpe.typeSymbolDirect eq AnyRefClass) {
	        // must expand the fake AnyRef type alias, because bootstrapping (init in Definitions) is not
	        // designed to deal with the cycles in the scala package (ScalaObject extends
	        // AnyRef, but the AnyRef type alias is entered after the scala package is
	        // loaded and completed, so that ScalaObject is unpickled while AnyRef is not
	        // yet defined )
	        // !!! TODO - revisit now that ScalaObject is gone.
	        result setType(restpe)
	      } else { // must not normalize: type application must be (bounds-)checked (during RefChecks), see #2208
	        // during uncurry (after refchecks), all types are normalized
	        result
	      }
      }
    }

    def typedTypeConstructor(tree: Tree): Tree = typedTypeConstructor(tree, NOmode)

    def computeType(tree: Tree, pt: Type): Type = {
      // macros employ different logic of `computeType`
      assert(!context.owner.isTermMacro, context.owner)
      val tree1 = typed(tree, pt)
      transformed(tree) = tree1
      val tpe = packedType(tree1, context.owner)
      checkExistentialsFeature(tree.pos, tpe, "inferred existential type")
      tpe
    }

    def computeMacroDefType(tree: Tree, pt: Type): Type = {
      assert(context.owner.isTermMacro, context.owner)
      assert(tree.symbol.isTermMacro, tree.symbol)
      assert(tree.isInstanceOf[DefDef], tree.getClass)
      val ddef = tree.asInstanceOf[DefDef]

      val tree1 =
        if (transformed contains ddef.rhs) {
          // macro defs are typechecked in `methodSig` (by calling this method) in order to establish their link to macro implementation asap
          // if a macro def doesn't have explicitly specified return type, this method will be called again by `assignTypeToTree`
          // here we guard against this case
          transformed(ddef.rhs)
        } else {
          val tree1 = typedMacroBody(this, ddef)
          transformed(ddef.rhs) = tree1
          tree1
        }

      val isMacroBodyOkay = !tree.symbol.isErroneous && !(tree1 exists (_.isErroneous))
      if (isMacroBodyOkay) computeMacroDefTypeFromMacroImpl(ddef, tree.symbol, tree1.symbol) else AnyClass.tpe
    }

    def transformedOr(tree: Tree, op: => Tree): Tree = transformed.get(tree) match {
      case Some(tree1) => transformed -= tree; tree1
      case None => op
    }

    def transformedOrTyped(tree: Tree, mode: Int, pt: Type): Tree = transformed.get(tree) match {
      case Some(tree1) => transformed -= tree; tree1
      case None => typed(tree, mode, pt)
    }

/*
    def convertToTypeTree(tree: Tree): Tree = tree match {
      case TypeTree() => tree
      case _ => TypeTree(tree.tpe)
    }
*/
  }
}

