/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

// Added: Sat Oct 7 16:08:21 2006
//todo: use inherited type info also for vars and values

// Added: Thu Apr 12 18:23:58 2007
//todo: disallow C#D in superclass
//todo: treat :::= correctly
package scala
package tools.nsc
package typechecker

import scala.collection.{mutable, immutable}
import scala.reflect.internal.util.{ BatchSourceFile, Statistics, shortClassOfInstance, ListOfNil }
import mutable.ListBuffer
import symtab.Flags._
import Mode._

// Suggestion check whether we can do without priming scopes with symbols of outer scopes,
// like the IDE does.
/** This trait provides methods to assign types to trees.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Typers extends Adaptations with Tags with TypersTracking with PatternTypers {
  self: Analyzer =>

  import global._
  import definitions._
  import TypersStats._

  final def forArgMode(fun: Tree, mode: Mode) =
    if (treeInfo.isSelfOrSuperConstrCall(fun)) mode | SCCmode else mode

  // namer calls typer.computeType(rhs) on DefDef / ValDef when tpt is empty. the result
  // is cached here and re-used in typedDefDef / typedValDef
  // Also used to cache imports type-checked by namer.
  val transformed = new mutable.AnyRefMap[Tree, Tree]

  final val shortenImports = false

  // allows override of the behavior of the resetTyper method w.r.t comments
  def resetDocComments() = {
    clearDocComments()
  }

  def resetTyper() {
    //println("resetTyper called")
    resetContexts()
    resetImplicits()
    resetDocComments()
  }

  sealed abstract class SilentResult[+T] {
    def isEmpty: Boolean
    def nonEmpty = !isEmpty

    @inline final def fold[U](none: => U)(f: T => U): U = this match {
      case SilentResultValue(value) => f(value)
      case _                        => none
    }
    @inline final def map[U](f: T => U): SilentResult[U] = this match {
      case SilentResultValue(value) => SilentResultValue(f(value))
      case x: SilentTypeError       => x
    }
    @inline final def filter(p: T => Boolean): SilentResult[T] = this match {
      case SilentResultValue(value) if !p(value) => SilentTypeError(TypeErrorWrapper(new TypeError(NoPosition, "!p")))
      case _                                     => this
  }
    @inline final def orElse[T1 >: T](f: Seq[AbsTypeError] => T1): T1 = this match {
      case SilentResultValue(value) => value
      case s : SilentTypeError      => f(s.reportableErrors)
    }
  }
  class SilentTypeError private(val errors: List[AbsTypeError], val warnings: List[(Position, String)]) extends SilentResult[Nothing] {
    override def isEmpty = true
    def err: AbsTypeError = errors.head
    def reportableErrors = errors match {
      case (e1: AmbiguousImplicitTypeError) +: _ =>
        List(e1) // DRYer error reporting for neg/t6436b.scala
      case all =>
        all
    }
  }
  object SilentTypeError {
    def apply(errors: AbsTypeError*): SilentTypeError = apply(errors.toList, Nil)
    def apply(errors: List[AbsTypeError], warnings: List[(Position, String)]): SilentTypeError = new SilentTypeError(errors, warnings)
    // todo: this extracts only one error, should be a separate extractor.
    def unapply(error: SilentTypeError): Option[AbsTypeError] = error.errors.headOption
  }

  // todo: should include reporter warnings in SilentResultValue.
  // e.g. tryTypedApply could print warnings on arguments when the typing succeeds.
  case class SilentResultValue[+T](value: T) extends SilentResult[T] { override def isEmpty = false }

  def newTyper(context: Context): Typer = new NormalTyper(context)

  private class NormalTyper(context : Context) extends Typer(context)

  // A transient flag to mark members of anonymous classes
  // that are turned private by typedBlock
  private final val SYNTHETIC_PRIVATE = TRANS_FLAG

  private final val InterpolatorCodeRegex  = """\$\{.*?\}""".r
  private final val InterpolatorIdentRegex = """\$[$\w]+""".r // note that \w doesn't include $

  abstract class Typer(context0: Context) extends TyperDiagnostics with Adaptation with Tag with PatternTyper with TyperContextErrors {
    import context0.unit
    import typeDebug.{ ptTree, ptBlock, ptLine, inGreen, inRed }
    import TyperErrorGen._
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._

    private val transformed: mutable.Map[Tree, Tree] = unit.transformed

    val infer = new Inferencer {
      def context = Typer.this.context
      // See SI-3281 re undoLog
      override def isCoercible(tp: Type, pt: Type) = undoLog undo viewExists(tp, pt)
    }

    /** Overridden to false in scaladoc and/or interactive. */
    def canAdaptConstantTypeToLiteral = true
    def canTranslateEmptyListToNil    = true
    def missingSelectErrorTree(tree: Tree, qual: Tree, name: Name): Tree = tree

    def typedDocDef(docDef: DocDef, mode: Mode, pt: Type): Tree =
      typed(docDef.definition, mode, pt)

    /** Find implicit arguments and pass them to given tree.
     */
    def applyImplicitArgs(fun: Tree): Tree = fun.tpe match {
      case MethodType(params, _) =>
        val argResultsBuff = new ListBuffer[SearchResult]()
        val argBuff = new ListBuffer[Tree]()
        // paramFailed cannot be initialized with params.exists(_.tpe.isError) because that would
        // hide some valid errors for params preceding the erroneous one.
        var paramFailed = false
        var mkArg: (Name, Tree) => Tree = (_, tree) => tree

        // DEPMETTODO: instantiate type vars that depend on earlier implicit args (see adapt (4.1))
        //
        // apply the substitutions (undet type param -> type) that were determined
        // by implicit resolution of implicit arguments on the left of this argument
        for(param <- params) {
          var paramTp = param.tpe
          for(ar <- argResultsBuff)
            paramTp = paramTp.subst(ar.subst.from, ar.subst.to)

          val res = if (paramFailed || (paramTp.isErroneous && {paramFailed = true; true})) SearchFailure else inferImplicit(fun, paramTp, context.reportErrors, isView = false, context)
          argResultsBuff += res

          if (res.isSuccess) {
            argBuff += mkArg(param.name, res.tree)
          } else {
            mkArg = gen.mkNamedArg // don't pass the default argument (if any) here, but start emitting named arguments for the following args
            if (!param.hasDefault && !paramFailed) {
              context.reporter.reportFirstDivergentError(fun, param, paramTp)(context)
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

    def viewExists(from: Type, to: Type): Boolean = (
         !from.isError
      && !to.isError
      && context.implicitsEnabled
      && (inferView(context.tree, from, to, reportAmbiguous = false, saveErrors = true) != EmptyTree)
      // SI-8230 / SI-8463 We'd like to change this to `saveErrors = false`, but can't.
      // For now, we can at least pass in `context.tree` rather then `EmptyTree` so as
      // to avoid unpositioned type errors.
    )

    def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean): Tree =
      inferView(tree, from, to, reportAmbiguous, saveErrors = true)

    /** Infer an implicit conversion (`view`) between two types.
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
        case MethodType(_, _)     => EmptyTree
        case OverloadedType(_, _) => EmptyTree
        case PolyType(_, _)       => EmptyTree
        case _                    =>
          def wrapImplicit(from: Type): Tree = {
            val result = inferImplicit(tree, functionType(from.withoutAnnotations :: Nil, to), reportAmbiguous, isView = true, context, saveAmbiguousDivergent = saveErrors)
            if (result.subst != EmptyTreeTypeSubstituter) {
              result.subst traverse tree
              notifyUndetparamsInferred(result.subst.from, result.subst.to)
            }
            result.tree
          }
          wrapImplicit(from) orElse wrapImplicit(byNameType(from))
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
        val tp0 = tp.dealias
        if (tp eq tp0) {
          devWarning(s"dropExistential did not progress dealiasing $tp, see SI-7126")
          tp
        } else {
          val tp1 = dropExistential(tp0)
          if (tp1 eq tp0) tp else tp1
        }
      case _ => tp
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

    /** Check that type `tp` is not a subtype of itself.
     */
    def checkNonCyclic(pos: Position, tp: Type): Boolean = {
      def checkNotLocked(sym: Symbol) = {
        sym.initialize.lockOK || { CyclicAliasingOrSubtypingError(pos, sym); false }
      }
      tp match {
        case TypeRef(pre, sym, args) =>
          checkNotLocked(sym) &&
          ((!sym.isNonClassType) || checkNonCyclic(pos, appliedType(pre.memberInfo(sym), args), sym))
          // @M! info for a type ref to a type parameter now returns a polytype
          // @M was: checkNonCyclic(pos, pre.memberInfo(sym).subst(sym.typeParams, args), sym)

        case SingleType(pre, sym) =>
          checkNotLocked(sym)
        case st: SubType =>
          checkNonCyclic(pos, st.supertype)
        case ct: CompoundType =>
          ct.parents forall (x => checkNonCyclic(pos, x))
        case _ =>
          true
      }
    }

    def checkNonCyclic(pos: Position, tp: Type, lockedSym: Symbol): Boolean = try {
      if (!lockedSym.lock(CyclicReferenceError(pos, tp, lockedSym))) false
      else checkNonCyclic(pos, tp)
    } finally {
      lockedSym.unlock()
    }

    def checkNonCyclic(sym: Symbol) {
      if (!checkNonCyclic(sym.pos, sym.tpe_*)) sym.setInfo(ErrorType)
    }

    def checkNonCyclic(defn: Tree, tpt: Tree) {
      if (!checkNonCyclic(defn.pos, tpt.tpe, defn.symbol)) {
        tpt setType ErrorType
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
            if (tpe.isDependentMethodType)
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

      /** Check that type `tree` does not refer to private
       *  components unless itself is wrapped in something private
       *  (`owner` tells where the type occurs).
       */
      def privates[T <: Tree](owner: Symbol, tree: T): T =
        check(owner, EmptyScope, WildcardType, tree)

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
        else if (!isPastTyper) { // privates
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
                   !o.isLocalToBlock && !o.isPrivate &&
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
                t.dealias
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
          context.scope enter vparam.symbol
    }

    def reenterTypeParams(tparams: List[TypeDef]): List[Symbol] =
      for (tparam <- tparams) yield {
        context.scope enter tparam.symbol
        tparam.symbol.deSkolemize
      }

    /** The qualifying class
     *  of a this or super with prefix `qual`.
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
    final def typerWithLocalContext[T](c: Context)(f: Typer => T): T =
      c.reporter.propagatingErrorsTo(context.reporter)(f(newTyper(c)))

    /** The typer for a label definition. If this is part of a template we
     *  first have to enter the label definition.
     */
    def labelTyper(ldef: LabelDef): Typer =
      if (ldef.symbol == NoSymbol) { // labeldef is part of template
        val typer1 = newTyper(context.makeNewScope(ldef, context.owner))
        typer1.enterLabelDef(ldef)
        typer1
      } else this

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

    /** Does the context of tree `tree` require a stable type?
     */
    private def isStableContext(tree: Tree, mode: Mode, pt: Type) = {
      def ptSym = pt.typeSymbol
      def expectsStable = (
           pt.isStable
        || mode.inQualMode && !tree.symbol.isConstant
        || !(tree.tpe <:< pt) && (ptSym.isAbstractType && pt.bounds.lo.isStable || ptSym.isRefinementClass)
      )

      (    isNarrowable(tree.tpe)
        && mode.typingExprNotLhs
        && expectsStable
      )
    }

    /** Make symbol accessible. This means:
     *  If symbol refers to package object, insert `.package` as second to last selector.
     *  (exception for some symbols in scala package which are dealiased immediately)
     *  Call checkAccessible, which sets tree's attributes.
     *  Also note that checkAccessible looks up sym on pre without checking that pre is well-formed
     *  (illegal type applications in pre will be skipped -- that's why typedSelect wraps the resulting tree in a TreeWithDeferredChecks)
     *  @return modified tree and new prefix type
     */
    private def makeAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): (Tree, Type) =
      if (context.isInPackageObject(sym, pre.typeSymbol)) {
        if (pre.typeSymbol == ScalaPackageClass && sym.isTerm) {
          // short cut some aliases. It seems pattern matching needs this
          // to notice exhaustiveness and to generate good code when
          // List extractors are mixed with :: patterns. See Test5 in lists.scala.
          //
          // TODO SI-6609 Eliminate this special case once the old pattern matcher is removed.
          def dealias(sym: Symbol) =
            (atPos(tree.pos.makeTransparent) {gen.mkAttributedRef(sym)} setPos tree.pos, sym.owner.thisType)
          sym.name match {
            case nme.List => return dealias(ListModule)
            case nme.Seq  => return dealias(SeqModule)
            case nme.Nil  => return dealias(NilModule)
            case _ =>
          }
        }
        val qual = typedQualifier { atPos(tree.pos.makeTransparent) {
          tree match {
            case Ident(_) => Ident(rootMirror.getPackageObjectWithMember(pre, sym))
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

    /** Post-process an identifier or selection node, performing the following:
     *  1. Check that non-function pattern expressions are stable (ignoring volatility concerns -- SI-6815)
     *       (and narrow the type of modules: a module reference in a pattern has type Foo.type, not "object Foo")
     *  2. Check that packages and static modules are not used as values
     *  3. Turn tree type into stable type if possible and required by context.
     *  4. Give getClass calls a more precise type based on the type of the target of the call.
     */
    protected def stabilize(tree: Tree, pre: Type, mode: Mode, pt: Type): Tree = {

      // Side effect time! Don't be an idiot like me and think you
      // can move "val sym = tree.symbol" before this line, because
      // inferExprAlternative side-effects the tree's symbol.
      if (tree.symbol.isOverloaded && !mode.inFunMode)
        inferExprAlternative(tree, pt)

      val sym = tree.symbol
      val isStableIdPattern = mode.typingPatternNotConstructor && tree.isTerm

      def isModuleTypedExpr = (
           treeInfo.admitsTypeSelection(tree)
        && (isStableContext(tree, mode, pt) || sym.isModuleNotMethod)
      )
      def isStableValueRequired = (
           isStableIdPattern
        || mode.in(all = EXPRmode, none = QUALmode) && !phase.erasedTypes
      )
      // To fully benefit from special casing the return type of
      // getClass, we have to catch it immediately so expressions like
      // x.getClass().newInstance() are typed with the type of x. TODO: If the
      // type of the qualifier is inaccessible, we can cause private types to
      // escape scope here, e.g. pos/t1107. I'm not sure how to properly handle
      // this so for now it requires the type symbol be public.
      def isGetClassCall = isGetClass(sym) && pre.typeSymbol.isPublic

      def narrowIf(tree: Tree, condition: Boolean) =
        if (condition) tree setType singleType(pre, sym) else tree

      def checkStable(tree: Tree): Tree =
        if (treeInfo.isStableIdentifierPattern(tree)) tree
        else UnstableTreeError(tree)

      if (tree.isErrorTyped)
        tree
      else if (!sym.isValue && isStableValueRequired) // (2)
        NotAValueError(tree, sym)
      else if (isStableIdPattern)                     // (1)
        // A module reference in a pattern has type Foo.type, not "object Foo"
        narrowIf(checkStable(tree), sym.isModuleNotMethod)
      else if (isModuleTypedExpr)                     // (3)
        narrowIf(tree, true)
      else if (isGetClassCall)                        // (4)
        tree setType MethodType(Nil, getClassReturnType(pre))
      else
        tree
    }

    private def isNarrowable(tpe: Type): Boolean = unwrapWrapperTypes(tpe) match {
      case TypeRef(_, _, _) | RefinedType(_, _) => true
      case _                                    => !phase.erasedTypes
    }

    def stabilizeFun(tree: Tree, mode: Mode, pt: Type): Tree = {
      val sym = tree.symbol
      val pre = tree match {
        case Select(qual, _) => qual.tpe
        case _               => NoPrefix
      }
      def stabilizable = (
           pre.isStable
        && sym.tpe.params.isEmpty
        && (isStableContext(tree, mode, pt) || sym.isModule)
      )
      tree.tpe match {
        case MethodType(_, _) if stabilizable => tree setType MethodType(Nil, singleType(pre, sym)) // TODO: should this be a NullaryMethodType?
        case _                                => tree
      }
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
      val rawTypeStart = if (Statistics.canEnable) Statistics.startCounter(rawTypeFailed) else null
      val findMemberStart = if (Statistics.canEnable) Statistics.startCounter(findMemberFailed) else null
      val subtypeStart = if (Statistics.canEnable) Statistics.startCounter(subtypeFailed) else null
      val failedSilentStart = if (Statistics.canEnable) Statistics.startTimer(failedSilentNanos) else null
      def stopStats() = {
        if (Statistics.canEnable) Statistics.stopCounter(rawTypeFailed, rawTypeStart)
        if (Statistics.canEnable) Statistics.stopCounter(findMemberFailed, findMemberStart)
        if (Statistics.canEnable) Statistics.stopCounter(subtypeFailed, subtypeStart)
        if (Statistics.canEnable) Statistics.stopTimer(failedSilentNanos, failedSilentStart)
      }
      @inline def wrapResult(reporter: ContextReporter, result: T) =
        if (reporter.hasErrors) {
          stopStats()
          SilentTypeError(reporter.errors.toList, reporter.warnings.toList)
        } else SilentResultValue(result)

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

          // If we have a successful result, emit any warnings it created.
          if (!context1.reporter.hasErrors)
            context1.reporter.emitWarnings()

          wrapResult(context1.reporter, result)
        } else {
          assert(context.bufferErrors || isPastTyper, "silent mode is not available past typer")

          context.reporter.withFreshErrorBuffer {
            wrapResult(context.reporter, op(this))
          }
        }
      } catch {
        case ex: CyclicReference => throw ex
        case ex: TypeError =>
          // fallback in case TypeError is still thrown
          // @H this happens for example in cps annotation checker
          stopStats()
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
          def hasImport = inferImplicit(EmptyTree: Tree, featureTrait.tpe, reportAmbiguous = true, isView = false, context).isSuccess
          def hasOption = settings.language contains featureName
          val OK = hasImport || hasOption
          if (!OK) {
            val Some(AnnotationInfo(_, List(Literal(Constant(featureDesc: String)), Literal(Constant(required: Boolean))), _)) =
              featureTrait getAnnotation LanguageFeatureAnnot
            context.featureWarning(pos, featureName, featureDesc, featureTrait, construct, required)
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

    /**
     * Convert a SAM type to the corresponding FunctionType,
     * extrapolating BoundedWildcardTypes in the process
     * (no type precision is lost by the extrapolation,
     *  but this facilitates dealing with the types arising from Java's use-site variance).
     */
    def samToFunctionType(tp: Type, sam: Symbol = NoSymbol): Type = {
      val samSym = sam orElse samOf(tp)

      def correspondingFunctionSymbol = {
        val numVparams = samSym.info.params.length
        if (numVparams > definitions.MaxFunctionArity) NoSymbol
        else FunctionClass(numVparams)
      }

      if (samSym.exists && samSym.owner != correspondingFunctionSymbol) // don't treat Functions as SAMs
        wildcardExtrapolation(normalize(tp memberInfo samSym))
      else NoType
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
    protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
      def hasUndets           = context.undetparams.nonEmpty
      def hasUndetsInMonoMode = hasUndets && !mode.inPolyMode

      def adaptToImplicitMethod(mt: MethodType): Tree = {
        if (hasUndets) { // (9) -- should revisit dropped condition `hasUndetsInMonoMode`
          // dropped so that type args of implicit method are inferred even if polymorphic expressions are allowed
          // needed for implicits in 2.8 collection library -- maybe once #3346 is fixed, we can reinstate the condition?
            context.undetparams = inferExprInstance(tree, context.extractUndetparams(), pt,
              // approximate types that depend on arguments since dependency on implicit argument is like dependency on type parameter
              mt.approximate,
              keepNothings = false,
              useWeaklyCompatible = true) // #3808
        }

        // avoid throwing spurious DivergentImplicit errors
        if (context.reporter.hasErrors)
          setError(tree)
        else
          withCondConstrTyper(treeInfo.isSelfOrSuperConstrCall(tree))(typer1 =>
            if (original != EmptyTree && pt != WildcardType) (
              typer1 silent { tpr =>
                val withImplicitArgs = tpr.applyImplicitArgs(tree)
                if (tpr.context.reporter.hasErrors) tree // silent will wrap it in SilentTypeError anyway
                else tpr.typed(withImplicitArgs, mode, pt)
              }
              orElse { _ =>
                val resetTree = resetAttrs(original)
                resetTree match {
                  case treeInfo.Applied(fun, targs, args) =>
                    if (fun.symbol != null && fun.symbol.isError)
                      // SI-9041 Without this, we leak error symbols past the typer!
                      // because the fallback typechecking notices the error-symbol,
                      // refuses to re-attempt typechecking, and presumes that someone
                      // else was responsible for issuing the related type error!
                      fun.setSymbol(NoSymbol)
                  case _ =>
                }
                debuglog(s"fallback on implicits: ${tree}/$resetTree")
                val tree1 = typed(resetTree, mode)
                // Q: `typed` already calls `pluginsTyped` and `adapt`. the only difference here is that
                // we pass `EmptyTree` as the `original`. intended? added in 2009 (53d98e7d42) by martin.
                tree1 setType pluginsTyped(tree1.tpe, this, tree1, mode, pt)
                if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt, EmptyTree)
              }
            )
            else
              typer1.typed(typer1.applyImplicitArgs(tree), mode, pt)
          )
      }

      def instantiateToMethodType(mt: MethodType): Tree = {
        val meth = tree match {
          // a partial named application is a block (see comment in EtaExpansion)
          case Block(_, tree1) => tree1.symbol
          case _               => tree.symbol
        }
        if (!meth.isConstructor && (isFunctionType(pt) || samOf(pt).exists)) { // (4.2)
          debuglog(s"eta-expanding $tree: ${tree.tpe} to $pt")
          checkParamsConvertible(tree, tree.tpe)
          val tree0 = etaExpand(context.unit, tree, this)

          // #2624: need to infer type arguments for eta expansion of a polymorphic method
          // context.undetparams contains clones of meth.typeParams (fresh ones were generated in etaExpand)
          // need to run typer on tree0, since etaExpansion sets the tpe's of its subtrees to null
          // can't type with the expected type, as we can't recreate the setup in (3) without calling typed
          // (note that (3) does not call typed to do the polymorphic type instantiation --
          //  it is called after the tree has been typed with a polymorphic expected result type)
          if (hasUndets)
            instantiate(typed(tree0, mode), mode, pt)
          else
            typed(tree0, mode, pt)
        }
        else if (!meth.isConstructor && mt.params.isEmpty) // (4.3)
          adapt(typed(Apply(tree, Nil) setPos tree.pos), mode, pt, original)
        else if (context.implicitsEnabled)
          MissingArgsForMethodTpeError(tree, meth)
        else
          setError(tree)
      }

      def adaptType(): Tree = {
        // @M When not typing a type constructor (!context.inTypeConstructorAllowed)
        // or raw type, types must be of kind *,
        // and thus parameterized types must be applied to their type arguments
        // @M TODO: why do kind-* tree's have symbols, while higher-kinded ones don't?
        def properTypeRequired = (
             tree.hasSymbolField
          && !context.inTypeConstructorAllowed
          && !context.unit.isJava
        )
        // @M: don't check tree.tpe.symbol.typeParams. check tree.tpe.typeParams!!!
        // (e.g., m[Int] --> tree.tpe.symbol.typeParams.length == 1, tree.tpe.typeParams.length == 0!)
        // @M: removed check for tree.hasSymbolField and replace tree.symbol by tree.tpe.symbol
        // (TypeTree's must also be checked here, and they don't directly have a symbol)
        def kindArityMismatch = (
             context.inTypeConstructorAllowed
          && !sameLength(tree.tpe.typeParams, pt.typeParams)
        )
        // Note that we treat Any and Nothing as kind-polymorphic.
        // We can't perform this check when typing type arguments to an overloaded method before the overload is resolved
        // (or in the case of an error type) -- this is indicated by pt == WildcardType (see case TypeApply in typed1).
        def kindArityMismatchOk = tree.tpe.typeSymbol match {
          case NothingClass | AnyClass => true
          case _                       => pt == WildcardType
        }

        // todo. It would make sense when mode.inFunMode to instead use
        //    tree setType tree.tpe.normalize
        // when typechecking, say, TypeApply(Ident(`some abstract type symbol`), List(...))
        // because otherwise Ident will have its tpe set to a TypeRef, not to a PolyType, and `typedTypeApply` will fail
        // but this needs additional investigation, because it crashes t5228, gadts1 and maybe something else
        if (mode.inFunMode)
          tree
        else if (properTypeRequired && tree.symbol.typeParams.nonEmpty)  // (7)
          MissingTypeParametersError(tree)
        else if (kindArityMismatch && !kindArityMismatchOk)  // (7.1) @M: check kind-arity
          KindArityMismatchError(tree, pt)
        else tree match { // (6)
          case TypeTree() => tree
          case _          => TypeTree(tree.tpe) setOriginal tree
        }
      }

      def insertApply(): Tree = {
        assert(!context.inTypeConstructorAllowed, mode) //@M
        val adapted = adaptToName(tree, nme.apply)
        def stabilize0(pre: Type): Tree = stabilize(adapted, pre, MonoQualifierModes, WildcardType)

        // TODO reconcile the overlap between Typers#stablize and TreeGen.stabilize
        val qual = adapted match {
          case This(_) =>
            gen.stabilize(adapted)
          case Ident(_) =>
            val owner = adapted.symbol.owner
            val pre =
              if (owner.isPackageClass) owner.thisType
              else if (owner.isClass) context.enclosingSubClassContext(owner).prefix
              else NoPrefix
            stabilize0(pre)
          case Select(qualqual, _) =>
            stabilize0(qualqual.tpe)
          case other =>
            other
        }
        typedPos(tree.pos, mode, pt) {
          Select(qual setPos tree.pos.makeTransparent, nme.apply)
        }
      }
      def adaptConstant(value: Constant): Tree = {
        val sym = tree.symbol
        if (sym != null && sym.isDeprecated)
          context.deprecationWarning(tree.pos, sym)

        treeCopy.Literal(tree, value)
      }

      // Ignore type errors raised in later phases that are due to mismatching types with existential skolems
      // We have lift crashing in 2.9 with an adapt failure in the pattern matcher.
      // Here's my hypothesis why this happens. The pattern matcher defines a variable of type
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
      //
      // SI-6029 shows another case where we also fail (in uncurry), but this time the expected
      // type is an existential type.
      //
      // The reason for both failures have to do with the way we (don't) transform
      // skolem types along with the trees that contain them. We'd need a
      // radically different approach to do it. But before investing a lot of time to
      // to do this (I have already sunk 3 full days with in the end futile attempts
      // to consistently transform skolems and fix 6029), I'd like to
      // investigate ways to avoid skolems completely.
      //
      // upd. The same problem happens when we try to typecheck the result of macro expansion against its expected type
      // (which is the return type of the macro definition instantiated in the context of expandee):
      //
      //   Test.scala:2: error: type mismatch;
      //     found   : $u.Expr[Class[_ <: Object]]
      //     required: reflect.runtime.universe.Expr[Class[?0(in value <local Test>)]] where type ?0(in value <local Test>) <: Object
      //     scala.reflect.runtime.universe.reify(new Object().getClass)
      //                                         ^
      // Therefore following Martin's advice I use this logic to recover from skolem errors after macro expansions
      // (by adding the ` || tree.attachments.get[MacroExpansionAttachment].isDefined` clause to the conditional above).
      //
      def adaptMismatchedSkolems() = {
        def canIgnoreMismatch = (
             !context.reportErrors && isPastTyper
          || tree.hasAttachment[MacroExpansionAttachment]
        )
        def bound = pt match {
          case ExistentialType(qs, _) => qs
          case _                      => Nil
        }
        def msg = sm"""
          |Recovering from existential or skolem type error in
          |  $tree
          |with type: ${tree.tpe}
          |       pt: $pt
          |  context: ${context.tree}
          |  adapted
          """.trim

        val boundOrSkolems = if (canIgnoreMismatch) bound ++ pt.skolemsExceptMethodTypeParams else Nil
        boundOrSkolems match {
          case Nil => AdaptTypeError(tree, tree.tpe, pt) ; setError(tree)
          case _   => logResult(msg)(adapt(tree, mode, deriveTypeWithWildcards(boundOrSkolems)(pt)))
        }
      }

      def fallbackAfterVanillaAdapt(): Tree = {
        def isPopulatedPattern = {
          if ((tree.symbol ne null) && tree.symbol.isModule)
            inferModulePattern(tree, pt)

          isPopulated(tree.tpe, approximateAbstracts(pt))
        }
        if (mode.inPatternMode && isPopulatedPattern)
          return tree

        val tree1 = constfold(tree, pt) // (10) (11)
        if (tree1.tpe <:< pt)
          return adapt(tree1, mode, pt, original)

        if (mode.typingExprNotFun) {
          // The <: Any requirement inhibits attempts to adapt continuation types
          // to non-continuation types.
          if (tree.tpe <:< AnyTpe) pt.dealias match {
            case TypeRef(_, UnitClass, _) => // (12)
              if (!isPastTyper && settings.warnValueDiscard)
                context.warning(tree.pos, "discarded non-Unit value")
              return typedPos(tree.pos, mode, pt)(Block(List(tree), Literal(Constant(()))))
            case TypeRef(_, sym, _) if isNumericValueClass(sym) && isNumericSubType(tree.tpe, pt) =>
              if (!isPastTyper && settings.warnNumericWiden)
                context.warning(tree.pos, "implicit numeric widening")
              return typedPos(tree.pos, mode, pt)(Select(tree, "to" + sym.name))
            case _ =>
          }
          if (pt.dealias.annotations.nonEmpty && canAdaptAnnotations(tree, this, mode, pt)) // (13)
            return typed(adaptAnnotations(tree, this, mode, pt), mode, pt)

          if (hasUndets)
            return instantiate(tree, mode, pt)

          if (context.implicitsEnabled && !pt.isError && !tree.isErrorTyped) {
            // (14); the condition prevents chains of views
            debuglog("inferring view from " + tree.tpe + " to " + pt)
            inferView(tree, tree.tpe, pt, reportAmbiguous = true) match {
              case EmptyTree =>
              case coercion  =>
                def msg = "inferred view from " + tree.tpe + " to " + pt + " = " + coercion + ":" + coercion.tpe
                if (settings.logImplicitConv)
                  context.echo(tree.pos, msg)

                debuglog(msg)
                val silentContext = context.makeImplicit(context.ambiguousErrors)
                val res = newTyper(silentContext).typed(
                  new ApplyImplicitView(coercion, List(tree)) setPos tree.pos, mode, pt)
                silentContext.reporter.firstError match {
                  case Some(err) => context.issue(err)
                  case None      => return res
                }
            }
          }
        }

        debuglog("error tree = " + tree)
        if (settings.debug && settings.explaintypes)
          explainTypes(tree.tpe, pt)

        if (tree.tpe.isErroneous || pt.isErroneous)
          setError(tree)
        else
          adaptMismatchedSkolems()
      }

      def vanillaAdapt(tree: Tree) = {
        def applyPossible = {
          def applyMeth = member(adaptToName(tree, nme.apply), nme.apply)
          def hasPolymorphicApply = applyMeth.alternatives exists (_.tpe.typeParams.nonEmpty)
          def hasMonomorphicApply = applyMeth.alternatives exists (_.tpe.paramSectionCount > 0)

          dyna.acceptsApplyDynamic(tree.tpe) || (
            if (mode.inTappMode)
              tree.tpe.typeParams.isEmpty && hasPolymorphicApply
            else
              hasMonomorphicApply
          )
        }
        def shouldInsertApply(tree: Tree) = mode.typingExprFun && {
          tree.tpe match {
            case _: MethodType | _: OverloadedType | _: PolyType => false
            case _                                               => applyPossible
          }
        }
        if (tree.isType)
          adaptType()
        else if (mode.typingExprNotFun && treeInfo.isMacroApplication(tree) && !isMacroExpansionSuppressed(tree))
          macroExpand(this, tree, mode, pt)
        else if (mode.typingConstructorPattern)
          typedConstructorPattern(tree, pt)
        else if (shouldInsertApply(tree))
          insertApply()
        else if (hasUndetsInMonoMode) { // (9)
          assert(!context.inTypeConstructorAllowed, context) //@M
          instantiatePossiblyExpectingUnit(tree, mode, pt)
        }
        else if (tree.tpe <:< pt)
          tree
        else
          fallbackAfterVanillaAdapt()
      }

      // begin adapt
      if (isMacroImplRef(tree)) {
        if (treeInfo.isMacroApplication(tree)) adapt(unmarkMacroImplRef(tree), mode, pt, original)
        else tree
      } else tree.tpe match {
        case atp @ AnnotatedType(_, _) if canAdaptAnnotations(tree, this, mode, pt) => // (-1)
          adaptAnnotations(tree, this, mode, pt)
        case ct @ ConstantType(value) if mode.inNone(TYPEmode | FUNmode) && (ct <:< pt) && canAdaptConstantTypeToLiteral => // (0)
          adaptConstant(value)
        case OverloadedType(pre, alts) if !mode.inFunMode => // (1)
          inferExprAlternative(tree, pt)
          adaptAfterOverloadResolution(tree, mode, pt, original)
        case NullaryMethodType(restpe) => // (2)
          adapt(tree setType restpe, mode, pt, original)
        case TypeRef(_, ByNameParamClass, arg :: Nil) if mode.inExprMode => // (2)
          adapt(tree setType arg, mode, pt, original)
        case tp if mode.typingExprNotLhs && isExistentialType(tp) =>
          adapt(tree setType tp.dealias.skolemizeExistential(context.owner, tree), mode, pt, original)
        case PolyType(tparams, restpe) if mode.inNone(TAPPmode | PATTERNmode) && !context.inTypeConstructorAllowed => // (3)
          // assert((mode & HKmode) == 0) //@M a PolyType in HKmode represents an anonymous type function,
          // we're in HKmode since a higher-kinded type is expected --> hence, don't implicitly apply it to type params!
          // ticket #2197 triggered turning the assert into a guard
          // I guess this assert wasn't violated before because type aliases weren't expanded as eagerly
          //  (the only way to get a PolyType for an anonymous type function is by normalisation, which applies eta-expansion)
          // -- are we sure we want to expand aliases this early?
          // -- what caused this change in behaviour??
          val tparams1 = cloneSymbols(tparams)
          val tree1 = (
            if (tree.isType) tree
            else TypeApply(tree, tparams1 map (tparam => TypeTree(tparam.tpeHK) setPos tree.pos.focus)) setPos tree.pos
          )
          context.undetparams ++= tparams1
          notifyUndetparamsAdded(tparams1)
          adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt, original)

        case mt: MethodType if mode.typingExprNotFunNotLhs && mt.isImplicit => // (4.1)
          adaptToImplicitMethod(mt)
        case mt: MethodType if mode.typingExprNotFunNotLhs && !hasUndetsInMonoMode && !treeInfo.isMacroApplicationOrBlock(tree) =>
          instantiateToMethodType(mt)
        case _ =>
          vanillaAdapt(tree)
      }
    }

    // This just exists to help keep track of the spots where we have to adapt a tree after
    // overload resolution. These proved hard to find during the fix for SI-8267.
    def adaptAfterOverloadResolution(tree: Tree, mode: Mode, pt: Type = WildcardType, original: Tree = EmptyTree): Tree = {
      adapt(tree, mode, pt, original)
    }

    def instantiate(tree: Tree, mode: Mode, pt: Type): Tree = {
      inferExprInstance(tree, context.extractUndetparams(), pt)
      adapt(tree, mode, pt)
    }
    /** If the expected type is Unit: try instantiating type arguments
     *  with expected type Unit, but if that fails, try again with pt = WildcardType
     *  and discard the expression.
     */
    def instantiateExpectingUnit(tree: Tree, mode: Mode): Tree = {
      val savedUndetparams = context.undetparams
      silent(_.instantiate(tree, mode, UnitTpe)) orElse { _ =>
        context.undetparams = savedUndetparams
        val valueDiscard = atPos(tree.pos)(Block(List(instantiate(tree, mode, WildcardType)), Literal(Constant(()))))
        typed(valueDiscard, mode, UnitTpe)
      }
    }

    def instantiatePossiblyExpectingUnit(tree: Tree, mode: Mode, pt: Type): Tree = {
      if (mode.typingExprNotFun && pt.typeSymbol == UnitClass && !tree.tpe.isInstanceOf[MethodType])
        instantiateExpectingUnit(tree, mode)
      else
        instantiate(tree, mode, pt)
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
        qual.tpe.dealiasWiden match {
          case et: ExistentialType =>
            qual setType et.skolemizeExistential(context.owner, qual) // open the existential
          case _ =>
        }
        inferView(qual, qual.tpe, searchTemplate, reportAmbiguous, saveErrors) match {
          case EmptyTree  => qual
          case coercion   =>
            if (settings.logImplicitConv)
              context.echo(qual.pos,
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

      if (pt == WildcardType)
        doAdapt(pt)
      else silent(_ => doAdapt(pt)) filter (_ != qual) orElse (_ =>
        logResult(s"fallback on implicits in adaptToArguments: $qual.$name")(doAdapt(WildcardType))
      )
    }

    /** Try to apply an implicit conversion to `qual` so that it contains
     *  a method `name`. If that's ambiguous try taking arguments into
     *  account using `adaptToArguments`.
     */
    def adaptToMemberWithArgs(tree: Tree, qual: Tree, name: Name, mode: Mode, reportAmbiguous: Boolean, saveErrors: Boolean): Tree = {
      def onError(reportError: => Tree): Tree = context.tree match {
        case Apply(tree1, args) if (tree1 eq tree) && args.nonEmpty =>
          ( silent   (_.typedArgs(args.map(_.duplicate), mode))
              filter (xs => !(xs exists (_.isErrorTyped)))
                 map (xs => adaptToArguments(qual, name, xs, WildcardType, reportAmbiguous, saveErrors))
              orElse ( _ => reportError)
          )
        case _            =>
          reportError
      }

      silent(_.adaptToMember(qual, HasMember(name), reportAmbiguous = false)) orElse (errs =>
        onError {
          if (reportAmbiguous) errs foreach (context issue _)
          setError(tree)
        }
      )
    }

    /** Try to apply an implicit conversion to `qual` to that it contains a
     *  member `name` of arbitrary type.
     *  If no conversion is found, return `qual` unchanged.
     */
    def adaptToName(qual: Tree, name: Name) =
      if (member(qual, name) != NoSymbol) qual
      else adaptToMember(qual, HasMember(name))

    private def validateNoCaseAncestor(clazz: Symbol) = {
      if (!phase.erasedTypes) {
        for (ancestor <- clazz.ancestors find (_.isCase)) {
          context.error(clazz.pos, (
            "case %s has case ancestor %s, but case-to-case inheritance is prohibited."+
            " To overcome this limitation, use extractors to pattern match on non-leaf nodes."
          ).format(clazz, ancestor.fullName))
        }
      }
    }

    private def checkEphemeral(clazz: Symbol, body: List[Tree]) = {
      // NOTE: Code appears to be messy in this method for good reason: it clearly
      // communicates the fact that it implements rather ad-hoc, arbitrary and
      // non-regular set of rules that identify features that interact badly with
      // value classes. This code can be cleaned up a lot once implementation
      // restrictions are addressed.
      val isValueClass = !clazz.isTrait
      def where = if (isValueClass) "value class" else "universal trait extending from class Any"
      def implRestriction(tree: Tree, what: String) =
        context.error(tree.pos, s"implementation restriction: $what is not allowed in $where" +
           "\nThis restriction is planned to be removed in subsequent releases.")
      /**
       * Deeply traverses the tree in search of constructs that are not allowed
       * in value classes (at any nesting level).
       *
       * All restrictions this object imposes are probably not fundamental but require
       * fair amount of work and testing. We are conservative for now when it comes
       * to allowing language features to interact with value classes.
       *  */
      object checkEphemeralDeep extends Traverser {
        override def traverse(tree: Tree): Unit = if (isValueClass) {
          tree match {
            case _: ModuleDef =>
              //see https://issues.scala-lang.org/browse/SI-6359
              implRestriction(tree, "nested object")
            //see https://issues.scala-lang.org/browse/SI-6444
            //see https://issues.scala-lang.org/browse/SI-6463
            case cd: ClassDef if !cd.symbol.isAnonymousClass => // Don't warn about partial functions, etc. SI-7571
              implRestriction(tree, "nested class") // avoiding Type Tests that might check the $outer pointer.
            case Select(sup @ Super(qual, mix), selector) if selector != nme.CONSTRUCTOR && qual.symbol == clazz && mix != tpnme.EMPTY =>
              //see https://issues.scala-lang.org/browse/SI-6483
              implRestriction(sup, "qualified super reference")
            case _ =>
          }
          super.traverse(tree)
        }
      }
      for (stat <- body) {
        def notAllowed(what: String) = context.error(stat.pos, s"$what is not allowed in $where")
        stat match {
          // see https://issues.scala-lang.org/browse/SI-6444
          // see https://issues.scala-lang.org/browse/SI-6463
          case ClassDef(mods, _, _, _) if isValueClass =>
            implRestriction(stat, s"nested ${ if (mods.isTrait) "trait" else "class" }")
          case _: Import | _: ClassDef | _: TypeDef | EmptyTree => // OK
          case DefDef(_, name, _, _, _, rhs) =>
            if (stat.symbol.isAuxiliaryConstructor)
              notAllowed("secondary constructor")
            else if (isValueClass && (name == nme.equals_ || name == nme.hashCode_) && !stat.symbol.isSynthetic)
              notAllowed(s"redefinition of $name method. See SIP-15, criterion 4.")
            else if (stat.symbol != null && stat.symbol.isParamAccessor)
              notAllowed("additional parameter")
            checkEphemeralDeep.traverse(rhs)
          case _: ValDef =>
            notAllowed("field definition")
          case _: ModuleDef =>
            //see https://issues.scala-lang.org/browse/SI-6359
            implRestriction(stat, "nested object")
          case _ =>
            notAllowed("this statement")
        }
      }
    }

    private def validateDerivedValueClass(clazz: Symbol, body: List[Tree]) = {
      if (clazz.isTrait)
        context.error(clazz.pos, "only classes (not traits) are allowed to extend AnyVal")
      if (!clazz.isStatic)
        context.error(clazz.pos, "value class may not be a "+
          (if (clazz.owner.isTerm) "local class" else "member of another class"))
      if (!clazz.isPrimitiveValueClass) {
        clazz.primaryConstructor.paramss match {
          case List(List(param)) =>
            val decls = clazz.info.decls
            val paramAccessor = clazz.constrParamAccessors.head
            if (paramAccessor.isMutable)
              context.error(paramAccessor.pos, "value class parameter must not be a var")
            val accessor = decls.toList.find(x => x.isMethod && x.accessedOrSelf == paramAccessor)
            accessor match {
              case None =>
                context.error(paramAccessor.pos, "value class parameter must be a val and not be private[this]")
              case Some(acc) if acc.isProtectedLocal =>
                context.error(paramAccessor.pos, "value class parameter must not be protected[this]")
              case Some(acc) =>
                if (acc.tpe.typeSymbol.isDerivedValueClass)
                  context.error(acc.pos, "value class may not wrap another user-defined value class")
                checkEphemeral(clazz, body filterNot (stat => stat.symbol != null && stat.symbol.accessedOrSelf == paramAccessor))
            }
          case _ =>
            context.error(clazz.pos, "value class needs to have exactly one val parameter")
        }
      }

      for (tparam <- clazz.typeParams)
        if (tparam hasAnnotation definitions.SpecializedClass)
          context.error(tparam.pos, "type parameter of value class may not be specialized")
    }

    /** Typechecks a parent type reference.
     *
     *  This typecheck is harder than it might look, because it should honor early
     *  definitions and also perform type argument inference with the help of super call
     *  arguments provided in `encodedtpt`.
     *
     *  The method is called in batches (batch = 1 time per each parent type referenced),
     *  two batches per definition: once from namer, when entering a ClassDef or a ModuleDef
     *  and once from typer, when typechecking the definition.
     *
     *  ***Arguments***
     *
     *  `encodedtpt` represents the parent type reference wrapped in an `Apply` node
     *  which indicates value arguments (i.e. type macro arguments or super constructor call arguments)
     *  If no value arguments are provided by the user, the `Apply` node is still
     *  there, but its `args` will be set to `Nil`.
     *  This argument is synthesized by `tools.nsc.ast.Parsers.templateParents`.
     *
     *  `templ` is an enclosing template, which contains a primary constructor synthesized by the parser.
     *  Such a constructor is a DefDef which contains early initializers and maybe a super constructor call
     *  (I wrote "maybe" because trait constructors don't call super constructors).
     *  This argument is synthesized by `tools.nsc.ast.Trees.Template`.
     *
     *  `inMixinPosition` indicates whether the reference is not the first in the
     *  list of parents (and therefore cannot be a class) or the opposite.
     *
     *  ***Return value and side effects***
     *
     *  Returns a `TypeTree` representing a resolved parent type.
     *  If the typechecked parent reference implies non-nullary and non-empty argument list,
     *  this argument list is attached to the returned value in SuperArgsAttachment.
     *  The attachment is necessary for the subsequent typecheck to fixup a super constructor call
     *  in the body of the primary constructor (see `typedTemplate` for details).
     *
     *  This method might invoke `typedPrimaryConstrBody`, hence it might cause the side effects
     *  described in the docs of that method. It might also attribute the Super(_, _) reference
     *  (if present) inside the primary constructor of `templ`.
     *
     *  ***Example***
     *
     *  For the following definition:
     *
     *    class D extends {
     *      val x = 2
     *      val y = 4
     *    } with B(x)(3) with C(y) with T
     *
     *  this method will be called six times:
     *
     *    (3 times from the namer)
     *    typedParentType(Apply(Apply(Ident(B), List(Ident(x))), List(3)), templ, inMixinPosition = false)
     *    typedParentType(Apply(Ident(C), List(Ident(y))), templ, inMixinPosition = true)
     *    typedParentType(Apply(Ident(T), List()), templ, inMixinPosition = true)
     *
     *    (3 times from the typer)
     *    <the same three calls>
     */
    private def typedParentType(encodedtpt: Tree, templ: Template, inMixinPosition: Boolean): Tree = {
      val app = treeInfo.dissectApplied(encodedtpt)
      val (treeInfo.Applied(core, _, argss), decodedtpt) = ((app, app.callee))
      val argssAreTrivial = argss == Nil || argss == ListOfNil

      // we cannot avoid cyclic references with `initialize` here, because when type macros arrive,
      // we'll have to check the probe for isTypeMacro anyways.
      // therefore I think it's reasonable to trade a more specific "inherits itself" error
      // for a generic, yet understandable "cyclic reference" error
      var probe = typedTypeConstructor(core.duplicate).tpe.typeSymbol
      if (probe == null) probe = NoSymbol
      probe.initialize

      if (probe.isTrait || inMixinPosition) {
        if (!argssAreTrivial) {
          if (probe.isTrait) ConstrArgsInParentWhichIsTraitError(encodedtpt, probe)
          else () // a class in a mixin position - this warrants an error in `validateParentClasses`
                  // therefore here we do nothing, e.g. don't check that the # of ctor arguments
                  // matches the # of ctor parameters or stuff like that
        }
        typedType(decodedtpt)
      } else {
        val supertpt = typedTypeConstructor(decodedtpt)
        val supertparams = if (supertpt.hasSymbolField) supertpt.symbol.typeParams else Nil
        def inferParentTypeArgs: Tree = {
          typedPrimaryConstrBody(templ) {
            val supertpe = PolyType(supertparams, appliedType(supertpt.tpe, supertparams map (_.tpeHK)))
            val supercall = New(supertpe, mmap(argss)(_.duplicate))
            val treeInfo.Applied(Select(ctor, nme.CONSTRUCTOR), _, _) = supercall
            ctor setType supertpe // this is an essential hack, otherwise it will occasionally fail to typecheck
            atPos(supertpt.pos.focus)(supercall)
          } match {
            case EmptyTree => MissingTypeArgumentsParentTpeError(supertpt); supertpt
            case tpt       => TypeTree(tpt.tpe) setPos supertpt.pos  // SI-7224: don't .focus positions of the TypeTree of a parent that exists in source
          }
        }

        val supertptWithTargs = if (supertparams.isEmpty || context.unit.isJava) supertpt else inferParentTypeArgs

        // this is the place where we tell the typer what argss should be used for the super call
        // if argss are nullary or empty, then (see the docs for `typedPrimaryConstrBody`)
        // the super call dummy is already good enough, so we don't need to do anything
        if (argssAreTrivial) supertptWithTargs else supertptWithTargs updateAttachment SuperArgsAttachment(argss)
      }
    }

    /** Typechecks the mishmash of trees that happen to be stuffed into the primary constructor of a given template.
     *  Before commencing the typecheck, replaces the `pendingSuperCall` dummy with the result of `actualSuperCall`.
     *  `actualSuperCall` can return `EmptyTree`, in which case the dummy is replaced with a literal unit.
     *
     *  ***Return value and side effects***
     *
     *  If a super call is present in the primary constructor and is not erased by the transform, returns it typechecked.
     *  Otherwise (e.g. if the primary constructor is missing or the super call isn't there) returns `EmptyTree`.
     *
     *  As a side effect, this method attributes the underlying fields of early vals.
     *  Early vals aren't typechecked anywhere else, so it's essential to call `typedPrimaryConstrBody`
     *  at least once per definition. It'd be great to disentangle this logic at some point.
     *
     *  ***Example***
     *
     *  For the following definition:
     *
     *    class D extends {
     *      val x = 2
     *      val y = 4
     *    } with B(x)(3) with C(y) with T
     *
     *  the primary constructor of `templ` will be:
     *
     *    Block(List(
     *      ValDef(NoMods, x, TypeTree(), 2)
     *      ValDef(NoMods, y, TypeTree(), 4)
     *      global.pendingSuperCall,
     *      Literal(Constant(())))
     *
     *  Note the `pendingSuperCall` part. This is the representation of a fill-me-in-later supercall dummy,
     *  which encodes the fact that supercall argss are unknown during parsing and need to be transplanted
     *  from one of the parent types. Read more about why the argss are unknown in `tools.nsc.ast.Trees.Template`.
     */
    private def typedPrimaryConstrBody(templ: Template)(actualSuperCall: => Tree): Tree =
        treeInfo.firstConstructor(templ.body) match {
        case ctor @ DefDef(_, _, _, vparamss, _, cbody @ Block(cstats, cunit)) =>
            val (preSuperStats, superCall) = {
              val (stats, rest) = cstats span (x => !treeInfo.isSuperConstrCall(x))
              (stats map (_.duplicate), if (rest.isEmpty) EmptyTree else rest.head.duplicate)
            }
          val superCall1 = (superCall match {
            case global.pendingSuperCall => actualSuperCall
            case EmptyTree => EmptyTree
          }) orElse cunit
          val cbody1 = treeCopy.Block(cbody, preSuperStats, superCall1)
          val clazz = context.owner
            assert(clazz != NoSymbol, templ)
          // SI-9086 The position of this symbol is material: implicit search will avoid triggering
          //         cyclic errors in an implicit search in argument to the super constructor call on
          //         account of the "ignore symbols without complete info that succeed the implicit search"
          //         in this source file. See `ImplicitSearch#isValid` and `ImplicitInfo#isCyclicOrErroneous`.
          val dummy = context.outer.owner.newLocalDummy(context.owner.pos)
          val cscope = context.outer.makeNewScope(ctor, dummy)
          if (dummy.isTopLevel) currentRun.symSource(dummy) = currentUnit.source.file
          val cbody2 = { // called both during completion AND typing.
            val typer1 = newTyper(cscope)
            // XXX: see about using the class's symbol....
            clazz.unsafeTypeParams foreach (sym => typer1.context.scope.enter(sym))
            typer1.namer.enterValueParams(vparamss map (_.map(_.duplicate)))
            typer1.typed(cbody1)
            }

            val preSuperVals = treeInfo.preSuperFields(templ.body)
            if (preSuperVals.isEmpty && preSuperStats.nonEmpty)
            devWarning("Wanted to zip empty presuper val list with " + preSuperStats)
            else
            map2(preSuperStats, preSuperVals)((ldef, gdef) => gdef.tpt setType ldef.symbol.tpe)

          if (superCall1 == cunit) EmptyTree
          else cbody2 match {
            case Block(_, expr) => expr
            case tree => tree
          }
          case _ =>
          EmptyTree
        }

    /** Makes sure that the first type tree in the list of parent types is always a class.
     *  If the first parent is a trait, prepend its supertype to the list until it's a class.
     */
    private def normalizeFirstParent(parents: List[Tree]): List[Tree] = {
      @annotation.tailrec
      def explode0(parents: List[Tree]): List[Tree] = {
        val supertpt :: rest = parents // parents is always non-empty here - it only grows
        if (supertpt.tpe.typeSymbol == AnyClass) {
          supertpt setType AnyRefTpe
          parents
        } else if (treeInfo isTraitRef supertpt) {
          val supertpt1  = typedType(supertpt)
          def supersuper = TypeTree(supertpt1.tpe.firstParent) setPos supertpt.pos.focus
          if (supertpt1.isErrorTyped) rest
          else explode0(supersuper :: supertpt1 :: rest)
        } else parents
      }

      def explode(parents: List[Tree]) =
        if (treeInfo isTraitRef parents.head) explode0(parents)
        else parents

      if (parents.isEmpty) Nil else explode(parents)
    }

    /** Certain parents are added in the parser before it is known whether
     *  that class also declared them as parents. For instance, this is an
     *  error unless we take corrective action here:
     *
     *    case class Foo() extends Serializable
     *
     *  So we strip the duplicates before typer.
     */
    private def fixDuplicateSyntheticParents(parents: List[Tree]): List[Tree] = parents match {
      case Nil      => Nil
      case x :: xs  =>
        val sym = x.symbol
        x :: fixDuplicateSyntheticParents(
          if (isPossibleSyntheticParent(sym)) xs filterNot (_.symbol == sym)
          else xs
        )
    }

    def typedParentTypes(templ: Template): List[Tree] = templ.parents match {
      case Nil => List(atPos(templ.pos)(TypeTree(AnyRefTpe)))
      case first :: rest =>
        try {
          val supertpts = fixDuplicateSyntheticParents(normalizeFirstParent(
            typedParentType(first, templ, inMixinPosition = false) +:
            (rest map (typedParentType(_, templ, inMixinPosition = true)))))

          // if that is required to infer the targs of a super call
          // typedParentType calls typedPrimaryConstrBody to do the inferring typecheck
          // as a side effect, that typecheck also assigns types to the fields underlying early vals
          // however if inference is not required, the typecheck doesn't happen
          // and therefore early fields have their type trees not assigned
          // here we detect this situation and take preventive measures
          if (treeInfo.hasUntypedPreSuperFields(templ.body))
            typedPrimaryConstrBody(templ)(EmptyTree)

          supertpts mapConserve (tpt => checkNoEscaping.privates(context.owner, tpt))
        }
        catch {
          case ex: TypeError =>
            // fallback in case of cyclic errors
            // @H none of the tests enter here but I couldn't rule it out
            // upd. @E when a definition inherits itself, we end up here
            // because `typedParentType` triggers `initialize` for parent types symbols
            log("Type error calculating parents in template " + templ)
            log("Error: " + ex)
            ParentTypesError(templ, ex)
            List(TypeTree(AnyRefTpe))
        }
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
      def validateDynamicParent(parent: Symbol, parentPos: Position) =
        if (parent == DynamicClass) checkFeature(parentPos, DynamicsFeature)

      def validateParentClass(parent: Tree, superclazz: Symbol) =
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

          val sameSourceFile = context.unit.source.file == psym.sourceFile

          if (!isPastTyper && psym.hasDeprecatedInheritanceAnnotation &&
            !sameSourceFile && !context.owner.ownerChain.exists(x => x.isDeprecated || x.hasBridgeAnnotation)) {
            val suffix = psym.deprecatedInheritanceMessage map (": " + _) getOrElse ""
            val msg = s"inheritance from ${psym.fullLocationString} is deprecated$suffix"
            context.deprecationWarning(parent.pos, psym, msg)
          }

          if (psym.isSealed && !phase.erasedTypes)
            if (sameSourceFile)
              psym addChild context.owner
            else
              pending += ParentSealedInheritanceError(parent, psym)
          val parentTypeOfThis = parent.tpe.dealias.typeOfThis

          if (!(selfType <:< parentTypeOfThis) &&
              !phase.erasedTypes &&
              !context.owner.isSynthetic &&   // don't check synthetic concrete classes for virtuals (part of DEVIRTUALIZE)
              !selfType.isErroneous &&
              !parent.tpe.isErroneous)
          {
            pending += ParentSelfTypeConformanceError(parent, selfType)
            if (settings.explaintypes) explainTypes(selfType, parentTypeOfThis)
          }

          if (parents exists (p => p != parent && p.tpe.typeSymbol == psym && !psym.isError))
            pending += ParentInheritedTwiceError(parent, psym)

          validateDynamicParent(psym, parent.pos)
        }

      if (!parents.isEmpty && parents.forall(!_.isErrorTyped)) {
        val superclazz = parents.head.tpe.typeSymbol
        for (p <- parents) validateParentClass(p, superclazz)
      }

      pending.foreach(ErrorUtils.issueTypeError)
    }

    def checkFinitary(classinfo: ClassInfoType) {
      val clazz = classinfo.typeSymbol

      for (tparam <- clazz.typeParams) {
        if (classinfo.expansiveRefs(tparam) contains tparam) {
          val newinfo = ClassInfoType(
            classinfo.parents map (_.instantiateTypeParams(List(tparam), List(AnyRefTpe))),
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

    def typedClassDef(cdef: ClassDef): Tree = {
      val clazz = cdef.symbol
      val typedMods = typedModifiers(cdef.mods)
      assert(clazz != NoSymbol, cdef)
      reenterTypeParams(cdef.tparams)
      val tparams1 = cdef.tparams mapConserve (typedTypeDef)
      val impl1 = newTyper(context.make(cdef.impl, clazz, newScope)).typedTemplate(cdef.impl, typedParentTypes(cdef.impl))
      val impl2 = finishMethodSynthesis(impl1, clazz, context)
      if (clazz.isTrait && clazz.info.parents.nonEmpty && clazz.info.firstParent.typeSymbol == AnyClass)
        checkEphemeral(clazz, impl2.body)

      if ((clazz isNonBottomSubClass ClassfileAnnotationClass) && (clazz != ClassfileAnnotationClass)) {
        if (!clazz.owner.isPackageClass)
          context.error(clazz.pos, "inner classes cannot be classfile annotations")
        // Ignore @SerialVersionUID, because it is special-cased and handled completely differently.
        // It only extends ClassfileAnnotationClass instead of StaticAnnotation to get the enforcement
        // of constant argument values "for free". Related to SI-7041.
        else if (clazz != SerialVersionUIDAttr) restrictionWarning(cdef.pos, unit,
          """|subclassing Classfile does not
             |make your annotation visible at runtime.  If that is what
             |you want, you must write the annotation class in Java.""".stripMargin)
      }

      warnTypeParameterShadow(tparams1, clazz)

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
      val impl1 = newTyper(context.make(mdef.impl, clazz, newScope)).typedTemplate(mdef.impl, {
        typedParentTypes(mdef.impl) ++ (
          if (noSerializable) Nil
          else {
            clazz.makeSerializable()
            List(TypeTree(SerializableTpe) setPos clazz.pos.focus)
          }
        )
      })

      val impl2  = finishMethodSynthesis(impl1, clazz, context)

      if (settings.isScala211  && mdef.symbol == PredefModule)
        ensurePredefParentsAreInSameSourceFile(impl2)

      treeCopy.ModuleDef(mdef, typedMods, mdef.name, impl2) setType NoType
    }

    private def ensurePredefParentsAreInSameSourceFile(template: Template) = {
      val parentSyms = template.parents map (_.symbol) filterNot (_ == AnyRefClass)
      if (parentSyms exists (_.associatedFile != PredefModule.associatedFile))
        context.error(template.pos, s"All parents of Predef must be defined in ${PredefModule.associatedFile}.")
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
      if (txt eq context) namer enterSym tree
      else newNamer(txt) enterSym tree

    /** <!-- 2 --> Check that inner classes do not inherit from Annotation
     */
    def typedTemplate(templ0: Template, parents1: List[Tree]): Template = {
      val templ = templ0
      // please FIXME: uncommenting this line breaks everything
      // val templ = treeCopy.Template(templ0, templ0.body, templ0.self, templ0.parents)
      val clazz = context.owner
      clazz.annotations.map(_.completeInfo())
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
      val body1 = pluginsEnterStats(this, templ.body)
      enterSyms(context.outer.make(templ, clazz, clazz.info.decls), body1)
      if (!templ.isErrorTyped) // if `parentTypes` has invalidated the template, don't validate it anymore
      validateParentClasses(parents1, selfType)
      if (clazz.isCase)
        validateNoCaseAncestor(clazz)
      if (clazz.isTrait && hasSuperArgs(parents1.head))
        ConstrArgsInParentOfTraitError(parents1.head, clazz)

      if ((clazz isSubClass ClassfileAnnotationClass) && !clazz.isTopLevel)
        context.error(clazz.pos, "inner classes cannot be classfile annotations")

      if (!phase.erasedTypes && !clazz.info.resultType.isError) // @S: prevent crash for duplicated type members
        checkFinitary(clazz.info.resultType.asInstanceOf[ClassInfoType])

      val body2 = {
        val body2 =
          if (isPastTyper || reporter.hasErrors) body1
          else body1 flatMap rewrappingWrapperTrees(namer.addDerivedTrees(Typer.this, _))
        val primaryCtor = treeInfo.firstConstructor(body2)
        val primaryCtor1 = primaryCtor match {
          case DefDef(_, _, _, _, _, Block(earlyVals :+ global.pendingSuperCall, unit)) =>
            val argss = superArgs(parents1.head) getOrElse Nil
            val pos = wrappingPos(parents1.head.pos, primaryCtor :: argss.flatten).makeTransparent
            val superCall = atPos(pos)(PrimarySuperCall(argss))
            deriveDefDef(primaryCtor)(block => Block(earlyVals :+ superCall, unit) setPos pos) setPos pos
          case _ => primaryCtor
        }
        body2 mapConserve { case `primaryCtor` => primaryCtor1; case stat => stat }
      }

      val body3 = typedStats(body2, templ.symbol)

      if (clazz.info.firstParent.typeSymbol == AnyValClass)
        validateDerivedValueClass(clazz, body3)

      if (clazz.isTrait) {
        for (decl <- clazz.info.decls if decl.isTerm && decl.isEarlyInitialized) {
          context.warning(decl.pos, "Implementation restriction: early definitions in traits are not initialized before the super class is initialized.")
        }
      }

      treeCopy.Template(templ, parents1, self1, body3) setType clazz.tpe_*
    }

    /** Remove definition annotations from modifiers (they have been saved
     *  into the symbol's `annotations` in the type completer / namer)
     *
     *  However reification does need annotation definitions to proceed.
     *  Unfortunately, AnnotationInfo doesn't provide enough info to reify it in general case.
     *  The biggest problem is with the "atp: Type" field, which cannot be reified in some situations
     *  that involve locally defined annotations. See more about that in Reifiers.scala.
     *
     *  That's why the original tree gets saved into `original` field of AnnotationInfo (happens elsewhere).
     *  The field doesn't get pickled/unpickled and exists only during a single compilation run.
     *  This simultaneously allows us to reify annotations and to preserve backward compatibility.
     */
    def typedModifiers(mods: Modifiers): Modifiers =
      mods.copy(annotations = Nil) setPositions mods.positions

    def typedValDef(vdef: ValDef): ValDef = {
      val sym = vdef.symbol
      val valDefTyper = {
        val maybeConstrCtx =
          if ((sym.isParameter || sym.isEarlyInitialized) && sym.owner.isConstructor) context.makeConstructorContext
          else context
        newTyper(maybeConstrCtx.makeNewScope(vdef, sym))
      }
      valDefTyper.typedValDefImpl(vdef)
    }

    // use typedValDef instead. this version is called after creating a new context for the ValDef
    private def typedValDefImpl(vdef: ValDef) = {
      val sym = vdef.symbol.initialize
      val typedMods = typedModifiers(vdef.mods)

      sym.annotations.map(_.completeInfo())
      val tpt1 = checkNoEscaping.privates(sym, typedType(vdef.tpt))
      checkNonCyclic(vdef, tpt1)

      if (sym.hasAnnotation(definitions.VolatileAttr) && !sym.isMutable)
        VolatileValueError(vdef)

      val rhs1 =
        if (vdef.rhs.isEmpty) {
          if (sym.isVariable && sym.owner.isTerm && !sym.isLazy && !isPastTyper)
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
          transformedOrTyped(vdef.rhs, EXPRmode | BYVALmode, tpt2)
        }
      treeCopy.ValDef(vdef, typedMods, vdef.name, tpt1, checkDead(rhs1)) setType NoType
    }

    /** Enter all aliases of local parameter accessors.
     */
    def computeParamAliases(clazz: Symbol, vparamss: List[List[ValDef]], rhs: Tree) {
      debuglog(s"computing param aliases for $clazz:${clazz.primaryConstructor.tpe}:$rhs")
      val pending = ListBuffer[AbsTypeError]()

      // !!! This method is redundant with other, less buggy ones.
      def decompose(call: Tree): (Tree, List[Tree]) = call match {
        case _ if call.isErrorTyped => // e.g. SI-7636
          (call, Nil)
        case Apply(fn, args) =>
          // an object cannot be allowed to pass a reference to itself to a superconstructor
          // because of initialization issues; SI-473, SI-3913, SI-6928.
          foreachSubTreeBoundTo(args, clazz) { tree =>
            if (tree.symbol.isModule)
              pending += SuperConstrReferenceError(tree)
            tree match {
              case This(qual) =>
                pending += SuperConstrArgsThisReferenceError(tree)
              case _ => ()
            }
          }
          val (superConstr, preArgs) = decompose(fn)
          val params = fn.tpe.params
          // appending a dummy tree to represent Nil for an empty varargs (is this really necessary?)
          val applyArgs = if (args.length < params.length) args :+ EmptyTree else args take params.length

          assert(sameLength(applyArgs, params) || call.isErrorTyped,
            s"arity mismatch but call is not error typed: $clazz (params=$params, args=$applyArgs)")

          (superConstr, preArgs ::: applyArgs)
        case Block(_ :+ superCall, _) =>
          decompose(superCall)
        case _ =>
          (call, Nil)
      }
      val (superConstr, superArgs) = decompose(rhs)
      assert(superConstr.symbol ne null, superConstr)//debug
      def superClazz = superConstr.symbol.owner
      def superParamAccessors = superClazz.constrParamAccessors

      // associate superclass paramaccessors with their aliases
      if (superConstr.symbol.isPrimaryConstructor && !superClazz.isJavaDefined && sameLength(superParamAccessors, superArgs)) {
        for ((superAcc, superArg @ Ident(name)) <- superParamAccessors zip superArgs) {
          if (mexists(vparamss)(_.symbol == superArg.symbol)) {
            val alias = (
              superAcc.initialize.alias
                orElse (superAcc getterIn superAcc.owner)
                filter (alias => superClazz.info.nonPrivateMember(alias.name) == alias)
            )
            if (alias.exists && !alias.accessed.isVariable && !isRepeatedParamType(alias.accessed.info)) {
              val ownAcc = clazz.info decl name suchThat (_.isParamAccessor) match {
                case acc if !acc.isDeferred && acc.hasAccessorFlag => acc.accessed
                case acc                                           => acc
              }
              ownAcc match {
                case acc: TermSymbol if !acc.isVariable && !isByNameParamType(acc.info) =>
                  debuglog(s"$acc has alias ${alias.fullLocationString}")
                  acc setAlias alias
                case _ =>
              }
            }
          }
        }
      }
      pending.foreach(ErrorUtils.issueTypeError)
    }

    // Check for SI-4842.
    private def checkSelfConstructorArgs(ddef: DefDef, clazz: Symbol) {
      val pending = ListBuffer[AbsTypeError]()
      ddef.rhs match {
        case Block(stats, expr) =>
          val selfConstructorCall = stats.headOption.getOrElse(expr)
          foreachSubTreeBoundTo(List(selfConstructorCall), clazz) {
            case tree @ This(qual) =>
              pending += SelfConstrArgsThisReferenceError(tree)
            case _ => ()
          }
        case _ =>
      }
      pending.foreach(ErrorUtils.issueTypeError)
    }

    /**
     * Run the provided function for each sub tree of `trees` that
     * are bound to a symbol with `clazz` as a base class.
     *
     * @param f This function can assume that `tree.symbol` is non null
     */
    private def foreachSubTreeBoundTo[A](trees: List[Tree], clazz: Symbol)(f: Tree => Unit): Unit =
      for {
        tree <- trees
        subTree <- tree
      } {
        val sym = subTree.symbol
        if (sym != null && sym.info.baseClasses.contains(clazz))
          f(subTree)
      }

      /** Check if a structurally defined method violates implementation restrictions.
     *  A method cannot be called if it is a non-private member of a refinement type
     *  and if its parameter's types are any of:
     *    - the self-type of the refinement
     *    - a type member of the refinement
     *    - an abstract type declared outside of the refinement.
     *    - an instance of a value class
     *  Furthermore, the result type may not be a value class either
     */
    def checkMethodStructuralCompatible(ddef: DefDef): Unit = {
      val meth = ddef.symbol
      def parentString = meth.owner.parentSymbols filterNot (_ == ObjectClass) match {
        case Nil => ""
        case xs  => xs.map(_.nameString).mkString(" (of ", " with ", ")")
      }
      def fail(pos: Position, msg: String): Boolean = {
        context.error(pos, msg)
        false
      }
      /* Have to examine all parameters in all lists.
       */
      def paramssTypes(tp: Type): List[List[Type]] = tp match {
        case mt @ MethodType(_, restpe) => mt.paramTypes :: paramssTypes(restpe)
        case PolyType(_, restpe)        => paramssTypes(restpe)
        case _                          => Nil
      }
      def resultType = meth.tpe_*.finalResultType
      def nthParamPos(n1: Int, n2: Int) =
        try ddef.vparamss(n1)(n2).pos catch { case _: IndexOutOfBoundsException => meth.pos }

      def failStruct(pos: Position, what: String, where: String = "Parameter type") =
        fail(pos, s"$where in structural refinement may not refer to $what")

      foreachWithIndex(paramssTypes(meth.tpe)) { (paramList, listIdx) =>
        foreachWithIndex(paramList) { (paramType, paramIdx) =>
          val sym = paramType.typeSymbol
          def paramPos = nthParamPos(listIdx, paramIdx)

          /* Not enough to look for abstract types; have to recursively check the bounds
           * of each abstract type for more abstract types. Almost certainly there are other
           * exploitable type soundness bugs which can be seen by bounding a type parameter
           * by an abstract type which itself is bounded by an abstract type.
           */
          def checkAbstract(tp0: Type, what: String): Boolean = {
            def check(sym: Symbol): Boolean = !sym.isAbstractType || {
              log(s"""checking $tp0 in refinement$parentString at ${meth.owner.owner.fullLocationString}""")
              (    (!sym.hasTransOwner(meth.owner) && failStruct(paramPos, "an abstract type defined outside that refinement", what))
                || (!sym.hasTransOwner(meth) && failStruct(paramPos, "a type member of that refinement", what))
                || checkAbstract(sym.info.bounds.hi, "Type bound")
              )
            }
            tp0.dealiasWidenChain forall (t => check(t.typeSymbol))
          }
          checkAbstract(paramType, "Parameter type")

          if (sym.isDerivedValueClass)
            failStruct(paramPos, "a user-defined value class")
          if (paramType.isInstanceOf[ThisType] && sym == meth.owner)
            failStruct(paramPos, "the type of that refinement (self type)")
        }
      }
      if (resultType.typeSymbol.isDerivedValueClass)
        failStruct(ddef.tpt.pos, "a user-defined value class", where = "Result type")
    }

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

      warnTypeParameterShadow(tparams1, meth)

      meth.annotations.map(_.completeInfo())

      for (vparams1 <- vparamss1; vparam1 <- vparams1 dropRight 1)
        if (isRepeatedParamType(vparam1.symbol.tpe))
          StarParamNotLastError(vparam1)

      val tpt1 = checkNoEscaping.privates(meth, typedType(ddef.tpt))
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
        } else if (meth.isMacro) {
          // typechecking macro bodies is sort of unconventional
          // that's why we employ our custom typing scheme orchestrated outside of the typer
          transformedOr(ddef.rhs, typedMacroBody(this, ddef))
        } else {
          transformedOrTyped(ddef.rhs, EXPRmode, tpt1.tpe)
        }

      if (meth.isClassConstructor && !isPastTyper && !meth.owner.isSubClass(AnyValClass)) {
        // At this point in AnyVal there is no supercall, which will blow up
        // in computeParamAliases; there's nothing to be computed for Anyval anyway.
        if (meth.isPrimaryConstructor)
          computeParamAliases(meth.owner, vparamss1, rhs1)
        else
          checkSelfConstructorArgs(ddef, meth.owner)
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
        if (meth.isStructuralRefinementMember)
          checkMethodStructuralCompatible(ddef)

        if (meth.isImplicit && !meth.isSynthetic) meth.info.paramss match {
          case List(param) :: _ if !param.isImplicit =>
            checkFeature(ddef.pos, ImplicitConversionsFeature, meth.toString)
          case _ =>
        }
      }

      treeCopy.DefDef(ddef, typedMods, ddef.name, tparams1, vparamss1, tpt1, rhs1) setType NoType
    }

    def typedTypeDef(tdef: TypeDef): TypeDef =
      typerWithCondLocalContext(context.makeNewScope(tdef, tdef.symbol))(tdef.tparams.nonEmpty) {
        _.typedTypeDefImpl(tdef)
      }

    // use typedTypeDef instead. this version is called after creating a new context for the TypeDef
    private def typedTypeDefImpl(tdef: TypeDef): TypeDef = {
      tdef.symbol.initialize
      reenterTypeParams(tdef.tparams)
      val tparams1 = tdef.tparams mapConserve typedTypeDef
      val typedMods = typedModifiers(tdef.mods)
      tdef.symbol.annotations.map(_.completeInfo())

      warnTypeParameterShadow(tparams1, tdef.symbol)

      // @specialized should not be pickled when compiling with -no-specialize
      if (settings.nospecialization && currentRun.compiles(tdef.symbol)) {
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
              context.owner.newLabel(ldef.name, ldef.pos) setInfo MethodType(List(), UnitTpe))
        case _ =>
      }
    }

    def typedLabelDef(ldef: LabelDef): LabelDef = {
      if (!nme.isLoopHeaderLabel(ldef.symbol.name) || isPastTyper) {
        val restpe = ldef.symbol.tpe.resultType
        val rhs1 = typed(ldef.rhs, restpe)
        ldef.params foreach (param => param setType param.symbol.tpe)
        deriveLabelDef(ldef)(_ => rhs1) setType restpe
      }
      else {
        val initpe = ldef.symbol.tpe.resultType
        val rhs1 = typed(ldef.rhs)
        val restpe = rhs1.tpe
        if (restpe == initpe) { // stable result, no need to check again
          ldef.params foreach (param => param setType param.symbol.tpe)
          treeCopy.LabelDef(ldef, ldef.name, ldef.params, rhs1) setType restpe
        } else {
          context.scope.unlink(ldef.symbol)
          val sym2 = namer.enterInScope(
            context.owner.newLabel(ldef.name, ldef.pos) setInfo MethodType(List(), restpe))
          val LabelDef(_, _, rhs1) = resetAttrs(ldef)
          val rhs2 = typed(brutallyResetAttrs(rhs1), restpe)
          ldef.params foreach (param => param setType param.symbol.tpe)
          deriveLabelDef(ldef)(_ => rhs2) setSymbol sym2 setType restpe
        }
      }
    }

    def typedBlock(block0: Block, mode: Mode, pt: Type): Block = {
      val syntheticPrivates = new ListBuffer[Symbol]
      try {
        namer.enterSyms(block0.stats)
        val block = treeCopy.Block(block0, pluginsEnterStats(this, block0.stats), block0.expr)
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
        val stats1 = if (isPastTyper) block.stats else
          block.stats.flatMap(stat => stat match {
            case vd@ValDef(_, _, _, _) if vd.symbol.isLazy =>
              namer.addDerivedTrees(Typer.this, vd)
            case _ => stat::Nil
            })
        val stats2 = typedStats(stats1, context.owner)
        val expr1 = typed(block.expr, mode &~ (FUNmode | QUALmode), pt)
        treeCopy.Block(block, stats2, expr1)
          .setType(if (treeInfo.isExprSafeToInline(block)) expr1.tpe else expr1.tpe.deconst)
      } finally {
        // enable escaping privates checking from the outside and recycle
        // transient flag
        syntheticPrivates foreach (_ resetFlag SYNTHETIC_PRIVATE)
      }
    }

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
        pat1 modifyType (_.finalResultType)

      for (bind @ Bind(name, _) <- cdef.pat) {
        val sym = bind.symbol
        if (name.toTermName != nme.WILDCARD && sym != null) {
          if (sym == NoSymbol) {
            if (context.scope.lookup(name) == NoSymbol)
              namer.enterInScope(context.owner.newErrorSymbol(name))
          } else
            namer.enterIfNotThere(sym)
        }
      }

      val guard1: Tree = if (cdef.guard == EmptyTree) EmptyTree
                         else typed(cdef.guard, BooleanTpe)
      var body1: Tree = typed(cdef.body, pt)

      if (context.enclosingCaseDef.savedTypeBounds.nonEmpty) {
        body1 modifyType context.enclosingCaseDef.restoreTypeBounds
        // insert a cast if something typechecked under the GADT constraints,
        // but not in real life (i.e., now that's we've reset the method's type skolems'
        //   infos back to their pre-GADT-constraint state)
        if (isFullyDefined(pt) && !(body1.tpe <:< pt)) {
          log(s"Adding cast to pattern because ${body1.tpe} does not conform to expected type $pt")
          body1 = typedPos(body1.pos)(gen.mkCast(body1, pt.dealiasWiden))
        }
      }

//    body1 = checkNoEscaping.locals(context.scope, pt, body1)
      treeCopy.CaseDef(cdef, pat1, guard1, body1) setType body1.tpe
    }

    def typedCases(cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef] =
      cases mapConserve { cdef =>
        newTyper(context.makeNewScope(cdef, context.owner)).typedCase(cdef, pattp, pt)
      }

    def adaptCase(cdef: CaseDef, mode: Mode, tpe: Type): CaseDef = deriveCaseDef(cdef)(adapt(_, mode, tpe))

    def packedTypes(trees: List[Tree]): List[Type] = trees map (c => packedType(c, context.owner).deconst)

    // takes untyped sub-trees of a match and type checks them
    def typedMatch(selector: Tree, cases: List[CaseDef], mode: Mode, pt: Type, tree: Tree = EmptyTree): Match = {
      val selector1  = checkDead(typedByValueExpr(selector))
      val selectorTp = packCaptured(selector1.tpe.widen).skolemizeExistential(context.owner, selector)
      val casesTyped = typedCases(cases, selectorTp, pt)

      def finish(cases: List[CaseDef], matchType: Type) =
        treeCopy.Match(tree, selector1, cases) setType matchType

      if (isFullyDefined(pt))
        finish(casesTyped, pt)
      else packedTypes(casesTyped) match {
        case packed if sameWeakLubAsLub(packed) => finish(casesTyped, lub(packed))
        case packed                             =>
          val lub = weakLub(packed)
          finish(casesTyped map (adaptCase(_, mode, lub)), lub)
      }
    }

    // match has been typed -- virtualize it during type checking so the full context is available
    def virtualizedMatch(match_ : Match, mode: Mode, pt: Type) = {
      import patmat.{ vpmName, PureMatchTranslator }

      // TODO: add fallback __match sentinel to predef
      val matchStrategy: Tree =
        if (!(settings.Xexperimental && context.isNameInScope(vpmName._match))) null    // fast path, avoiding the next line if there's no __match to be seen
        else newTyper(context.makeImplicit(reportAmbiguousErrors = false)).silent(_.typed(Ident(vpmName._match)), reportAmbiguousErrors = false) orElse (_ => null)

      if (matchStrategy ne null) // virtualize
        typed((new PureMatchTranslator(this.asInstanceOf[patmat.global.analyzer.Typer] /*TODO*/, matchStrategy)).translateMatch(match_), mode, pt)
      else
        match_ // will be translated in phase `patmat`
    }

    /** synthesize and type check a PartialFunction implementation based on the match in `tree`
     *
     *  `param => sel match { cases }` becomes:
     *
     *  new AbstractPartialFunction[$argTp, $matchResTp] {
     *    def applyOrElse[A1 <: $argTp, B1 >: $matchResTp]($param: A1, default: A1 => B1): B1 =
     *       $selector match { $cases }
     *    def isDefinedAt(x: $argTp): Boolean =
     *       $selector match { $casesTrue }
     *  }
     *
     * TODO: it would be nicer to generate the tree specified above at once and type it as a whole,
     * there are two gotchas:
     *    - matchResTp may not be known until we've typed the match (can only use resTp when it's fully defined),
     *       - if we typed the match in isolation first, you'd know its result type, but would have to re-jig the owner structure
     *       - could we use a type variable for matchResTp and backpatch it?
     *    - occurrences of `this` in `cases` or `sel` must resolve to the this of the class originally enclosing the match,
     *      not of the anonymous partial function subclass
     *
     * an alternative TODO: add partial function AST node or equivalent and get rid of this synthesis --> do everything in uncurry (or later)
     * however, note that pattern matching codegen is designed to run *before* uncurry
     */
    def synthesizePartialFunction(paramName: TermName, paramPos: Position, paramSynthetic: Boolean,
                                  tree: Tree, mode: Mode, pt: Type): Tree = {
      assert(pt.typeSymbol == PartialFunctionClass, s"PartialFunction synthesis for match in $tree requires PartialFunction expected type, but got $pt.")
      val targs = pt.dealiasWiden.typeArgs

      // if targs.head isn't fully defined, we can't translate --> error
      targs match {
        case argTp :: _ if isFullyDefined(argTp) => // ok
        case _ => // uh-oh
          MissingParameterTypeAnonMatchError(tree, pt)
          return setError(tree)
      }

      // NOTE: resTp still might not be fully defined
      val argTp :: resTp :: Nil = targs

      // targs must conform to Any for us to synthesize an applyOrElse (fallback to apply otherwise -- typically for @cps annotated targs)
      val targsValidParams = targs forall (_ <:< AnyTpe)

      val anonClass = context.owner newAnonymousFunctionClass tree.pos addAnnotation SerialVersionUIDAnnotation

      import CODE._

      val Match(sel, cases) = tree

      // need to duplicate the cases before typing them to generate the apply method, or the symbols will be all messed up
      val casesTrue = cases map (c => deriveCaseDef(c)(x => atPos(x.pos.focus)(TRUE)).duplicate.asInstanceOf[CaseDef])

      // must generate a new tree every time
      def selector(paramSym: Symbol): Tree = gen.mkUnchecked(
        if (sel != EmptyTree) sel.duplicate
        else atPos(tree.pos.focusStart)(
          // SI-6925: subsume type of the selector to `argTp`
          // we don't want/need the match to see the `A1` type that we must use for variance reasons in the method signature
          //
          // this failed: replace `selector` by `Typed(selector, TypeTree(argTp))` -- as it's an upcast, this should never fail,
          //   `(x: A1): A` doesn't always type check, even though `A1 <: A`, due to singleton types (test/files/pos/t4269.scala)
          // hence the cast, which will be erased in posterasure
          // (the cast originally caused  extremely weird types to show up
          //  in test/scaladoc/run/SI-5933.scala because `variantToSkolem` was missing `tpSym.initialize`)
          gen.mkCastPreservingAnnotations(Ident(paramSym), argTp)
        ))

      def mkParam(methodSym: Symbol, tp: Type = argTp) =
        methodSym.newValueParameter(paramName, paramPos.focus, SYNTHETIC) setInfo tp

      def mkDefaultCase(body: Tree) =
        atPos(tree.pos.makeTransparent) {
          CaseDef(Bind(nme.DEFAULT_CASE, Ident(nme.WILDCARD)), body)
        }

      // `def applyOrElse[A1 <: $argTp, B1 >: $matchResTp](x: A1, default: A1 => B1): B1 =
      //  ${`$selector match { $cases; case default$ => default(x) }`
      def applyOrElseMethodDef = {
        val methodSym = anonClass.newMethod(nme.applyOrElse, tree.pos, FINAL | OVERRIDE)

        // create the parameter that corresponds to the function's parameter
        val A1 = methodSym newTypeParameter (newTypeName("A1")) setInfo TypeBounds.upper(argTp)
        val x = mkParam(methodSym, A1.tpe)

        // applyOrElse's default parameter:
        val B1 = methodSym newTypeParameter (newTypeName("B1")) setInfo TypeBounds.empty
        val default = methodSym newValueParameter (newTermName("default"), tree.pos.focus, SYNTHETIC) setInfo functionType(List(A1.tpe), B1.tpe)

        val paramSyms = List(x, default)
        methodSym setInfo genPolyType(List(A1, B1), MethodType(paramSyms, B1.tpe))

        val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym))
        if (!paramSynthetic) methodBodyTyper.context.scope enter x

        // First, type without the default case; only the cases provided
        // by the user are typed. The LUB of these becomes `B`, the lower
        // bound of `B1`, which in turn is the result type of the default
        // case
        val match0 = methodBodyTyper.typedMatch(selector(x), cases, mode, resTp)
        val matchResTp = match0.tpe

        B1 setInfo TypeBounds.lower(matchResTp) // patch info

        // the default uses applyOrElse's first parameter since the scrut's type has been widened
        val match_ = {
          val defaultCase = methodBodyTyper.typedCase(
            mkDefaultCase(methodBodyTyper.typed1(REF(default) APPLY (REF(x)), mode, B1.tpe).setType(B1.tpe)), argTp, B1.tpe)
          treeCopy.Match(match0, match0.selector, match0.cases :+ defaultCase)
        }
        match_ setType B1.tpe

        // SI-6187 Do you really want to know? Okay, here's what's going on here.
        //
        //         Well behaved trees satisfy the property:
        //
        //         typed(tree) == typed(resetAttrs(typed(tree))
        //
        //         Trees constructed without low-level symbol manipulation get this for free;
        //         references to local symbols are cleared by `ResetAttrs`, but bind to the
        //         corresponding symbol in the re-typechecked tree. But PartialFunction synthesis
        //         doesn't play by these rules.
        //
        //         During typechecking of method bodies, references to method type parameter from
        //         the declared types of the value parameters should bind to a fresh set of skolems,
        //         which have been entered into scope by `Namer#methodSig`. A comment therein:
        //
        //         "since the skolemized tparams are in scope, the TypeRefs in vparamSymss refer to skolemized tparams"
        //
        //         But, if we retypecheck the reset `applyOrElse`, the TypeTree of the `default`
        //         parameter contains no type. Somehow (where?!) it recovers a type that is _almost_ okay:
        //         `A1 => B1`. But it should really be `A1&0 => B1&0`. In the test, run/t6187.scala, this
        //         difference results in a type error, as `default.apply(x)` types as `B1`, which doesn't
        //         conform to the required `B1&0`
        //
        //         I see three courses of action.
        //
        //         1) synthesize a `asInstanceOf[B1]` below (I tried this first. But... ewwww.)
        //         2) install an 'original' TypeTree that will used after ResetAttrs (the solution below)
        //         3) Figure out how the almost-correct type is recovered on re-typechecking, and
        //            substitute in the skolems.
        //
        //         For 2.11, we'll probably shift this transformation back a phase or two, so macros
        //         won't be affected. But in any case, we should satisfy retypecheckability.
        //
        val originals: Map[Symbol, Tree] = {
          def typedIdent(sym: Symbol) = methodBodyTyper.typedType(Ident(sym), mode)
          val A1Tpt = typedIdent(A1)
          val B1Tpt = typedIdent(B1)
          Map(
            x -> A1Tpt,
            default -> gen.scalaFunctionConstr(List(A1Tpt), B1Tpt)
          )
        }
        def newParam(param: Symbol): ValDef = {
          val vd              = ValDef(param, EmptyTree)
          val tt @ TypeTree() = vd.tpt
          tt setOriginal (originals(param) setPos param.pos.focus)
          vd
        }

        val rhs    = methodBodyTyper.virtualizedMatch(match_, mode, B1.tpe)
        val defdef = newDefDef(methodSym, rhs)(vparamss = mapParamss(methodSym)(newParam), tpt = TypeTree(B1.tpe))

        (defdef, matchResTp)
      }

      // `def isDefinedAt(x: $argTp): Boolean = ${`$selector match { $casesTrue; case default$ => false } }`
      def isDefinedAtMethod = {
        val methodSym = anonClass.newMethod(nme.isDefinedAt, tree.pos.makeTransparent, FINAL)
        val paramSym = mkParam(methodSym)

        val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym)) // should use the DefDef for the context's tree, but it doesn't exist yet (we need the typer we're creating to create it)
        if (!paramSynthetic) methodBodyTyper.context.scope enter paramSym
        methodSym setInfo MethodType(List(paramSym), BooleanTpe)

        val defaultCase = mkDefaultCase(FALSE)
        val match_ = methodBodyTyper.typedMatch(selector(paramSym), casesTrue :+ defaultCase, mode, BooleanTpe)

        DefDef(methodSym, methodBodyTyper.virtualizedMatch(match_, mode, BooleanTpe))
      }

      // only used for @cps annotated partial functions
      // `def apply(x: $argTp): $matchResTp = $selector match { $cases }`
      def applyMethod = {
        val methodSym = anonClass.newMethod(nme.apply, tree.pos, FINAL | OVERRIDE)
        val paramSym = mkParam(methodSym)

        methodSym setInfo MethodType(List(paramSym), AnyTpe)

        val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym))
        if (!paramSynthetic) methodBodyTyper.context.scope enter paramSym

        val match_ = methodBodyTyper.typedMatch(selector(paramSym), cases, mode, resTp)

        val matchResTp = match_.tpe
        methodSym setInfo MethodType(List(paramSym), matchResTp) // patch info

        (DefDef(methodSym, methodBodyTyper.virtualizedMatch(match_, mode, matchResTp)), matchResTp)
      }

      def parents(resTp: Type) = addSerializable(appliedType(AbstractPartialFunctionClass.typeConstructor, List(argTp, resTp)))

      val members = {
        val (applyMeth, matchResTp) = {
          // rig the show so we can get started typing the method body -- later we'll correct the infos...
          // targs were type arguments for PartialFunction, so we know they will work for AbstractPartialFunction as well
          anonClass setInfo ClassInfoType(parents(resTp), newScope, anonClass)

          // somehow @cps annotations upset the typer when looking at applyOrElse's signature, but not apply's
          // TODO: figure out the details (T @cps[U] is not a subtype of Any, but then why does it work for the apply method?)
          if (targsValidParams) applyOrElseMethodDef
          else applyMethod
        }

        // patch info to the class's definitive info
        anonClass setInfo ClassInfoType(parents(matchResTp), newScope, anonClass)
        List(applyMeth, isDefinedAtMethod)
      }

      members foreach (m => anonClass.info.decls enter m.symbol)

      val typedBlock = typedPos(tree.pos, mode, pt) {
        Block(ClassDef(anonClass, NoMods, ListOfNil, members, tree.pos.focus), atPos(tree.pos.focus)(
          Apply(Select(New(Ident(anonClass.name).setSymbol(anonClass)), nme.CONSTRUCTOR), List())
        ))
      }

      if (typedBlock.isErrorTyped) typedBlock
      else // Don't leak implementation details into the type, see SI-6575
        typedPos(tree.pos, mode, pt) {
          Typed(typedBlock, TypeTree(typedBlock.tpe baseType PartialFunctionClass))
        }
    }

    /** Synthesize and type check the implementation of a type with a Single Abstract Method
     *
     *  `{ (p1: T1, ..., pN: TN) => body } : S`
     *
     *  expands to (where `S` is the expected type that defines a single abstract method named `apply`)
     *
     *  `{
     *    def apply$body(p1: T1, ..., pN: TN): T = body
     *    new S {
     *     def apply(p1: T1', ..., pN: TN'): T' = apply$body(p1,..., pN)
     *    }
     *  }`
     *
     * If 'T' is not fully defined, it is inferred by type checking
     * `apply$body` without a result type before type checking the block.
     * The method's inferred result type is used instead of `T`. [See test/files/pos/sammy_poly.scala]
     *
     * The `apply` method is identified by the argument `sam`; `S` corresponds to the argument `samClassTp`,
     * and `resPt` is derived from `samClassTp` -- it may be fully defined, or not...
     * If it is not fully defined, we derive `samClassTpFullyDefined` by inferring any unknown type parameters.
     *
     * The types T1' ... TN' and T' are derived from the method signature of the sam method,
     * as seen from the fully defined `samClassTpFullyDefined`.
     *
     * The function's body is put in a method outside of the class definition to enforce scoping.
     * S's members should not be in scope in `body`.
     *
     * The restriction on implicit arguments (neither S's constructor, nor sam may take an implicit argument list),
     * is largely to keep the implementation of type inference (the computation of `samClassTpFullyDefined`) simple.
     *
     * NOTE: it would be nicer to not have to type check `apply$body` separately when `T` is not fully defined.
     * However T must be fully defined before we type the instantiation, as it'll end up as a parent type,
     * which must be fully defined. Would be nice to have some kind of mechanism to insert type vars in a block of code,
     * and have the instantiation of the first occurrence propagate to the rest of the block.
     *
     * TODO: by-name params
     *  scala> trait LazySink { def accept(a: => Any): Unit }
     *  defined trait LazySink
     *
     *  scala> val f: LazySink = (a) => (a, a)
     *  f: LazySink = $anonfun$1@1fb26910
     *
     *  scala> f(println("!"))
     *  <console>:10: error: LazySink does not take parameters
     *                f(println("!"))
     *                 ^
     *
     *  scala> f.accept(println("!"))
     *  !
     *  !
     */
    def synthesizeSAMFunction(sam: Symbol, fun: Function, resPt: Type, samClassTp: Type, mode: Mode): Tree = {
      // assert(fun.vparams forall (vp => isFullyDefined(vp.tpt.tpe))) -- by construction, as we take them from sam's info
      val sampos = fun.pos

      // if the expected sam type is fully defined, use it for the method's result type
      // otherwise, NoType, so that type inference will determine the method's result type
      // resPt is syntactically contained in samClassTp, so if the latter is fully defined, so is the former
      // ultimately, we want to fully define samClassTp as it is used as the superclass of our anonymous class
      val samDefTp = if (isFullyDefined(resPt)) resPt else NoType
      val bodyName = newTermName(sam.name + "$body")

      // `def '${sam.name}\$body'($p1: $T1, ..., $pN: $TN): $resPt = $body`
      val samBodyDef =
        DefDef(NoMods,
          bodyName,
          Nil,
          List(fun.vparams.map(_.duplicate)), // must duplicate as we're also using them for `samDef`
          TypeTree(samDefTp) setPos sampos.focus,
          fun.body)

      // If we need to enter the sym for the body def before type checking the block,
      // we'll create a nested context, as explained below.
      var nestedTyper = this

      // Type check body def before classdef to fully determine samClassTp (if necessary).
      // As `samClassTp` determines a parent type for the class,
      // we can't type check `block` in one go unless `samClassTp` is fully defined.
      val samClassTpFullyDefined =
        if (isFullyDefined(samClassTp)) samClassTp
        else try {
          // This creates a symbol for samBodyDef with a type completer that'll be triggered immediately below.
          // The symbol is entered in the same scope used for the block below, and won't thus be reentered later.
          // It has to be a new scope, though, or we'll "get ambiguous reference to overloaded definition" [pos/sammy_twice.scala]
          // makeSilent: [pos/nonlocal-unchecked.scala -- when translation all functions to sams]
          val nestedCtx = enterSym(context.makeNewScope(context.tree, context.owner).makeSilent(), samBodyDef)
          nestedTyper = newTyper(nestedCtx)

          // NOTE: this `samBodyDef.symbol.info` runs the type completer set up by the enterSym above
          val actualSamType = samBodyDef.symbol.info

          // we're trying to fully define the type arguments for this type constructor
          val samTyCon  = samClassTp.typeSymbol.typeConstructor

          // the unknowns
          val tparams   = samClassTp.typeSymbol.typeParams
          // ... as typevars
          val tvars     = tparams map freshVar

          // 1. Recover partial information:
          //   - derive a type from samClassTp that has the corresponding tparams for type arguments that aren't fully defined
          //   - constrain typevars to be equal to type args that are fully defined
          val samClassTpMoreDefined = appliedType(samTyCon,
            (samClassTp.typeArgs, tparams, tvars).zipped map {
              case (a, _, tv) if isFullyDefined(a) => tv =:= a; a
              case (_, p, _)                       => p.typeConstructor
            })

          // the method type we're expecting the synthesized sam to have, based on the expected sam type,
          // where fully defined type args to samClassTp have been preserved,
          // with the unknown args replaced by their corresponding type param
          val expectedSamType = samClassTpMoreDefined.memberInfo(sam)

          // 2. make sure the body def's actual type (formals and result) conforms to
          //    sam's expected type (in terms of the typevars that represent the sam's class's type params)
          actualSamType <:< expectedSamType.substituteTypes(tparams, tvars)

          // solve constraints tracked by tvars
          val targs = solvedTypes(tvars, tparams, tparams map varianceInType(sam.info), upper = false, lubDepth(sam.info :: Nil))

          debuglog(s"sam infer: $samClassTp --> ${appliedType(samTyCon, targs)} by $actualSamType <:< $expectedSamType --> $targs for $tparams")

          // a fully defined samClassTp
          appliedType(samTyCon, targs)
        } catch {
          case _: NoInstance | _: TypeError =>
            devWarning(sampos, s"Could not define type $samClassTp using ${samBodyDef.symbol.rawInfo} <:< ${samClassTp memberInfo sam} (for $sam)")
            samClassTp
        }

      // what's the signature of the method that we should actually be overriding?
      val samMethTp = samClassTpFullyDefined memberInfo sam
      // Before the mutation, `tp <:< vpar.tpt.tpe` should hold.
      // TODO: error message when this is not the case, as the expansion won't type check
      //  - Ti' <:< Ti and T <: T' must hold for the samDef body to type check
      val funArgTps = foreach2(samMethTp.paramTypes, fun.vparams)((tp, vpar) => vpar.tpt setType tp)

      // `final override def ${sam.name}($p1: $T1', ..., $pN: $TN'): ${samMethTp.finalResultType} = ${sam.name}\$body'($p1, ..., $pN)`
      val samDef =
        DefDef(Modifiers(FINAL | OVERRIDE | SYNTHETIC),
          sam.name.toTermName,
          Nil,
          List(fun.vparams),
          TypeTree(samMethTp.finalResultType) setPos sampos.focus,
          Apply(Ident(bodyName), fun.vparams map gen.paramToArg)
        )

      val serializableParentAddendum =
        if (typeIsSubTypeOfSerializable(samClassTp)) Nil
        else List(TypeTree(SerializableTpe))

      val classDef =
        ClassDef(Modifiers(FINAL), tpnme.ANON_FUN_NAME, tparams = Nil,
          gen.mkTemplate(
            parents    = TypeTree(samClassTpFullyDefined) :: serializableParentAddendum,
            self       = noSelfType,
            constrMods = NoMods,
            vparamss   = ListOfNil,
            body       = List(samDef),
            superPos   = sampos.focus
          )
        )

      // type checking the whole block, so that everything is packaged together nicely
      // and we don't have to create any symbols by hand
      val block =
        nestedTyper.typedPos(sampos, mode, samClassTpFullyDefined) {
          Block(
            samBodyDef,
            classDef,
            Apply(Select(New(Ident(tpnme.ANON_FUN_NAME)), nme.CONSTRUCTOR), Nil)
          )
        }

      // TODO: improve error reporting -- when we're in silent mode (from `silent(_.doTypedApply(tree, fun, args, mode, pt)) orElse onError`)
      // the errors in the function don't get out...
      if (block exists (_.isErroneous))
        context.error(fun.pos, s"Could not derive subclass of $samClassTp\n (with SAM `def $sam$samMethTp`)\n based on: $fun.")

      classDef.symbol addAnnotation SerialVersionUIDAnnotation
      block
    }

    /** Type check a function literal.
     *
     * Based on the expected type pt, potentially synthesize an instance of
     *   - PartialFunction,
     *   - a type with a Single Abstract Method (under -Xexperimental for now).
     */
    private def typedFunction(fun: Function, mode: Mode, pt: Type): Tree = {
      val numVparams = fun.vparams.length
      val FunctionSymbol =
        if (numVparams > definitions.MaxFunctionArity) NoSymbol
        else FunctionClass(numVparams)

      /* The Single Abstract Member of pt, unless pt is the built-in function type of the expected arity,
       * as `(a => a): Int => Int` should not (yet) get the sam treatment.
       */
      val sam =
        if (pt.typeSymbol == FunctionSymbol) NoSymbol
        else samOf(pt)

      /* The SAM case comes first so that this works:
       *   abstract class MyFun extends (Int => Int)
       *   (a => a): MyFun
       *
       * Note that the arity of the sam must correspond to the arity of the function.
       */
      val samViable = sam.exists && sameLength(sam.info.params, fun.vparams)
      val ptNorm = if (samViable) samToFunctionType(pt, sam) else pt
      val (argpts, respt) =
        ptNorm baseType FunctionSymbol match {
          case TypeRef(_, FunctionSymbol, args :+ res) => (args, res)
          case _                                       => (fun.vparams map (_ => if (pt == ErrorType) ErrorType else NoType), WildcardType)
        }

      if (!FunctionSymbol.exists)
        MaxFunctionArityError(fun)
      else if (argpts.lengthCompare(numVparams) != 0)
        WrongNumberOfParametersError(fun, argpts)
      else {
        var issuedMissingParameterTypeError = false
        foreach2(fun.vparams, argpts) { (vparam, argpt) =>
          if (vparam.tpt.isEmpty) {
            val vparamType =
              if (isFullyDefined(argpt)) argpt
              else {
                fun match {
                  case etaExpansion(vparams, fn, args) =>
                    silent(_.typed(fn, mode.forFunMode, pt)) filter (_ => context.undetparams.isEmpty) map { fn1 =>
                        // if context.undetparams is not empty, the function was polymorphic,
                        // so we need the missing arguments to infer its type. See #871
                        //println("typing eta "+fun+":"+fn1.tpe+"/"+context.undetparams)
                        val ftpe = normalize(fn1.tpe) baseType FunctionClass(numVparams)
                        if (isFunctionType(ftpe) && isFullyDefined(ftpe))
                          return typedFunction(fun, mode, ftpe)
                    }
                  case _ =>
                }
                MissingParameterTypeError(fun, vparam, pt, withTupleAddendum = !issuedMissingParameterTypeError)
                issuedMissingParameterTypeError = true
                ErrorType
              }
            vparam.tpt.setType(vparamType)
            if (!vparam.tpt.pos.isDefined) vparam.tpt setPos vparam.pos.focus
          }
        }

        fun.body match {
          // translate `x => x match { <cases> }` : PartialFunction to
          // `new PartialFunction { def applyOrElse(x, default) = x match { <cases> } def isDefinedAt(x) = ... }`
          case Match(sel, cases) if (sel ne EmptyTree) && (pt.typeSymbol == PartialFunctionClass) =>
            // go to outer context -- must discard the context that was created for the Function since we're discarding the function
            // thus, its symbol, which serves as the current context.owner, is not the right owner
            // you won't know you're using the wrong owner until lambda lift crashes (unless you know better than to use the wrong owner)
            val outerTyper = newTyper(context.outer)
            val p = fun.vparams.head
            if (p.tpt.tpe == null) p.tpt setType outerTyper.typedType(p.tpt).tpe

            outerTyper.synthesizePartialFunction(p.name, p.pos, paramSynthetic = false, fun.body, mode, pt)

          // Use synthesizeSAMFunction to expand `(p1: T1, ..., pN: TN) => body`
          // to an instance of the corresponding anonymous subclass of `pt`.
          case _ if samViable =>
            newTyper(context.outer).synthesizeSAMFunction(sam, fun, respt, pt, mode)

          // regular Function
          case _ =>
            val vparamSyms = fun.vparams map { vparam =>
              enterSym(context, vparam)
              if (context.retyping) context.scope enter vparam.symbol
              vparam.symbol
            }
            val vparams = fun.vparams mapConserve typedValDef
            val formals = vparamSyms map (_.tpe)
            val body1 = typed(fun.body, respt)
            val restpe = packedType(body1, fun.symbol).deconst.resultType
            val funtpe  = appliedType(FunctionSymbol, formals :+ restpe: _*)

            treeCopy.Function(fun, vparams, body1) setType funtpe
        }
      }
    }

    def typedRefinement(templ: Template) {
      val stats = templ.body
      namer.enterSyms(stats)

      // need to delay rest of typedRefinement to avoid cyclic reference errors
      unit.toCheck += { () =>
        val stats1 = typedStats(stats, NoSymbol)
        // this code kicks in only after typer, so `stats` will never be filled in time
        // as a result, most of compound type trees with non-empty stats will fail to reify
        // todo. investigate whether something can be done about this
        val att = templ.attachments.get[CompoundTypeTreeOriginalAttachment].getOrElse(CompoundTypeTreeOriginalAttachment(Nil, Nil))
        templ.removeAttachment[CompoundTypeTreeOriginalAttachment]
        templ updateAttachment att.copy(stats = stats1)
        for (stat <- stats1 if stat.isDef && stat.symbol.isOverridingSymbol)
          stat.symbol setFlag OVERRIDE
      }
    }

    def typedImport(imp : Import) : Import = (transformed remove imp) match {
      case Some(imp1: Import) => imp1
      case _                  => log("unhandled import: "+imp+" in "+unit); imp
    }

    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val inBlock = exprOwner == context.owner
      def includesTargetPos(tree: Tree) =
        tree.pos.isRange && context.unit.exists && (tree.pos includes context.unit.targetPos)
      val localTarget = stats exists includesTargetPos
      def typedStat(stat: Tree): Tree = {
        if (context.owner.isRefinementClass && !treeInfo.isDeclarationOrTypeDef(stat))
          OnlyDeclarationsError(stat)
        else
          stat match {
            case imp @ Import(_, _) =>
              imp.symbol.initialize
              if (!imp.symbol.isError) {
                context = context.make(imp)
                typedImport(imp)
              } else EmptyTree
            case _ =>
              if (localTarget && !includesTargetPos(stat)) {
                // skip typechecking of statements in a sequence where some other statement includes
                // the targetposition
                stat
              } else {
                val localTyper = if (inBlock || (stat.isDef && !stat.isInstanceOf[LabelDef])) {
                  this
                } else newTyper(context.make(stat, exprOwner))
                // XXX this creates a spurious dead code warning if an exception is thrown
                // in a constructor, even if it is the only thing in the constructor.
                val result = checkDead(localTyper.typedByValueExpr(stat))

                if (treeInfo.isSelfOrSuperConstrCall(result)) {
                  context.inConstructorSuffix = true
                  if (treeInfo.isSelfConstrCall(result) && result.symbol.pos.pointOrElse(0) >= exprOwner.enclMethod.pos.pointOrElse(0))
                    ConstructorsOrderError(stat)
                }

                if (!isPastTyper && treeInfo.isPureExprForWarningPurposes(result)) context.warning(stat.pos,
                  "a pure expression does nothing in statement position; " +
                  "you may be omitting necessary parentheses"
                )
                result
              }
          }
      }

      /* 'accessor' and 'accessed' are so similar it becomes very difficult to
       * follow the logic, so I renamed one to something distinct.
       */
      def accesses(looker: Symbol, accessed: Symbol) = accessed.isLocalToThis && (
           (accessed.isParamAccessor)
        || (looker.hasAccessorFlag && !accessed.hasAccessorFlag && accessed.isPrivate)
      )

      def checkNoDoubleDefs: Unit = {
        val scope = if (inBlock) context.scope else context.owner.info.decls
        var e = scope.elems
        while ((e ne null) && e.owner == scope) {
          var e1 = scope.lookupNextEntry(e)
          while ((e1 ne null) && e1.owner == scope) {
            if (!accesses(e.sym, e1.sym) && !accesses(e1.sym, e.sym) &&
                (e.sym.isType || inBlock || (e.sym.tpe matches e1.sym.tpe)))
              // default getters are defined twice when multiple overloads have defaults. an
              // error for this is issued in RefChecks.checkDefaultsInOverloaded
              if (!e.sym.isErroneous && !e1.sym.isErroneous && !e.sym.hasDefault &&
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
          // SI-5877 The decls of a package include decls of the package object. But we don't want to add
          //         the corresponding synthetics to the package class, only to the package object class.
          def shouldAdd(sym: Symbol) =
            inBlock || !context.isInPackageObject(sym, context.owner)
          for (sym <- scope)
            for (tree <- context.unit.synthetics get sym if shouldAdd(sym)) { // OPT: shouldAdd is usually true. Call it here, rather than in the outer loop
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
            // synt is default arg for stat
            case (DefDef(_, statName, _, _, _, _), DefDef(mods, syntName, _, _, _, _)) =>
              mods.hasDefault && syntName.toString.startsWith(statName.toString)

            // synt is companion module
            case (ClassDef(_, className, _, _), ModuleDef(_, moduleName, _)) =>
              className.toTermName == moduleName

            // synt is implicit def for implicit class (#6278)
            case (ClassDef(cmods, cname, _, _), DefDef(dmods, dname, _, _, _, _)) =>
              cmods.isImplicit && dmods.isImplicit && cname.toTermName == dname

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

      val stats1 = stats mapConserve typedStat
      if (phase.erasedTypes) stats1
      else {
        // As packages are open, it doesn't make sense to check double definitions here. Furthermore,
        // it is expensive if the package is large. Instead, such double defininitions are checked in `Namers.enterInScope`
        if (!context.owner.isPackageClass)
          checkNoDoubleDefs
        addSynthetics(stats1)
      }
    }

    def typedArg(arg: Tree, mode: Mode, newmode: Mode, pt: Type): Tree = {
      val typedMode = mode.onlySticky | newmode
      val t = withCondConstrTyper(mode.inSccMode)(_.typed(arg, typedMode, pt))
      checkDead.inMode(typedMode, t)
    }

    def typedArgs(args: List[Tree], mode: Mode) =
      args mapConserve (arg => typedArg(arg, mode, NOmode, WildcardType))

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

    def doTypedApply(tree: Tree, fun0: Tree, args: List[Tree], mode: Mode, pt: Type): Tree = {
      // TODO_NMT: check the assumption that args nonEmpty
      def duplErrTree = setError(treeCopy.Apply(tree, fun0, args))
      def duplErrorTree(err: AbsTypeError) = { context.issue(err); duplErrTree }

      def preSelectOverloaded(fun: Tree): Tree = {
        if (fun.hasSymbolField && fun.symbol.isOverloaded) {
          // remove alternatives with wrong number of parameters without looking at types.
          // less expensive than including them in inferMethodAlternative (see below).
          def shapeType(arg: Tree): Type = arg match {
            case Function(vparams, body) =>
              functionType(vparams map (_ => AnyTpe), shapeType(body))
            case AssignOrNamedArg(Ident(name), rhs) =>
              NamedType(name, shapeType(rhs))
            case _ =>
              NothingTpe
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
            isApplicableSafe(context.undetparams, followApply(pre memberType alt), argtypes, pt)
          }
          if (sym.isOverloaded) {
              // eliminate functions that would result from tupling transforms
              // keeps alternatives with repeated params
            val sym1 = sym filter (alt =>
                 isApplicableBasedOnArity(pre memberType alt, argtypes.length, varargsStar = false, tuplingAllowed = false)
              || alt.tpe.params.exists(_.hasDefault)
            )
            if (sym1 != NoSymbol) sym = sym1
          }
          if (sym == NoSymbol) fun
          else adaptAfterOverloadResolution(fun setSymbol sym setType pre.memberType(sym), mode.forFunMode)
        } else fun
      }

      val fun = preSelectOverloaded(fun0)

      fun.tpe match {
        case OverloadedType(pre, alts) =>
          def handleOverloaded = {
            val undetparams = context.undetparams
            val (args1, argTpes) = context.savingUndeterminedTypeParams() {
              val amode = forArgMode(fun, mode)
              def typedArg0(tree: Tree) = typedArg(tree, amode, BYVALmode, WildcardType)
              args.map {
                case arg @ AssignOrNamedArg(Ident(name), rhs) =>
                  // named args: only type the righthand sides ("unknown identifier" errors otherwise)
                  // the assign is untyped; that's ok because we call doTypedApply
                  val typedRhs        = typedArg0(rhs)
                  val argWithTypedRhs = treeCopy.AssignOrNamedArg(arg, arg.lhs, typedRhs)

                  // TODO: SI-8197/SI-4592: check whether this named argument could be interpreted as an assign
                  // infer.checkNames must not use UnitType: it may not be a valid assignment, or the setter may return another type from Unit
                  //
                  // var typedAsAssign = true
                  // val argTyped = silent(_.typedArg(argWithTypedRhs, amode, BYVALmode, WildcardType)) orElse { errors =>
                  //   typedAsAssign = false
                  //   argWithTypedRhs
                  // }
                  //
                  // TODO: add an assignmentType field to NamedType, equal to:
                  // assignmentType = if (typedAsAssign) argTyped.tpe else NoType

                  (argWithTypedRhs, NamedType(name, typedRhs.tpe.deconst))
                case arg @ treeInfo.WildcardStarArg(repeated) =>
                  val arg1 = typedArg0(arg)
                  (arg1, RepeatedType(arg1.tpe.deconst))
                case arg =>
                  val arg1 = typedArg0(arg)
                  (arg1, arg1.tpe.deconst)
              }.unzip
            }
            if (context.reporter.hasErrors)
              setError(tree)
            else {
              inferMethodAlternative(fun, undetparams, argTpes, pt)
              doTypedApply(tree, adaptAfterOverloadResolution(fun, mode.forFunMode, WildcardType), args1, mode, pt)
            }
          }
          handleOverloaded

        case _ if isPolymorphicSignature(fun.symbol) =>
          // Mimic's Java's treatment of polymorphic signatures as described in
          // https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.12.3
          //
          // One can think of these methods as being infinitely overloaded. We create
          // a fictitious new cloned method symbol for each call site that takes on a signature
          // governed by a) the argument types and b) the expected type
          val args1 = typedArgs(args, forArgMode(fun, mode))
          val pts = args1.map(_.tpe.deconst)
          val clone = fun.symbol.cloneSymbol
          val cloneParams = pts map (pt => clone.newValueParameter(currentUnit.freshTermName()).setInfo(pt))
          val resultType = if (isFullyDefined(pt)) pt else ObjectTpe
          clone.modifyInfo(mt => copyMethodType(mt, cloneParams, resultType))
          val fun1 = fun.setSymbol(clone).setType(clone.info)
          doTypedApply(tree, fun1, args1, mode, resultType).setType(resultType)

        case mt @ MethodType(params, _) =>
          val paramTypes = mt.paramTypes
          // repeat vararg as often as needed, remove by-name
          val argslen = args.length
          val formals = formalTypes(paramTypes, argslen)

          /* Try packing all arguments into a Tuple and apply `fun`
           * to that. This is the last thing which is tried (after
           * default arguments)
           */
          def tryTupleApply: Tree = {
            if (eligibleForTupleConversion(paramTypes, argslen) && !phase.erasedTypes) {
              val tupleArgs = List(atPos(tree.pos.makeTransparent)(gen.mkTuple(args)))
              // expected one argument, but got 0 or >1 ==>  try applying to tuple
              // the inner "doTypedApply" does "extractUndetparams" => restore when it fails
              val savedUndetparams = context.undetparams
              silent(_.doTypedApply(tree, fun, tupleArgs, mode, pt)) map { t =>
                // Depending on user options, may warn or error here if
                // a Unit or tuple was inserted.
                val keepTree = (
                     !mode.typingExprNotFun // why? introduced in 4e488a60, doc welcome
                  || t.symbol == null       // ditto
                  || checkValidAdaptation(t, args)
                )
                if (keepTree) t else EmptyTree
              } orElse { _ => context.undetparams = savedUndetparams ; EmptyTree }
            }
            else EmptyTree
          }

          /* Treats an application which uses named or default arguments.
           * Also works if names + a vararg used: when names are used, the vararg
           * parameter has to be specified exactly once. Note that combining varargs
           * and defaults is ruled out by typedDefDef.
           */
          def tryNamesDefaults: Tree = {
            val lencmp = compareLengths(args, formals)

            def checkNotMacro() = {
              if (treeInfo.isMacroApplication(fun))
                tryTupleApply orElse duplErrorTree(NamedAndDefaultArgumentsNotSupportedForMacros(tree, fun))
            }

            if (mt.isErroneous) duplErrTree
            else if (mode.inPatternMode) {
              // #2064
              duplErrorTree(WrongNumberOfArgsError(tree, fun))
            } else if (lencmp > 0) {
              tryTupleApply orElse duplErrorTree(TooManyArgsNamesDefaultsError(tree, fun))
            } else if (lencmp == 0) {
              // we don't need defaults. names were used, so this application is transformed
              // into a block (@see transformNamedApplication in NamesDefaults)
              val (namelessArgs, argPos) = removeNames(Typer.this)(args, params)
              if (namelessArgs exists (_.isErroneous)) {
                duplErrTree
              } else if (!allArgsArePositional(argPos) && !sameLength(formals, params))
                // !allArgsArePositional indicates that named arguments are used to re-order arguments
                duplErrorTree(MultipleVarargError(tree))
              else if (allArgsArePositional(argPos) && !isNamedApplyBlock(fun)) {
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

              // SI-8111 transformNamedApplication eagerly shuffles around the application to preserve
              //         evaluation order. During this process, it calls `changeOwner` on symbols that
              //         are transplanted underneath synthetic temporary vals.
              //
              //         Here, we keep track of the symbols owned by `context.owner` to enable us to
              //         rollback, so that we don't end up with "orphaned" symbols.
              //
              //         TODO: Find a better way!
              //
              //         Note that duplicating trees would not be enough to fix this problem, we would also need to
              //         clone local symbols in the duplicated tree to truly isolate things (in the spirit of BodyDuplicator),
              //         or, better yet, disentangle the logic in `transformNamedApplication` so that we could
              //         determine whether names/defaults is viable *before* transforming trees.
              def ownerOf(sym: Symbol) = if (sym == null || sym == NoSymbol) NoSymbol else sym.owner
              val symsOwnedByContextOwner = tree.collect {
                case t @ (_: DefTree | _: Function) if ownerOf(t.symbol) == context.owner => t.symbol
              }
              def rollbackNamesDefaultsOwnerChanges() {
                symsOwnedByContextOwner foreach (_.owner = context.owner)
              }

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
                  checkNotMacro()
                  context.diagUsedDefaults = true
                  doTypedApply(tree, if (blockIsEmpty) fun else fun1, allArgs, mode, pt)
                } else {
                  rollbackNamesDefaultsOwnerChanges()
                  tryTupleApply orElse duplErrorTree(NotEnoughArgsError(tree, fun, missing))
                }
              }
            }
          }

          if (!sameLength(formals, args) ||   // wrong nb of arguments
              (args exists isNamedArg) ||     // uses a named argument
              isNamedApplyBlock(fun)) {       // fun was transformed to a named apply block =>
                                              // integrate this application into the block
            if (dyna.isApplyDynamicNamed(fun) && isDynamicRewrite(fun)) dyna.typedNamedApply(tree, fun, args, mode, pt)
            else tryNamesDefaults
          } else {
            val tparams = context.extractUndetparams()
            if (tparams.isEmpty) { // all type params are defined
              def handleMonomorphicCall: Tree = {
                // no expected type when jumping to a match label -- anything goes (this is ok since we're typing the translation of well-typed code)
                // ... except during erasure: we must take the expected type into account as it drives the insertion of casts!
                // I've exhausted all other semi-clean approaches I could think of in balancing GADT magic, SI-6145, CPS type-driven transforms and other existential trickiness
                // (the right thing to do -- packing existential types -- runs into limitations in subtyping existential types,
                //  casting breaks SI-6145,
                //  not casting breaks GADT typing as it requires sneaking ill-typed trees past typer)
                def noExpectedType = !phase.erasedTypes && fun.symbol.isLabel && treeInfo.isSynthCaseSymbol(fun.symbol)

                val args1 = (
                  if (noExpectedType)
                    typedArgs(args, forArgMode(fun, mode))
                  else
                    typedArgsForFormals(args, paramTypes, forArgMode(fun, mode))
                )

                // instantiate dependent method types, must preserve singleton types where possible (stableTypeFor) -- example use case:
                // val foo = "foo"; def precise(x: String)(y: x.type): x.type = {...}; val bar : foo.type = precise(foo)(foo)
                // precise(foo) : foo.type => foo.type
                val restpe = mt.resultType(mapList(args1)(arg => gen stableTypeFor arg orElse arg.tpe))
                def ifPatternSkipFormals(tp: Type) = tp match {
                  case MethodType(_, rtp) if (mode.inPatternMode) => rtp
                  case _ => tp
                }

                /*
                 * This is translating uses of List() into Nil.  This is less
                 *  than ideal from a consistency standpoint, but it shouldn't be
                 *  altered without due caution.
                 *  ... this also causes bootstrapping cycles if List_apply is
                 *  forced during kind-arity checking, so it is guarded by additional
                 *  tests to ensure we're sufficiently far along.
                 */
                if (args.isEmpty && canTranslateEmptyListToNil && fun.symbol.isInitialized && ListModule.hasCompleteInfo && (fun.symbol == List_apply))
                  atPos(tree.pos)(gen.mkNil setType restpe)
                else
                  constfold(treeCopy.Apply(tree, fun, args1) setType ifPatternSkipFormals(restpe))
              }
              checkDead.updateExpr(fun) {
                handleMonomorphicCall
              }
            } else if (needsInstantiation(tparams, formals, args)) {
              //println("needs inst "+fun+" "+tparams+"/"+(tparams map (_.info)))
              inferExprInstance(fun, tparams)
              doTypedApply(tree, fun, args, mode, pt)
            } else {
              def handlePolymorphicCall = {
                assert(!mode.inPatternMode, mode) // this case cannot arise for patterns
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
                if (args1 exists { _.isErrorTyped }) duplErrTree
                else {
                  debuglog("infer method inst " + fun + ", tparams = " + tparams + ", args = " + args1.map(_.tpe) + ", pt = " + pt + ", lobounds = " + tparams.map(_.tpe.bounds.lo) + ", parambounds = " + tparams.map(_.info)) //debug
                  // define the undetparams which have been fixed by this param list, replace the corresponding symbols in "fun"
                  // returns those undetparams which have not been instantiated.
                  val undetparams = inferMethodInstance(fun, tparams, args1, pt)
                  try doTypedApply(tree, fun, args1, mode, pt)
                  finally context.undetparams = undetparams
                }
              }
              handlePolymorphicCall
            }
          }

        case SingleType(_, _) =>
          doTypedApply(tree, fun setType fun.tpe.widen, args, mode, pt)

        case ErrorType =>
          if (!tree.isErrorTyped) setError(tree) else tree
          // @H change to setError(treeCopy.Apply(tree, fun, args))

        // SI-7877 `isTerm` needed to exclude `class T[A] { def unapply(..) }; ... case T[X] =>`
        case HasUnapply(unapply) if mode.inPatternMode && fun.isTerm =>
          doTypedUnapply(tree, fun0, fun, args, mode, pt)

        case _ =>
          if (treeInfo.isMacroApplication(tree)) duplErrorTree(MacroTooManyArgumentListsError(tree, fun.symbol))
          else duplErrorTree(ApplyWithoutArgsError(tree, fun))
      }
    }

    /**
     * Convert an annotation constructor call into an AnnotationInfo.
     */
    def typedAnnotation(ann: Tree, mode: Mode = EXPRmode): AnnotationInfo = {
      var hasError: Boolean = false
      val pending = ListBuffer[AbsTypeError]()
      def ErroneousAnnotation = new ErroneousAnnotation().setOriginal(ann)

      def finish(res: AnnotationInfo): AnnotationInfo = {
        if (hasError) {
          pending.foreach(ErrorUtils.issueTypeError)
          ErroneousAnnotation
        }
        else res
      }

      def reportAnnotationError(err: AbsTypeError) = {
        pending += err
        hasError = true
        ErroneousAnnotation
      }

      /* Calling constfold right here is necessary because some trees (negated
       * floats and literals in particular) are not yet folded.
       */
      def tryConst(tr: Tree, pt: Type): Option[LiteralAnnotArg] = {
        // The typed tree may be relevantly different than the tree `tr`,
        // e.g. it may have encountered an implicit conversion.
        val ttree = typed(constfold(tr), pt)
        val const: Constant = ttree match {
          case l @ Literal(c) if !l.isErroneous => c
          case tree => tree.tpe match {
            case ConstantType(c)  => c
            case tpe              => null
          }
        }

        if (const == null) {
          reportAnnotationError(AnnotationNotAConstantError(ttree)); None
        } else if (const.value == null) {
          reportAnnotationError(AnnotationArgNullError(tr)); None
        } else
          Some(LiteralAnnotArg(const))
      }

      /* Converts an untyped tree to a ClassfileAnnotArg. If the conversion fails,
       * an error message is reported and None is returned.
       */
      def tree2ConstArg(tree: Tree, pt: Type): Option[ClassfileAnnotArg] = tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) if (pt.typeSymbol == ArrayClass) =>
          reportAnnotationError(ArrayConstantsError(tree)); None

        case ann @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val annInfo = typedAnnotation(ann, mode)
          val annType = annInfo.tpe

          if (!annType.typeSymbol.isSubClass(pt.typeSymbol))
            reportAnnotationError(AnnotationTypeMismatchError(tpt, annType, annType))
          else if (!annType.typeSymbol.isSubClass(ClassfileAnnotationClass))
            reportAnnotationError(NestedAnnotationError(ann, annType))

          if (annInfo.atp.isErroneous) { hasError = true; None }
          else Some(NestedAnnotArg(annInfo))

        // use of Array.apply[T: ClassTag](xs: T*): Array[T]
        // and    Array.apply(x: Int, xs: Int*): Array[Int]       (and similar)
        case Apply(fun, args) =>
          val typedFun = typed(fun, mode.forFunMode)
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
      val treeInfo.Applied(fun0, targs, argss) = ann
      if (fun0.isErroneous)
        return finish(ErroneousAnnotation)
      val typedFun0 = typed(fun0, mode.forFunMode)
      val typedFunPart = (
        // If there are dummy type arguments in typeFun part, it suggests we
        // must type the actual constructor call, not only the select. The value
        // arguments are how the type arguments will be inferred.
        if (targs.isEmpty && typedFun0.exists(t => t.tpe != null && isDummyAppliedType(t.tpe)))
          logResult(s"Retyped $typedFun0 to find type args")(typed(argss.foldLeft(fun0)(Apply(_, _))))
        else
          typedFun0
      )
      val treeInfo.Applied(typedFun @ Select(New(annTpt), _), _, _) = typedFunPart
      val annType = annTpt.tpe

      finish(
        if (typedFun.isErroneous)
          ErroneousAnnotation
        else if (annType.typeSymbol isNonBottomSubClass ClassfileAnnotationClass) {
          // annotation to be saved as java classfile annotation
          val isJava = typedFun.symbol.owner.isJavaDefined
          if (argss.length > 1) {
            reportAnnotationError(MultipleArgumentListForAnnotationError(ann))
          }
          else {
            val annScope = annType.decls
                .filter(sym => sym.isMethod && !sym.isConstructor && sym.isJavaDefined)
            val names = mutable.Set[Symbol]()
            names ++= (if (isJava) annScope.iterator
                       else typedFun.tpe.params.iterator)

            def hasValue = names exists (_.name == nme.value)
            val args = argss match {
              case (arg :: Nil) :: Nil if !isNamedArg(arg) && hasValue => gen.mkNamedArg(nme.value, arg) :: Nil
              case args :: Nil                                         => args
            }

            val nvPairs = args map {
              case arg @ AssignOrNamedArg(Ident(name), rhs) =>
                val sym = if (isJava) annScope.lookup(name)
                          else findSymbol(typedFun.tpe.params)(_.name == name)
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

            if (hasError) ErroneousAnnotation
            else AnnotationInfo(annType, List(), nvPairs map {p => (p._1, p._2.get)}).setOriginal(Apply(typedFun, args).setPos(ann.pos))
          }
        }
        else {
          val typedAnn: Tree = {
            // local dummy fixes SI-5544
            val localTyper = newTyper(context.make(ann, context.owner.newLocalDummy(ann.pos)))
            localTyper.typed(ann, mode, annType)
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
            context.deprecationWarning(ann.pos, DeprecatedAttr, "@deprecated now takes two arguments; see the scaladoc.")

          if ((typedAnn.tpe == null) || typedAnn.tpe.isErroneous) ErroneousAnnotation
          else annInfo(typedAnn)
        }
      )
    }

    /** Compute an existential type from raw hidden symbols `syms` and type `tp`
     */
    def packSymbols(hidden: List[Symbol], tp: Type): Type = global.packSymbols(hidden, tp, context0.owner)

    def isReferencedFrom(ctx: Context, sym: Symbol): Boolean = (
       ctx.owner.isTerm && (ctx.scope.exists { dcl => dcl.isInitialized && (dcl.info contains sym) }) || {
          var ctx1 = ctx.outer
          while ((ctx1 != NoContext) && (ctx1.scope eq ctx.scope))
            ctx1 = ctx1.outer

          (ctx1 != NoContext) && isReferencedFrom(ctx1, sym)
       }
    )

    def isCapturedExistential(sym: Symbol) = (
      (sym hasAllFlags EXISTENTIAL | CAPTURED) && {
        val start = if (Statistics.canEnable) Statistics.startTimer(isReferencedNanos) else null
        try !isReferencedFrom(context, sym)
        finally if (Statistics.canEnable) Statistics.stopTimer(isReferencedNanos, start)
      }
    )

    def packCaptured(tpe: Type): Type = {
      val captured = mutable.Set[Symbol]()
      for (tp <- tpe)
        if (isCapturedExistential(tp.typeSymbol))
          captured += tp.typeSymbol
      existentialAbstraction(captured.toList, tpe)
    }

    /** convert local symbols and skolems to existentials */
    def packedType(tree: Tree, owner: Symbol): Type = {
      def defines(tree: Tree, sym: Symbol) = (
           sym.isExistentialSkolem && sym.unpackLocation == tree
        || tree.isDef && tree.symbol == sym
      )
      def isVisibleParameter(sym: Symbol) = (
           sym.isParameter
        && (sym.owner == owner)
        && (sym.isType || !owner.isAnonymousFunction)
      )
      def containsDef(owner: Symbol, sym: Symbol): Boolean =
        (!sym.hasPackageFlag) && {
          var o = sym.owner
          while (o != owner && o != NoSymbol && !o.hasPackageFlag) o = o.owner
          o == owner && !isVisibleParameter(sym)
        }
      var localSyms = immutable.Set[Symbol]()
      var boundSyms = immutable.Set[Symbol]()
      def isLocal(sym: Symbol): Boolean =
        if (sym == NoSymbol || sym.isRefinementClass || sym.isLocalDummy) false
        else if (owner == NoSymbol) tree exists (defines(_, sym))
        else containsDef(owner, sym) || isRawParameter(sym) || isCapturedExistential(sym)
      def containsLocal(tp: Type): Boolean =
        tp exists (t => isLocal(t.typeSymbol) || isLocal(t.termSymbol))

      val dealiasLocals = new TypeMap {
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) =>
            if (sym.isAliasType && containsLocal(tp) && (tp.dealias ne tp)) apply(tp.dealias)
            else {
              if (pre.isVolatile) pre match {
                case SingleType(_, sym) if sym.isSynthetic && isPastTyper =>
                  debuglog(s"ignoring volatility of prefix in pattern matcher generated inferred type: $tp") // See pos/t7459c.scala
                case _ =>
                  InferTypeWithVolatileTypeSelectionError(tree, pre)
              }
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
            case AnnotatedType(annots, _) =>
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

      val dealiasedType = dealiasLocals(tree.tpe)
      addLocals(dealiasedType)
      packSymbols(localSyms.toList, dealiasedType)
    }

    def typedClassOf(tree: Tree, tpt: Tree, noGen: Boolean = false) =
      if (!checkClassType(tpt) && noGen) tpt
      else atPos(tree.pos)(gen.mkClassOf(tpt.tpe))

    protected def typedExistentialTypeTree(tree: ExistentialTypeTree, mode: Mode): Tree = {
      for (wc <- tree.whereClauses)
        if (wc.symbol == NoSymbol) { namer enterSym wc; wc.symbol setFlag EXISTENTIAL }
        else context.scope enter wc.symbol
      val whereClauses1 = typedStats(tree.whereClauses, context.owner)
      for (vd @ ValDef(_, _, _, _) <- whereClauses1)
        if (vd.symbol.tpe.isVolatile)
          AbstractionFromVolatileTypeError(vd)
      val tpt1 = typedType(tree.tpt, mode)
      existentialTransform(whereClauses1 map (_.symbol), tpt1.tpe)((tparams, tp) => {
        val original = tpt1 match {
          case tpt : TypeTree => atPos(tree.pos)(ExistentialTypeTree(tpt.original, tree.whereClauses))
          case _ => {
            debuglog(s"cannot reconstruct the original for $tree, because $tpt1 is not a TypeTree")
            tree
          }
        }
        TypeTree(newExistentialType(tparams, tp)) setOriginal original
      }
      )
    }

    // lifted out of typed1 because it's needed in typedImplicit0
    protected def typedTypeApply(tree: Tree, mode: Mode, fun: Tree, args: List[Tree]): Tree = fun.tpe match {
      case OverloadedType(pre, alts) =>
        inferPolyAlternatives(fun, mapList(args)(treeTpe))

        // SI-8267 `memberType` can introduce existentials *around* a PolyType/MethodType, see AsSeenFromMap#captureThis.
        //         If we had selected a non-overloaded symbol, `memberType` would have been called in `makeAccessible`
        //         and the resulting existential type would have been skolemized in `adapt` *before* we typechecked
        //         the enclosing type-/ value- application.
        //
        //         However, if the selection is overloaded, we defer calling `memberType` until we can select a single
        //         alternative here. It is therefore necessary to skolemize the existential here.
        //
        val fun1 = adaptAfterOverloadResolution(fun, mode.forFunMode | TAPPmode)

        val tparams = fun1.symbol.typeParams //@M TODO: fun.symbol.info.typeParams ? (as in typedAppliedTypeTree)
        val args1 = if (sameLength(args, tparams)) {
          //@M: in case TypeApply we can't check the kind-arities of the type arguments,
          // as we don't know which alternative to choose... here we do
          map2Conserve(args, tparams) {
            //@M! the polytype denotes the expected kind
            (arg, tparam) => typedHigherKindedType(arg, mode, Kind.FromParams(tparam.typeParams))
          }
        } else // @M: there's probably something wrong when args.length != tparams.length... (triggered by bug #320)
         // Martin, I'm using fake trees, because, if you use args or arg.map(typedType),
         // inferPolyAlternatives loops...  -- I have no idea why :-(
         // ...actually this was looping anyway, see bug #278.
          return TypedApplyWrongNumberOfTpeParametersError(fun, fun)

        typedTypeApply(tree, mode, fun1, args1)
      case SingleType(_, _) =>
        typedTypeApply(tree, mode, fun setType fun.tpe.widen, args)
      case PolyType(tparams, restpe) if tparams.nonEmpty =>
        if (sameLength(tparams, args)) {
          val targs = mapList(args)(treeTpe)
          checkBounds(tree, NoPrefix, NoSymbol, tparams, targs, "")
          if (isPredefClassOf(fun.symbol))
            typedClassOf(tree, args.head, noGen = true)
          else {
            if (!isPastTyper && fun.symbol == Any_isInstanceOf && targs.nonEmpty) {
              val scrutineeType = fun match {
                case Select(qual, _) => qual.tpe
                case _               => AnyTpe
              }
              checkCheckable(tree, targs.head, scrutineeType, inPattern = false)
            }
            val resultpe = restpe.instantiateTypeParams(tparams, targs)
            //@M substitution in instantiateParams needs to be careful!
            //@M example: class Foo[a] { def foo[m[x]]: m[a] = error("") } (new Foo[Int]).foo[List] : List[Int]
            //@M    --> first, m[a] gets changed to m[Int], then m gets substituted for List,
            //          this must preserve m's type argument, so that we end up with List[Int], and not List[a]
            //@M related bug: #1438
            //println("instantiating type params "+restpe+" "+tparams+" "+targs+" = "+resultpe)
            treeCopy.TypeApply(tree, fun, args) setType resultpe
          }
        }
        else {
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
          // val treeInfo.Applied(methPart, _, _) = fun
          // println("methPart of "+ fun +" is "+ methPart)
          // if (methPart ne fun) isApplyDynamicNamed(methPart)
          // else false
      }

      def typedNamedApply(orig: Tree, fun: Tree, args: List[Tree], mode: Mode, pt: Type): Tree = {
        def argToBinding(arg: Tree): Tree = arg match {
          case AssignOrNamedArg(i @ Ident(name), rhs) =>
            atPos(i.pos.withEnd(rhs.pos.end)) {
              gen.mkTuple(List(atPos(i.pos)(CODE.LIT(name.toString)), rhs))
            }
          case _ =>
            gen.mkTuple(List(CODE.LIT(""), arg))
        }

        val t = treeCopy.Apply(orig, unmarkDynamicRewrite(fun), args map argToBinding)
        wrapErrors(t, _.typed(t, mode, pt))
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
      def mkInvoke(context: Context, tree: Tree, qual: Tree, name: Name): Option[Tree] = {
        val cxTree = context.enclosingNonImportContext.tree // SI-8364
        debuglog(s"dyna.mkInvoke($cxTree, $tree, $qual, $name)")
        val treeInfo.Applied(treeSelection, _, _) = tree
        def isDesugaredApply = {
          val protoQual = macroExpandee(qual) orElse qual
          treeSelection match {
            case Select(`protoQual`, nme.apply) => true
            case _                              => false
          }
        }
        acceptsApplyDynamicWithType(qual, name) map { tp =>
          // If tp == NoType, pass only explicit type arguments to applyXXX.  Not used at all
          // here - it is for scala-virtualized, where tp will be passed as an argument (for
          // selection on a staged Struct)
          def hasNamed(args: List[Tree]): Boolean = args exists (_.isInstanceOf[AssignOrNamedArg])
          // not supported: foo.bar(a1,..., an: _*)
          def hasStar(args: List[Tree]) = treeInfo.isWildcardStarArgList(args)
          def applyOp(args: List[Tree]) = if (hasNamed(args)) nme.applyDynamicNamed else nme.applyDynamic
          def matches(t: Tree)          = isDesugaredApply || treeInfo.dissectApplied(t).core == treeSelection

          /* Note that the trees which arrive here are potentially some distance from
           * the trees of direct interest. `cxTree` is some enclosing expression which
           * may apparently be arbitrarily larger than `tree`; and `tree` itself is
           * too small, having at least in some cases lost its explicit type parameters.
           * This logic is designed to use `tree` to pinpoint the immediately surrounding
           * Apply/TypeApply/Select node, and only then creates the dynamic call.
           * See SI-6731 among others.
           */
          def findSelection(t: Tree): Option[(TermName, Tree)] = t match {
            case Apply(fn, args) if hasStar(args) => DynamicVarArgUnsupported(tree, applyOp(args)) ; None
            case Apply(fn, args) if matches(fn)   => Some((applyOp(args), fn))
            case Assign(lhs, _) if matches(lhs)   => Some((nme.updateDynamic, lhs))
            case _ if matches(t)                  => Some((nme.selectDynamic, t))
            case _                                => (t.children flatMap findSelection).headOption
          }
          findSelection(cxTree) match {
            case Some((opName, treeInfo.Applied(_, targs, _))) =>
              val fun = gen.mkTypeApply(Select(qual, opName), targs)
              if (opName == nme.updateDynamic) suppressMacroExpansion(fun) // SI-7617
              val nameStringLit = atPos(treeSelection.pos.withStart(treeSelection.pos.point).makeTransparent) {
                Literal(Constant(name.decode))
              }
              markDynamicRewrite(atPos(qual.pos)(Apply(fun, List(nameStringLit))))
            case _ =>
              setError(tree)
          }
        }
      }
      def wrapErrors(tree: Tree, typeTree: Typer => Tree): Tree = silent(typeTree) orElse (err => DynamicRewriteError(tree, err.head))
    }

    def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      // Lookup in the given class using the root mirror.
      def lookupInOwner(owner: Symbol, name: Name): Symbol =
        if (mode.inQualMode) rootMirror.missingHook(owner, name) else NoSymbol

      // Lookup in the given qualifier.  Used in last-ditch efforts by typedIdent and typedSelect.
      def lookupInRoot(name: Name): Symbol  = lookupInOwner(rootMirror.RootClass, name)
      def lookupInEmpty(name: Name): Symbol = rootMirror.EmptyPackageClass.info member name

      def lookupInQualifier(qual: Tree, name: Name): Symbol = (
        if (name == nme.ERROR || qual.tpe.widen.isErroneous)
          NoSymbol
        else lookupInOwner(qual.tpe.typeSymbol, name) orElse {
          NotAMemberError(tree, qual, name)
          NoSymbol
        }
      )

      def typedAnnotated(atd: Annotated): Tree = {
        val ann = atd.annot
        val arg1 = typed(atd.arg, mode, pt)
        /* mode for typing the annotation itself */
        val annotMode = (mode &~ TYPEmode) | EXPRmode

        def resultingTypeTree(tpe: Type) = {
          // we need symbol-ful originals for reification
          // hence we go the extra mile to hand-craft this guy
          val original = arg1 match {
            case tt @ TypeTree() if tt.original != null => Annotated(ann, tt.original)
            // this clause is needed to correctly compile stuff like "new C @D" or "@(inline @getter)"
            case _ => Annotated(ann, arg1)
          }
          original setType ann.tpe
          TypeTree(tpe) setOriginal original setPos tree.pos.focus
        }

        if (arg1.isType) {
          // make sure the annotation is only typechecked once
          if (ann.tpe == null) {
            val ainfo = typedAnnotation(ann, annotMode)
            val atype = arg1.tpe.withAnnotation(ainfo)

            if (ainfo.isErroneous)
              // Erroneous annotations were already reported in typedAnnotation
              arg1  // simply drop erroneous annotations
            else {
              ann setType atype
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
            ann setType arg1.tpe.withAnnotation(annotInfo)
          }
          val atype = ann.tpe
          Typed(arg1, resultingTypeTree(atype)) setPos tree.pos setType atype
        }
      }

      def typedBind(tree: Bind) = {
        val name = tree.name
        val body = tree.body
        name match {
          case name: TypeName  => assert(body == EmptyTree, context.unit + " typedBind: " + name.debugString + " " + body + " " + body.getClass)
            val sym =
              if (tree.symbol != NoSymbol) tree.symbol
              else {
                if (isFullyDefined(pt))
                  context.owner.newAliasType(name, tree.pos) setInfo pt
                else
                  context.owner.newAbstractType(name, tree.pos) setInfo TypeBounds.empty
              }

            if (name != tpnme.WILDCARD) namer.enterInScope(sym)
            else context.scope.enter(sym)

            tree setSymbol sym setType sym.tpeHK

          case name: TermName  =>
            val sym =
              if (tree.symbol != NoSymbol) tree.symbol
              else context.owner.newValue(name, tree.pos)

            if (name != nme.WILDCARD) {
              if (context.inPatAlternative)
                VariableInPatternAlternativeError(tree)

              namer.enterInScope(sym)
            }

            val body1 = typed(body, mode, pt)
            val impliedType = patmat.binderTypeImpliedByPattern(body1, pt, sym) // SI-1503, SI-5204
            val symTp =
              if (treeInfo.isSequenceValued(body)) seqType(impliedType)
              else impliedType
            sym setInfo symTp

            // have to imperatively set the symbol for this bind to keep it in sync with the symbols used in the body of a case
            // when type checking a case we imperatively update the symbols in the body of the case
            // those symbols are bound by the symbols in the Binds in the pattern of the case,
            // so, if we set the symbols in the case body, but not in the patterns,
            // then re-type check the casedef (for a second try in typedApply for example -- SI-1832),
            // we are no longer in sync: the body has symbols set that do not appear in the patterns
            // since body1 is not necessarily equal to body, we must return a copied tree,
            // but we must still mutate the original bind
            tree setSymbol sym
            treeCopy.Bind(tree, name, body1) setSymbol sym setType body1.tpe
        }
      }

      def typedArrayValue(tree: ArrayValue) = {
        val elemtpt1 = typedType(tree.elemtpt, mode)
        val elems1   = tree.elems mapConserve (elem => typed(elem, mode, elemtpt1.tpe))
        // see run/t6126 for an example where `pt` does not suffice (tagged types)
        val tpe1     = if (isFullyDefined(pt) && !phase.erasedTypes) pt else arrayType(elemtpt1.tpe)

        treeCopy.ArrayValue(tree, elemtpt1, elems1) setType tpe1
      }

      def typedAssign(lhs: Tree, rhs: Tree): Tree = {
        // see SI-7617 for an explanation of why macro expansion is suppressed
        def typedLhs(lhs: Tree) = typed(lhs, EXPRmode | LHSmode)
        val lhs1    = unsuppressMacroExpansion(typedLhs(suppressMacroExpansion(lhs)))
        val varsym  = lhs1.symbol

        // see #2494 for double error message example
        def fail() =
          if (lhs1.isErrorTyped) lhs1
          else AssignmentError(tree, varsym)

        if (varsym == null)
          return fail()

        if (treeInfo.mayBeVarGetter(varsym)) {
          lhs1 match {
            case treeInfo.Applied(Select(qual, name), _, _) =>
              val sel = Select(qual, name.setterName) setPos lhs.pos
              val app = Apply(sel, List(rhs)) setPos tree.pos
              return typed(app, mode, pt)

            case _ =>
          }
        }
//      if (varsym.isVariable ||
//        // setter-rewrite has been done above, so rule out methods here, but, wait a minute, why are we assigning to non-variables after erasure?!
//        (phase.erasedTypes && varsym.isValue && !varsym.isMethod)) {
        if (varsym.isVariable || varsym.isValue && phase.erasedTypes) {
          val rhs1 = typedByValueExpr(rhs, lhs1.tpe)
          treeCopy.Assign(tree, lhs1, checkDead(rhs1)) setType UnitTpe
        }
        else if(dyna.isDynamicallyUpdatable(lhs1)) {
          val rhs1 = typedByValueExpr(rhs)
          val t = atPos(lhs1.pos.withEnd(rhs1.pos.end)) {
            Apply(lhs1, List(rhs1))
          }
          dyna.wrapErrors(t, _.typed1(t, mode, pt))
        }
        else fail()
      }

      def typedIf(tree: If): If = {
        val cond1 = checkDead(typedByValueExpr(tree.cond, BooleanTpe))
        // One-legged ifs don't need a lot of analysis
        if (tree.elsep.isEmpty)
          return treeCopy.If(tree, cond1, typed(tree.thenp, UnitTpe), tree.elsep) setType UnitTpe

        val thenp1 = typed(tree.thenp, pt)
        val elsep1 = typed(tree.elsep, pt)

        // in principle we should pack the types of each branch before lubbing, but lub doesn't really work for existentials anyway
        // in the special (though common) case where the types are equal, it pays to pack before comparing
        // especially virtpatmat needs more aggressive unification of skolemized types
        // this breaks src/library/scala/collection/immutable/TrieIterator.scala
        // annotated types need to be lubbed regardless (at least, continuations break if you bypass them like this)
        def samePackedTypes = (
             !isPastTyper
          && thenp1.tpe.annotations.isEmpty
          && elsep1.tpe.annotations.isEmpty
          && packedType(thenp1, context.owner) =:= packedType(elsep1, context.owner)
        )
        def finish(ownType: Type) = treeCopy.If(tree, cond1, thenp1, elsep1) setType ownType
        // TODO: skolemize (lub of packed types) when that no longer crashes on files/pos/t4070b.scala
        // @PP: This was doing the samePackedTypes check BEFORE the isFullyDefined check,
        // which based on everything I see everywhere else was a bug. I reordered it.
        if (isFullyDefined(pt))
          finish(pt)
        // Important to deconst, otherwise `if (???) 0 else 0` evaluates to 0 (SI-6331)
        else thenp1.tpe.deconst :: elsep1.tpe.deconst :: Nil match {
          case tp :: _ if samePackedTypes     => finish(tp)
          case tpes if sameWeakLubAsLub(tpes) => finish(lub(tpes))
          case tpes                           =>
            val lub = weakLub(tpes)
            treeCopy.If(tree, cond1, adapt(thenp1, mode, lub), adapt(elsep1, mode, lub)) setType lub
        }
      }

      // When there's a suitable __match in scope, virtualize the pattern match
      // otherwise, type the Match and leave it until phase `patmat` (immediately after typer)
      // empty-selector matches are transformed into synthetic PartialFunction implementations when the expected type demands it
      def typedVirtualizedMatch(tree: Match): Tree = {
        val selector = tree.selector
        val cases = tree.cases
        if (selector == EmptyTree) {
          if (pt.typeSymbol == PartialFunctionClass)
            synthesizePartialFunction(newTermName(context.unit.fresh.newName("x")), tree.pos, paramSynthetic = true, tree, mode, pt)
          else {
            val arity = if (isFunctionType(pt)) pt.dealiasWiden.typeArgs.length - 1 else 1
            val params = for (i <- List.range(0, arity)) yield
              atPos(tree.pos.focusStart) {
                ValDef(Modifiers(PARAM | SYNTHETIC),
                       unit.freshTermName("x" + i + "$"), TypeTree(), EmptyTree)
              }
            val ids = for (p <- params) yield Ident(p.name)
            val selector1 = atPos(tree.pos.focusStart) { if (arity == 1) ids.head else gen.mkTuple(ids) }
            // SI-8120 If we don't duplicate the cases, the original Match node will share trees with ones that
            //         receive symbols owned by this function. However if, after a silent mode session, we discard
            //         this Function and try a different approach (e.g. applying a view to the receiver) we end up
            //         with orphaned symbols which blows up far down the pipeline (or can be detected with -Ycheck:typer).
            val body = treeCopy.Match(tree, selector1, (cases map duplicateAndKeepPositions).asInstanceOf[List[CaseDef]])
            typed1(atPos(tree.pos) { Function(params, body) }, mode, pt)
          }
        } else
          virtualizedMatch(typedMatch(selector, cases, mode, pt, tree), mode, pt)
      }

      def typedReturn(tree: Return) = {
        val expr = tree.expr
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
          }
          else {
            val expr1 = context withinReturnExpr typedByValueExpr(expr, restpt.tpe)
            // Warn about returning a value if no value can be returned.
            if (restpt.tpe.typeSymbol == UnitClass) {
              // The typing in expr1 says expr is Unit (it has already been coerced if
              // it is non-Unit) so we have to retype it.  Fortunately it won't come up much
              // unless the warning is legitimate.
              if (typed(expr).tpe.typeSymbol != UnitClass)
                context.warning(tree.pos, "enclosing method " + name + " has result type Unit: return value discarded")
            }
            val res = treeCopy.Return(tree, checkDead(expr1)).setSymbol(enclMethod.owner)
            val tp = pluginsTypedReturn(NothingTpe, this, res, restpt.tpe)
            res.setType(tp)
          }
        }
      }

      def typedNew(tree: New) = {
        val tpt = tree.tpt
        val tpt1 = {
          // This way typedNew always returns a dealiased type. This used to happen by accident
          // for instantiations without type arguments due to ad hoc code in typedTypeConstructor,
          // and annotations depended on it (to the extent that they worked, which they did
          // not when given a parameterized type alias which dealiased to an annotation.)
          // typedTypeConstructor dealiases nothing now, but it makes sense for a "new" to always be
          // given a dealiased type.
          val tpt0 = typedTypeConstructor(tpt) modifyType (_.dealias)
          if (checkStablePrefixClassType(tpt0))
            if (tpt0.hasSymbolField && !tpt0.symbol.typeParams.isEmpty) {
              context.undetparams = cloneSymbols(tpt0.symbol.typeParams)
              notifyUndetparamsAdded(context.undetparams)
              TypeTree().setOriginal(tpt0)
                        .setType(appliedType(tpt0.tpe, context.undetparams map (_.tpeHK))) // @PP: tpeHK! #3343, #4018, #4347.
            } else tpt0
          else tpt0
        }

        /* If current tree <tree> appears in <val x(: T)? = <tree>>
         * return `tp with x.type' else return `tp`.
         */
        def narrowRhs(tp: Type) = { val sym = context.tree.symbol
          context.tree match {
            case ValDef(mods, _, _, Apply(Select(`tree`, _), _)) if !mods.isMutable && sym != null && sym != NoSymbol =>
              val sym1 = if (sym.owner.isClass && sym.getterIn(sym.owner) != NoSymbol) sym.getterIn(sym.owner)
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
        else if (!(  tp == sym.typeOfThis // when there's no explicit self type -- with (#3612) or without self variable
                     // sym.thisSym.tpe == tp.typeOfThis (except for objects)
                  || narrowRhs(tp) <:< tp.typeOfThis
                  || phase.erasedTypes
                  )) {
          DoesNotConformToSelfTypeError(tree, sym, tp.typeOfThis)
        } else
          treeCopy.New(tree, tpt1).setType(tp)
      }

      def functionTypeWildcard(tree: Tree, arity: Int): Type = {
        val tp = functionType(List.fill(arity)(WildcardType), WildcardType)
        if (tp == NoType) MaxFunctionArityError(tree)
        tp
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
          else adapt(expr1, mode, functionTypeWildcard(expr1, formals.length))
        case MethodType(formals, _) =>
          if (isFunctionType(pt)) expr1
          else adapt(expr1, mode, functionTypeWildcard(expr1, formals.length))
        case ErrorType =>
          expr1
        case _ =>
          UnderscoreEtaError(expr1)
      }

      def tryTypedArgs(args: List[Tree], mode: Mode): Option[List[Tree]] = {
        val c = context.makeSilent(reportAmbiguousErrors = false)
        c.retyping = true
        try {
          val res = newTyper(c).typedArgs(args, mode)
          if (c.reporter.hasErrors) None else Some(res)
        } catch {
          case ex: CyclicReference =>
            throw ex
          case te: TypeError =>
            // @H some of typer errors can still leak,
            // for instance in continuations
            None
        }
      }

      /* Try to apply function to arguments; if it does not work, try to convert Java raw to existentials, or try to
       * insert an implicit conversion.
       */
      def tryTypedApply(fun: Tree, args: List[Tree]): Tree = {
        val start = if (Statistics.canEnable) Statistics.startTimer(failedApplyNanos) else null

        def onError(typeErrors: Seq[AbsTypeError], warnings: Seq[(Position, String)]): Tree = {
          if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, start)

          // If the problem is with raw types, convert to existentials and try again.
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
            case Select(qual, name)                 => treesInResult(qual)
            case Apply(fun, args)                   => treesInResult(fun) ++ args.flatMap(treesInResult)
            case TypeApply(fun, args)               => treesInResult(fun) ++ args.flatMap(treesInResult)
            case _                                  => Nil
          })
          def errorInResult(tree: Tree) = treesInResult(tree) exists (err => typeErrors.exists(_.errPos == err.pos))

          val retry = (typeErrors.forall(_.errPos != null)) && (fun :: tree :: args exists errorInResult)
          typingStack.printTyping({
            val funStr = ptTree(fun) + " and " + (args map ptTree mkString ", ")
            if (retry) "second try: " + funStr
            else "no second try: " + funStr + " because error not in result: " + typeErrors.head.errPos+"!="+tree.pos
          })
          if (retry) {
            val Select(qual, name) = fun
            tryTypedArgs(args, forArgMode(fun, mode)) match {
              case Some(args1) if !args1.exists(arg => arg.exists(_.isErroneous)) =>
                val qual1 =
                  if (!pt.isError) adaptToArguments(qual, name, args1, pt, reportAmbiguous = true, saveErrors = true)
                  else qual
                if (qual1 ne qual) {
                  val tree1 = Apply(Select(qual1, name) setPos fun.pos, args1) setPos tree.pos
                  return context withinSecondTry typed1(tree1, mode, pt)
                }
              case _ => ()
            }
          }
          typeErrors foreach context.issue
          warnings foreach { case (p, m) => context.warning(p, m) }
          setError(treeCopy.Apply(tree, fun, args))
        }

        silent(_.doTypedApply(tree, fun, args, mode, pt)) match {
          case SilentResultValue(value) => value
          case e: SilentTypeError => onError(e.errors, e.warnings)
        }
      }

      def normalTypedApply(tree: Tree, fun: Tree, args: List[Tree]) = {
        // TODO: replace `fun.symbol.isStable` by `treeInfo.isStableIdentifierPattern(fun)`
        val stableApplication = (fun.symbol ne null) && fun.symbol.isMethod && fun.symbol.isStable
        val funpt = if (mode.inPatternMode) pt else WildcardType
        val appStart = if (Statistics.canEnable) Statistics.startTimer(failedApplyNanos) else null
        val opeqStart = if (Statistics.canEnable) Statistics.startTimer(failedOpEqNanos) else null

        def onError(reportError: => Tree): Tree = fun match {
          case Select(qual, name) if !mode.inPatternMode && nme.isOpAssignmentName(newTermName(name.decode)) =>
            val qual1 = typedQualifier(qual)
            if (treeInfo.isVariableOrGetter(qual1)) {
              if (Statistics.canEnable) Statistics.stopTimer(failedOpEqNanos, opeqStart)
              convertToAssignment(fun, qual1, name, args)
            }
            else {
              if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, appStart)
                reportError
            }
          case _ =>
            if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, appStart)
            reportError
        }
        val silentResult = silent(
          op                    = _.typed(fun, mode.forFunMode, funpt),
          reportAmbiguousErrors = !mode.inExprMode && context.ambiguousErrors,
          newtree               = if (mode.inExprMode) tree else context.tree
        )
        silentResult match {
          case SilentResultValue(fun1) =>
            val fun2 = if (stableApplication) stabilizeFun(fun1, mode, pt) else fun1
            if (Statistics.canEnable) Statistics.incCounter(typedApplyCount)
            val noSecondTry = (
                 isPastTyper
              || context.inSecondTry
              || (fun2.symbol ne null) && fun2.symbol.isConstructor
              || isImplicitMethodType(fun2.tpe)
            )
            val isFirstTry = fun2 match {
              case Select(_, _) => !noSecondTry && mode.inExprMode
              case _            => false
            }
            if (isFirstTry)
              tryTypedApply(fun2, args)
            else
              doTypedApply(tree, fun2, args, mode, pt)
          case err: SilentTypeError =>
            onError({
              err.reportableErrors foreach context.issue
              err.warnings foreach { case (p, m) => context.warning(p, m) }
              args foreach (arg => typed(arg, mode, ErrorType))
              setError(tree)
            })
        }
      }

      // convert new Array[T](len) to evidence[ClassTag[T]].newArray(len)
      // convert new Array^N[T](len) for N > 1 to evidence[ClassTag[Array[...Array[T]...]]].newArray(len)
      // where Array HK gets applied (N-1) times
      object ArrayInstantiation {
        def unapply(tree: Apply) = tree match {
          case Apply(Select(New(tpt), name), arg :: Nil) if tpt.tpe != null && tpt.tpe.typeSymbol == ArrayClass =>
            Some(tpt.tpe) collect {
              case erasure.GenericArray(level, componentType) =>
                val tagType = (1 until level).foldLeft(componentType)((res, _) => arrayType(res))

                resolveClassTag(tree.pos, tagType) match {
                  case EmptyTree => MissingClassTagError(tree, tagType)
                  case tag       => atPos(tree.pos)(new ApplyToImplicitArgs(Select(tag, nme.newArray), arg :: Nil))
                }
            }
          case _ => None
        }
      }

      def typedApply(tree: Apply) = tree match {
        case Apply(Block(stats, expr), args) =>
          typed1(atPos(tree.pos)(Block(stats, Apply(expr, args) setPos tree.pos.makeTransparent)), mode, pt)
        case Apply(fun, args) =>
          normalTypedApply(tree, fun, args) match {
            case ArrayInstantiation(tree1)                                           => if (tree1.isErrorTyped) tree1 else typed(tree1, mode, pt)
            case Apply(Select(fun, nme.apply), _) if treeInfo.isSuperConstrCall(fun) => TooManyArgumentListsForConstructor(tree) //SI-5696
            case tree1                                                               => tree1
          }
      }

      def convertToAssignment(fun: Tree, qual: Tree, name: Name, args: List[Tree]): Tree = {
        val prefix = name.toTermName stripSuffix nme.EQL
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
            fn match {
              case treeInfo.Applied(Select(table, nme.apply), _, _) => mkUpdate(table, indices)
              case _  => UnexpectedTreeAssignmentConversionError(qual)
            }
        }
        typed1(tree1, mode, pt)
      }

      def typedSuper(tree: Super) = {
        val mix = tree.mix
        val qual1 = typed(tree.qual)

        val clazz = qual1 match {
          case This(_) => qual1.symbol
          case _ => qual1.tpe.typeSymbol
        }
        def findMixinSuper(site: Type): Type = {
          var ps = site.parents filter (_.typeSymbol.name == mix)
          if (ps.isEmpty)
            ps = site.parents filter (_.typeSymbol.toInterface.name == mix)
          if (ps.isEmpty) {
            debuglog("Fatal: couldn't find site " + site + " in " + site.parents.map(_.typeSymbol.name))
            if (phase.erasedTypes && context.enclClass.owner.isImplClass) {
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
          else if (context.inSuperInit) clazz.info.firstParent
          else intersectionType(clazz.info.parents)
        )
        treeCopy.Super(tree, qual1, mix) setType SuperType(clazz.thisType, owntype)
      }

      def typedThis(tree: This) =
        tree.symbol orElse qualifyingClass(tree, tree.qual, packageOK = false) match {
          case NoSymbol => tree
          case clazz    =>
            tree setSymbol clazz setType clazz.thisType.underlying
            if (isStableContext(tree, mode, pt)) tree setType clazz.thisType else tree
        }

      /* Attribute a selection where `tree` is `qual.name`.
       * `qual` is already attributed.
       */
      def typedSelect(tree: Tree, qual: Tree, name: Name): Tree = {
        val t = typedSelectInternal(tree, qual, name)
        // Checking for OverloadedTypes being handed out after overloading
        // resolution has already happened.
        if (isPastTyper) t.tpe match {
          case OverloadedType(pre, alts) =>
            if (alts forall (s => (s.owner == ObjectClass) || (s.owner == AnyClass) || isPrimitiveValueClass(s.owner))) ()
            else if (settings.debug) printCaller(
              s"""|Select received overloaded type during $phase, but typer is over.
                  |If this type reaches the backend, we are likely doomed to crash.
                  |$t has these overloads:
                  |${alts map (s => "  " + s.defStringSeenAs(pre memberType s)) mkString "\n"}
                  |""".stripMargin
            )("")
          case _ =>
        }
        t
      }
      def typedSelectInternal(tree: Tree, qual: Tree, name: Name): Tree = {
        def asDynamicCall = dyna.mkInvoke(context, tree, qual, name) map { t =>
          dyna.wrapErrors(t, (_.typed1(t, mode, pt)))
        }

        val sym = tree.symbol orElse member(qual, name) orElse {
          // symbol not found? --> try to convert implicitly to a type that does have the required
          // member.  Added `| PATTERNmode` to allow enrichment in patterns (so we can add e.g., an
          // xml member to StringContext, which in turn has an unapply[Seq] method)
          if (name != nme.CONSTRUCTOR && mode.inAny(EXPRmode | PATTERNmode)) {
            val qual1 = adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = true, saveErrors = true)
            if ((qual1 ne qual) && !qual1.isErrorTyped)
              return typed(treeCopy.Select(tree, qual1, name), mode, pt)
          }
          NoSymbol
        }
        if (phase.erasedTypes && qual.isInstanceOf[Super] && tree.symbol != NoSymbol)
          qual setType tree.symbol.owner.tpe

        if (!reallyExists(sym)) {
          def handleMissing: Tree = {
            def errorTree = missingSelectErrorTree(tree, qual, name)
            def asTypeSelection = (
              if (context.unit.isJava && name.isTypeName) {
                // SI-3120 Java uses the same syntax, A.B, to express selection from the
                // value A and from the type A. We have to try both.
                atPos(tree.pos)(gen.convertToSelectFromType(qual, name)) match {
                  case EmptyTree => None
                  case tree1     => Some(typed1(tree1, mode, pt))
                }
              }
              else None
            )
            debuglog(s"""
              |qual=$qual:${qual.tpe}
              |symbol=${qual.tpe.termSymbol.defString}
              |scope-id=${qual.tpe.termSymbol.info.decls.hashCode}
              |members=${qual.tpe.members mkString ", "}
              |name=$name
              |found=$sym
              |owner=${context.enclClass.owner}
              """.stripMargin)

            // 1) Try converting a term selection on a java class into a type selection.
            // 2) Try expanding according to Dynamic rules.
            // 3) Try looking up the name in the qualifier.
            asTypeSelection orElse asDynamicCall getOrElse (lookupInQualifier(qual, name) match {
              case NoSymbol => setError(errorTree)
              case found    => typed1(tree setSymbol found, mode, pt)
            })
          }
          handleMissing
        }
        else {
          val tree1 = tree match {
            case Select(_, _) => treeCopy.Select(tree, qual, name)
            case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
          }
          val (result, accessibleError) = silent(_.makeAccessible(tree1, sym, qual.tpe, qual)) match {
            case SilentTypeError(err: AccessTypeError) =>
              (tree1, Some(err))
            case SilentTypeError(err) =>
              SelectWithUnderlyingError(tree, err)
              return tree
            case SilentResultValue(treeAndPre) =>
              (stabilize(treeAndPre._1, treeAndPre._2, mode, pt), None)
          }

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
              // don't adapt constructor, SI-6074
              val qual1 = if (name == nme.CONSTRUCTOR) qual
                          else adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = false, saveErrors = false)
              if (!qual1.isErrorTyped && (qual1 ne qual))
                typed(Select(qual1, name) setPos tree.pos, mode, pt)
              else
                // before failing due to access, try a dynamic call.
                asDynamicCall getOrElse {
                  context.issue(accessibleError.get)
                  setError(tree)
                }
            case _ =>
              result
          }
        }
      }

      // temporarily use `filter` as an alternative for `withFilter`
      def tryWithFilterAndFilter(tree: Select, qual: Tree): Tree = {
        def warn(sym: Symbol) = context.deprecationWarning(tree.pos, sym, s"`withFilter' method does not yet exist on ${qual.tpe.widen}, using `filter' method instead")
        silent(_ => typedSelect(tree, qual, nme.withFilter)) orElse { _ =>
          silent(_ => typed1(Select(qual, nme.filter) setPos tree.pos, mode, pt)) match {
            case SilentResultValue(res) => warn(res.symbol) ; res
            case SilentTypeError(err)   => WithFilterError(tree, err)
          }
        }
      }
      def typedSelectOrSuperCall(tree: Select) = tree match {
        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          // the qualifier type of a supercall constructor is its first parent class
          typedSelect(tree, typedSelectOrSuperQualifier(qual), nme.CONSTRUCTOR)
        case Select(qual, name) =>
          if (Statistics.canEnable) Statistics.incCounter(typedSelectCount)
          val qualTyped = checkDead(typedQualifier(qual, mode))
          val qualStableOrError = (
            if (qualTyped.isErrorTyped || !name.isTypeName || treeInfo.admitsTypeSelection(qualTyped))
              qualTyped
            else
              UnstableTreeError(qualTyped)
          )
          val tree1 = name match {
            case nme.withFilter if !settings.future => tryWithFilterAndFilter(tree, qualStableOrError)
            case _              => typedSelect(tree, qualStableOrError, name)
          }
          def sym = tree1.symbol
          if (tree.isInstanceOf[PostfixSelect])
            checkFeature(tree.pos, PostfixOpsFeature, name.decode)
          if (sym != null && sym.isOnlyRefinementMember && !sym.isMacro)
            checkFeature(tree1.pos, ReflectiveCallsFeature, sym.toString)

          qualStableOrError.symbol match {
            case s: Symbol if s.isRootPackage => treeCopy.Ident(tree1, name)
            case _                            => tree1
          }
      }

      /* A symbol qualifies if:
       *  - it exists
       *  - it is not stale (stale symbols are made to disappear here)
       *  - if we are in a constructor pattern, method definitions do not qualify
       *    unless they are stable.  Otherwise, 'case x :: xs' would find the :: method.
       */
      def qualifies(sym: Symbol) = (
           sym.hasRawInfo
        && reallyExists(sym)
        && !(mode.typingConstructorPattern && sym.isMethod && !sym.isStable)
      )

      /* Attribute an identifier consisting of a simple name or an outer reference.
       *
       * @param tree      The tree representing the identifier.
       * @param name      The name of the identifier.
       * Transformations: (1) Prefix class members with this.
       *                  (2) Change imported symbols to selections
       */
      def typedIdent(tree: Tree, name: Name): Tree = {
        // setting to enable unqualified idents in empty package (used by the repl)
        def inEmptyPackage = if (settings.exposeEmptyPackage) lookupInEmpty(name) else NoSymbol

        def issue(err: AbsTypeError) = {
          // Avoiding some spurious error messages: see SI-2388.
          val suppress = reporter.hasErrors && (name startsWith tpnme.ANON_CLASS_NAME)
          if (!suppress)
            ErrorUtils.issueTypeError(err)

          setError(tree)
        }
          // ignore current variable scope in patterns to enforce linearity
        val startContext = if (mode.typingPatternOrTypePat) context.outer else context
        val nameLookup   = tree.symbol match {
          case NoSymbol   => startContext.lookupSymbol(name, qualifies)
          case sym        => LookupSucceeded(EmptyTree, sym)
        }
        import InferErrorGen._
        nameLookup match {
          case LookupAmbiguous(msg)         => issue(AmbiguousIdentError(tree, name, msg))
          case LookupInaccessible(sym, msg) => issue(AccessError(tree, sym, context, msg))
          case LookupNotFound               =>
            inEmptyPackage orElse lookupInRoot(name) match {
              case NoSymbol => issue(SymbolNotFoundError(tree, name, context.owner, startContext))
              case sym      => typed1(tree setSymbol sym, mode, pt)
                }
          case LookupSucceeded(qual, sym)   =>
            (// this -> Foo.this
            if (sym.isThisSym)
              typed1(This(sym.owner) setPos tree.pos, mode, pt)
            else if (isPredefClassOf(sym) && pt.typeSymbol == ClassClass && pt.typeArgs.nonEmpty) {
              // Inferring classOf type parameter from expected type.  Otherwise an
              // actual call to the stubbed classOf method is generated, returning null.
              typedClassOf(tree, TypeTree(pt.typeArgs.head).setPos(tree.pos.focus))
            }
          else {
              val pre1  = if (sym.isTopLevel) sym.owner.thisType else if (qual == EmptyTree) NoPrefix else qual.tpe
              val tree1 = if (qual == EmptyTree) tree else atPos(tree.pos)(Select(atPos(tree.pos.focusStart)(qual), name))
              val (tree2, pre2) = makeAccessible(tree1, sym, pre1, qual)
            // SI-5967 Important to replace param type A* with Seq[A] when seen from from a reference, to avoid
            //         inference errors in pattern matching.
              stabilize(tree2, pre2, mode, pt) modifyType dropIllegalStarTypes
            }) setAttachments tree.attachments
          }
        }

      def typedIdentOrWildcard(tree: Ident) = {
        val name = tree.name
        if (Statistics.canEnable) Statistics.incCounter(typedIdentCount)
        if ((name == nme.WILDCARD && mode.typingPatternNotConstructor) ||
            (name == tpnme.WILDCARD && mode.inTypeMode))
          tree setType makeFullyDefined(pt)
        else
          typedIdent(tree, name)
      }

      def typedCompoundTypeTree(tree: CompoundTypeTree) = {
        val templ = tree.templ
        val parents1 = templ.parents mapConserve (typedType(_, mode))

        // This is also checked later in typedStats, but that is too late for SI-5361, so
        // we eagerly check this here.
        for (stat <- templ.body if !treeInfo.isDeclarationOrTypeDef(stat))
          OnlyDeclarationsError(stat)

        if ((parents1 ++ templ.body) exists (_.isErrorTyped)) tree setType ErrorType
        else {
          val decls = newScope
          //Console.println("Owner: " + context.enclClass.owner + " " + context.enclClass.owner.id)
          val self = refinedType(parents1 map (_.tpe), context.enclClass.owner, decls, templ.pos)
          newTyper(context.make(templ, self.typeSymbol, decls)).typedRefinement(templ)
          templ updateAttachment CompoundTypeTreeOriginalAttachment(parents1, Nil) // stats are set elsewhere
          tree setType (if (templ.exists(_.isErroneous)) ErrorType else self) // Being conservative to avoid SI-5361
        }
      }

      def typedAppliedTypeTree(tree: AppliedTypeTree) = {
        val tpt        = tree.tpt
        val args       = tree.args
        val tpt1       = typed1(tpt, mode | FUNmode | TAPPmode, WildcardType)
        def isPoly     = tpt1.tpe.isInstanceOf[PolyType]
        def isComplete = tpt1.symbol.rawInfo.isComplete

        if (tpt1.isErrorTyped) {
          tpt1
        } else if (!tpt1.hasSymbolField) {
          AppliedTypeNoParametersError(tree, tpt1.tpe)
        } else {
          val tparams = tpt1.symbol.typeParams

          if (sameLength(tparams, args)) {
            // @M: kind-arity checking is done here and in adapt, full kind-checking is in checkKindBounds (in Infer)
            val args1 = map2Conserve(args, tparams) { (arg, tparam) =>
              def ptParams = Kind.FromParams(tparam.typeParams)

              // if symbol hasn't been fully loaded, can't check kind-arity except when we're in a pattern,
              // where we can (we can't take part in F-Bounds) and must (SI-8023)
              val pt = if (mode.typingPatternOrTypePat) {
                tparam.initialize; ptParams
              }
              else if (isComplete) ptParams
              else Kind.Wildcard

              typedHigherKindedType(arg, mode, pt)
            }
            val argtypes = mapList(args1)(treeTpe)

            foreach2(args, tparams) { (arg, tparam) =>
              // note: can't use args1 in selector, because Binds got replaced
              val asym = arg.symbol
              def abounds = asym.info.bounds
              def tbounds = tparam.info.bounds
              def enhanceBounds(): Unit = {
                val TypeBounds(lo0, hi0) = abounds
                val TypeBounds(lo1, hi1) = tbounds.subst(tparams, argtypes)
                val lo = lub(List(lo0, lo1))
                val hi = glb(List(hi0, hi1))
                if (!(lo =:= lo0 && hi =:= hi0))
                  asym setInfo logResult(s"Updating bounds of ${asym.fullLocationString} in $tree from '$abounds' to")(TypeBounds(lo, hi))
              }
              if (asym != null && asym.isAbstractType) {
                arg match {
                  // I removed the Ident() case that partially fixed SI-1786,
                  // because the stricter bounds being inferred broke e.g., slick
                  // worse, the fix was compilation order-dependent
                  // sharpenQuantifierBounds (used in skolemizeExistential) has an alternative fix (SI-6169) that's less invasive
                  case Bind(_, _) => enhanceBounds()
                  case _          =>
                }
              }
            }
            val original = treeCopy.AppliedTypeTree(tree, tpt1, args1)
            val result = TypeTree(appliedType(tpt1.tpe, argtypes)) setOriginal original
            if (isPoly) // did the type application (performed by appliedType) involve an unchecked beta-reduction?
              TypeTreeWithDeferredRefCheck(){ () =>
                // wrap the tree and include the bounds check -- refchecks will perform this check (that the beta reduction was indeed allowed) and unwrap
                // we can't simply use original in refchecks because it does not contains types
                // (and the only typed trees we have been mangled so they're not quite the original tree anymore)
                checkBounds(result, tpt1.tpe.prefix, tpt1.symbol.owner, tpt1.symbol.typeParams, argtypes, "")
                result // you only get to see the wrapped tree after running this check :-p
              } setType (result.tpe) setPos(result.pos)
            else result
          } else if (tparams.isEmpty) {
            AppliedTypeNoParametersError(tree, tpt1.tpe)
          } else {
            //Console.println("\{tpt1}:\{tpt1.symbol}:\{tpt1.symbol.info}")
            if (settings.debug) Console.println(tpt1+":"+tpt1.symbol+":"+tpt1.symbol.info)//debug
            AppliedTypeWrongNumberOfArgsError(tree, tpt1, tparams)
          }
        }
      }

      val sym: Symbol = tree.symbol
      if ((sym ne null) && (sym ne NoSymbol)) sym.initialize

      def typedPackageDef(pdef0: PackageDef) = {
        val pdef = treeCopy.PackageDef(pdef0, pdef0.pid, pluginsEnterStats(this, pdef0.stats))
        val pid1 = typedQualifier(pdef.pid).asInstanceOf[RefTree]
        assert(sym.moduleClass ne NoSymbol, sym)
        val stats1 = newTyper(context.make(tree, sym.moduleClass, sym.info.decls))
          .typedStats(pdef.stats, NoSymbol)
        treeCopy.PackageDef(tree, pid1, stats1) setType NoType
      }

      /*
       * The typer with the correct context for a method definition. If the method is a default getter for
       * a constructor default, the resulting typer has a constructor context (fixes SI-5543).
       */
      def defDefTyper(ddef: DefDef) = {
        val isConstrDefaultGetter = ddef.mods.hasDefault && sym.owner.isModuleClass &&
            nme.defaultGetterToMethod(sym.name) == nme.CONSTRUCTOR
        newTyper(context.makeNewScope(ddef, sym)).constrTyperIf(isConstrDefaultGetter)
      }

      def typedAlternative(alt: Alternative) = {
        context withinPatAlternative (
          treeCopy.Alternative(tree, alt.trees mapConserve (alt => typed(alt, mode, pt))) setType pt
        )
      }
      def typedStar(tree: Star) = {
        if (!context.starPatterns && !isPastTyper)
          StarPatternWithVarargParametersError(tree)

        treeCopy.Star(tree, typed(tree.elem, mode, pt)) setType makeFullyDefined(pt)
      }
      def issueTryWarnings(tree: Try): Try = {
        def checkForCatchAll(cdef: CaseDef) {
          def unbound(t: Tree) = t.symbol == null || t.symbol == NoSymbol
          def warn(name: Name) = {
            val msg = s"This catches all Throwables. If this is really intended, use `case ${name.decoded} : Throwable` to clear this warning."
            context.warning(cdef.pat.pos, msg)
          }
          if (cdef.guard.isEmpty) cdef.pat match {
            case Bind(name, i @ Ident(_)) if unbound(i) => warn(name)
            case i @ Ident(name) if unbound(i)          => warn(name)
            case _                                      =>
          }
        }
        if (!isPastTyper) tree match {
          case Try(_, Nil, fin) =>
            if (fin eq EmptyTree)
              context.warning(tree.pos, "A try without a catch or finally is equivalent to putting its body in a block; no exceptions are handled.")
          case Try(_, catches, _) =>
            catches foreach checkForCatchAll
        }
        tree
      }

      def typedTry(tree: Try) = {
        val Try(block, catches, fin) = tree
        val block1   = typed(block, pt)
        val catches1 = typedCases(catches, ThrowableTpe, pt)
        val fin1     = if (fin.isEmpty) fin else typed(fin, UnitTpe)

        def finish(ownType: Type) = treeCopy.Try(tree, block1, catches1, fin1) setType ownType

        issueTryWarnings(
          if (isFullyDefined(pt))
            finish(pt)
          else block1 :: catches1 map (_.tpe.deconst) match {
            case tpes if sameWeakLubAsLub(tpes) => finish(lub(tpes))
            case tpes                           =>
              val lub      = weakLub(tpes)
              val block2   = adapt(block1, mode, lub)
              val catches2 = catches1 map (adaptCase(_, mode, lub))
              treeCopy.Try(tree, block2, catches2, fin1) setType lub
          }
        )
      }

      def typedThrow(tree: Throw) = {
        val expr1 = typedByValueExpr(tree.expr, ThrowableTpe)
        treeCopy.Throw(tree, expr1) setType NothingTpe
      }

      def typedTyped(tree: Typed) = {
        if (treeInfo isWildcardStarType tree.tpt)
          typedStarInPattern(tree, mode.onlySticky, pt)
        else if (mode.inPatternMode)
          typedInPattern(tree, mode.onlySticky, pt)
        else tree match {
          // find out whether the programmer is trying to eta-expand a macro def
          // to do that we need to typecheck the tree first (we need a symbol of the eta-expandee)
          // that typecheck must not trigger macro expansions, so we explicitly prohibit them
          // however we cannot do `context.withMacrosDisabled`
          // because `expr` might contain nested macro calls (see SI-6673)
          //
          // Note: apparently `Function(Nil, EmptyTree)` is the secret parser marker
          // which means trailing underscore.
          case Typed(expr, Function(Nil, EmptyTree)) =>
            typed1(suppressMacroExpansion(expr), mode, pt) match {
              case macroDef if treeInfo.isMacroApplication(macroDef) => MacroEtaError(macroDef)
              case exprTyped                                         => typedEta(checkDead(exprTyped))
            }
          case Typed(expr, tpt) =>
            val tpt1  = typedType(tpt, mode)                           // type the ascribed type first
            val expr1 = typed(expr, mode.onlySticky, tpt1.tpe.deconst) // then type the expression with tpt1 as the expected type
            treeCopy.Typed(tree, expr1, tpt1) setType tpt1.tpe
        }
      }

      def typedTypeApply(tree: TypeApply) = {
        val fun = tree.fun
        val args = tree.args
        // @M: kind-arity checking is done here and in adapt, full kind-checking is in checkKindBounds (in Infer)
        //@M! we must type fun in order to type the args, as that requires the kinds of fun's type parameters.
        // However, args should apparently be done first, to save context.undetparams. Unfortunately, the args
        // *really* have to be typed *after* fun. We escape from this classic Catch-22 by simply saving&restoring undetparams.

        // @M TODO: the compiler still bootstraps&all tests pass when this is commented out..
        //val undets = context.undetparams

        // @M: fun is typed in TAPPmode because it is being applied to its actual type parameters
        val fun1 = typed(fun, mode.forFunMode | TAPPmode)
        val tparams = if (fun1.symbol == null) Nil else fun1.symbol.typeParams

        //@M TODO: val undets_fun = context.undetparams  ?
        // "do args first" (by restoring the context.undetparams) in order to maintain context.undetparams on the function side.

        // @M TODO: the compiler still bootstraps when this is commented out.. TODO: run tests
        //context.undetparams = undets

        // @M maybe the well-kindedness check should be done when checking the type arguments conform to the type parameters' bounds?
        val args1 = if (sameLength(args, tparams)) map2Conserve(args, tparams) {
          (arg, tparam) => typedHigherKindedType(arg, mode, Kind.FromParams(tparam.typeParams))
        }
        else {
          //@M  this branch is correctly hit for an overloaded polymorphic type. It also has to handle erroneous cases.
          // Until the right alternative for an overloaded method is known, be very liberal,
          // typedTypeApply will find the right alternative and then do the same check as
          // in the then-branch above. (see pos/tcpoly_overloaded.scala)
          // this assert is too strict: be tolerant for errors like trait A { def foo[m[x], g]=error(""); def x[g] = foo[g/*ERR: missing argument type*/] }
          //assert(fun1.symbol.info.isInstanceOf[OverloadedType] || fun1.symbol.isError) //, (fun1.symbol,fun1.symbol.info,fun1.symbol.info.getClass,args,tparams))
          args mapConserve (typedHigherKindedType(_, mode))
        }

        //@M TODO: context.undetparams = undets_fun ?
        Typer.this.typedTypeApply(tree, mode, fun1, args1)
      }

      def typedApplyDynamic(tree: ApplyDynamic) = {
        assert(phase.erasedTypes)
        val qual1 = typed(tree.qual, AnyRefTpe)
        val args1 = tree.args mapConserve (arg => typed(arg, AnyRefTpe))
        treeCopy.ApplyDynamic(tree, qual1, args1) setType AnyRefTpe
      }

      def typedReferenceToBoxed(tree: ReferenceToBoxed) = {
        val id = tree.ident
        val id1 = typed1(id, mode, pt) match { case id: Ident => id }
        // [Eugene] am I doing it right?
        val erasedTypes = phaseId(currentPeriod) >= currentRun.erasurePhase.id
        val tpe = capturedVariableType(id.symbol, erasedTypes = erasedTypes)
        treeCopy.ReferenceToBoxed(tree, id1) setType tpe
      }

      // Warn about likely interpolated strings which are missing their interpolators
      def warnMissingInterpolator(lit: Literal): Unit = if (!isPastTyper) {
        // attempt to avoid warning about trees munged by macros
        def isMacroExpansion = {
          // context.tree is not the expandee; it is plain new SC(ps).m(args)
          //context.tree exists (t => (t.pos includes lit.pos) && hasMacroExpansionAttachment(t))
          // testing pos works and may suffice
          //openMacros exists (_.macroApplication.pos includes lit.pos)
          // tests whether the lit belongs to the expandee of an open macro
          openMacros exists (_.macroApplication.attachments.get[MacroExpansionAttachment] match {
            case Some(MacroExpansionAttachment(_, t: Tree)) => t exists (_ == lit)
            case _                                          => false
          })
        }
        // attempt to avoid warning about the special interpolated message string
        // for implicitNotFound or any standard interpolation (with embedded $$).
        def isRecognizablyNotForInterpolation = context.enclosingApply.tree match {
          case Apply(Select(Apply(RefTree(_, nme.StringContext), _), _), _) => true
          case Apply(Select(New(RefTree(_, tpnme.implicitNotFound)), _), _) => true
          case _                                                            => isMacroExpansion
        }
        def requiresNoArgs(tp: Type): Boolean = tp match {
          case PolyType(_, restpe)     => requiresNoArgs(restpe)
          case MethodType(Nil, restpe) => requiresNoArgs(restpe)  // may be a curried method - can't tell yet
          case MethodType(p :: _, _)   => p.isImplicit            // implicit method requires no args
          case _                       => true                    // catches all others including NullaryMethodType
        }
        def isPlausible(m: Symbol) = m.alternatives exists (m => requiresNoArgs(m.info))

        def maybeWarn(s: String): Unit = {
          def warn(message: String)         = context.warning(lit.pos, s"possible missing interpolator: $message")
          def suspiciousSym(name: TermName) = context.lookupSymbol(name, _ => true).symbol
          def suspiciousExpr                = InterpolatorCodeRegex findFirstIn s
          def suspiciousIdents              = InterpolatorIdentRegex findAllIn s map (s => suspiciousSym(TermName(s drop 1)))

          if (suspiciousExpr.nonEmpty)
            warn("detected an interpolated expression") // "${...}"
          else
            suspiciousIdents find isPlausible foreach (sym => warn(s"detected interpolated identifier `$$${sym.name}`")) // "$id"
        }
        lit match {
          case Literal(Constant(s: String)) if !isRecognizablyNotForInterpolation => maybeWarn(s)
          case _                                                                  =>
        }
      }

      def typedLiteral(tree: Literal) = {
        if (settings.warnMissingInterpolator) warnMissingInterpolator(tree)

        tree setType (if (tree.value.tag == UnitTag) UnitTpe else ConstantType(tree.value))
      }

      def typedSingletonTypeTree(tree: SingletonTypeTree) = {
        val refTyped =
          context.withImplicitsDisabled {
            typed(tree.ref, MonoQualifierModes | mode.onlyTypePat, AnyRefTpe)
          }

        if (refTyped.isErrorTyped) {
          setError(tree)
        } else {
          tree setType refTyped.tpe.resultType
          if (refTyped.isErrorTyped || treeInfo.admitsTypeSelection(refTyped)) tree
          else UnstableTreeError(tree)
        }
      }

      def typedSelectFromTypeTree(tree: SelectFromTypeTree) = {
        val qual1 = typedType(tree.qualifier, mode)
        if (qual1.isErrorTyped) setError(treeCopy.SelectFromTypeTree(tree, qual1, tree.name))
        else if (qual1.tpe.isVolatile) TypeSelectionFromVolatileTypeError(tree, qual1)
        else typedSelect(tree, qual1, tree.name)
      }

      def typedTypeBoundsTree(tree: TypeBoundsTree) = {
        val lo1 = if (tree.lo.isEmpty) TypeTree(NothingTpe) else typedType(tree.lo, mode)
        val hi1 = if (tree.hi.isEmpty) TypeTree(AnyTpe) else typedType(tree.hi, mode)
        treeCopy.TypeBoundsTree(tree, lo1, hi1) setType TypeBounds(lo1.tpe, hi1.tpe)
      }

      def typedExistentialTypeTree(tree: ExistentialTypeTree) = {
        val tree1 = typerWithLocalContext(context.makeNewScope(tree, context.owner)){
          typer =>
            if (context.inTypeConstructorAllowed)
              typer.context.withinTypeConstructorAllowed(typer.typedExistentialTypeTree(tree, mode))
            else
              typer.typedExistentialTypeTree(tree, mode)
        }
        checkExistentialsFeature(tree1.pos, tree1.tpe, "the existential type")
        tree1
      }

      def typedTypeTree(tree: TypeTree) = {
        if (tree.original != null) {
          val newTpt = typedType(tree.original, mode)
          tree setType newTpt.tpe
          newTpt match {
            case tt @ TypeTree() => tree setOriginal tt.original
            case _ => tree
          }
        }
        else {
          // we should get here only when something before failed
          // and we try again (@see tryTypedApply). In that case we can assign
          // whatever type to tree; we just have to survive until a real error message is issued.
          devWarning(tree.pos, s"Assigning Any type to TypeTree because tree.original is null: tree is $tree/${System.identityHashCode(tree)}, sym=${tree.symbol}, tpe=${tree.tpe}")
          tree setType AnyTpe
        }
      }
      def typedFunction(fun: Function) = {
        if (fun.symbol == NoSymbol)
          fun.symbol = context.owner.newAnonymousFunctionValue(fun.pos)

        typerWithLocalContext(context.makeNewScope(fun, fun.symbol))(_.typedFunction(fun, mode, pt))
      }

      // Trees only allowed during pattern mode.
      def typedInPatternMode(tree: Tree): Tree = tree match {
        case tree: Alternative => typedAlternative(tree)
        case tree: Star        => typedStar(tree)
        case _                 => abort(s"unexpected tree in pattern mode: ${tree.getClass}\n$tree")
      }

      def typedTypTree(tree: TypTree): Tree = tree match {
        case tree: TypeTree                     => typedTypeTree(tree)
        case tree: AppliedTypeTree              => typedAppliedTypeTree(tree)
        case tree: TypeBoundsTree               => typedTypeBoundsTree(tree)
        case tree: SingletonTypeTree            => typedSingletonTypeTree(tree)
        case tree: SelectFromTypeTree           => typedSelectFromTypeTree(tree)
        case tree: CompoundTypeTree             => typedCompoundTypeTree(tree)
        case tree: ExistentialTypeTree          => typedExistentialTypeTree(tree)
        case tree: TypeTreeWithDeferredRefCheck => tree // TODO: retype the wrapped tree? TTWDRC would have to change to hold the wrapped tree (not a closure)
        case _                                  => abort(s"unexpected type-representing tree: ${tree.getClass}\n$tree")
      }

      def typedMemberDef(tree: MemberDef): Tree = tree match {
        case tree: ValDef     => typedValDef(tree)
        case tree: DefDef     => defDefTyper(tree).typedDefDef(tree)
        case tree: ClassDef   => newTyper(context.makeNewScope(tree, sym)).typedClassDef(tree)
        case tree: ModuleDef  => newTyper(context.makeNewScope(tree, sym.moduleClass)).typedModuleDef(tree)
        case tree: TypeDef    => typedTypeDef(tree)
        case tree: PackageDef => typedPackageDef(tree)
        case _                => abort(s"unexpected member def: ${tree.getClass}\n$tree")
      }

      // Trees not allowed during pattern mode.
      def typedOutsidePatternMode(tree: Tree): Tree = tree match {
        case tree: Block            => typerWithLocalContext(context.makeNewScope(tree, context.owner))(_.typedBlock(tree, mode, pt))
        case tree: If               => typedIf(tree)
        case tree: TypeApply        => typedTypeApply(tree)
        case tree: Function         => typedFunction(tree)
        case tree: Match            => typedVirtualizedMatch(tree)
        case tree: New              => typedNew(tree)
        case tree: Assign           => typedAssign(tree.lhs, tree.rhs)
        case tree: AssignOrNamedArg => typedAssign(tree.lhs, tree.rhs) // called by NamesDefaults in silent typecheck
        case tree: Super            => typedSuper(tree)
        case tree: Annotated        => typedAnnotated(tree)
        case tree: Return           => typedReturn(tree)
        case tree: Try              => typedTry(tree)
        case tree: Throw            => typedThrow(tree)
        case tree: ArrayValue       => typedArrayValue(tree)
        case tree: ApplyDynamic     => typedApplyDynamic(tree)
        case tree: ReferenceToBoxed => typedReferenceToBoxed(tree)
        case tree: LabelDef         => labelTyper(tree).typedLabelDef(tree)
        case tree: DocDef           => typedDocDef(tree, mode, pt)
        case _                      => abort(s"unexpected tree: ${tree.getClass}\n$tree")
      }

      // Trees allowed in or out of pattern mode.
      def typedInAnyMode(tree: Tree): Tree = tree match {
        case tree: Ident   => typedIdentOrWildcard(tree)
        case tree: Bind    => typedBind(tree)
        case tree: Apply   => typedApply(tree)
        case tree: Select  => typedSelectOrSuperCall(tree)
        case tree: Literal => typedLiteral(tree)
        case tree: Typed   => typedTyped(tree)
        case tree: This    => typedThis(tree)  // SI-6104
        case tree: UnApply => abort(s"unexpected UnApply $tree") // turns out UnApply never reaches here
        case _             =>
          if (mode.inPatternMode)
            typedInPatternMode(tree)
          else
            typedOutsidePatternMode(tree)
      }

      // begin typed1
      tree match {
        case tree: TypTree   => typedTypTree(tree)
        case tree: MemberDef => typedMemberDef(tree)
        case _               => typedInAnyMode(tree)
      }
    }

    def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
      lastTreeToTyper = tree
      def body = (
        if (printTypings && !phase.erasedTypes && !noPrintTyping(tree))
          typingStack.nextTyped(tree, mode, pt, context)(typedInternal(tree, mode, pt))
        else
          typedInternal(tree, mode, pt)
      )
      val startByType = if (Statistics.canEnable) Statistics.pushTimer(byTypeStack, byTypeNanos(tree.getClass)) else null
      if (Statistics.canEnable) Statistics.incCounter(visitsByType, tree.getClass)
      try body
      finally if (Statistics.canEnable) Statistics.popTimer(byTypeStack, startByType)
    }

    private def typedInternal(tree: Tree, mode: Mode, pt: Type): Tree = {
      val ptPlugins = pluginsPt(pt, this, tree, mode)
      def retypingOk = (
            context.retyping
        && (tree.tpe ne null)
        && (tree.tpe.isErroneous || !(tree.tpe <:< ptPlugins))
      )
      def runTyper(): Tree = {
        if (retypingOk) {
          tree.setType(null)
          if (tree.hasSymbolField) tree.symbol = NoSymbol
        }
        val alreadyTyped = tree.tpe ne null
        val shouldPrint = !alreadyTyped && !phase.erasedTypes
        val ptWild = if (mode.inPatternMode)
          ptPlugins // SI-5022 don't widen pt for patterns as types flow from it to the case body.
        else
          dropExistential(ptPlugins) // FIXME: document why this is done.
        val tree1: Tree = if (alreadyTyped) tree else typed1(tree, mode, ptWild)
        if (shouldPrint)
          typingStack.showTyped(tree1)

        // Can happen during erroneous compilation - error(s) have been
        // reported, but we need to avoid causing an NPE with this tree
        if (tree1.tpe eq null)
          return setError(tree)

        tree1 modifyType (pluginsTyped(_, this, tree1, mode, ptPlugins))

        val result =
          if (tree1.isEmpty) tree1
          else {
            val result = adapt(tree1, mode, ptPlugins, tree)
            if (hasPendingMacroExpansions) macroExpandAll(this, result) else result
          }

        if (shouldPrint)
          typingStack.showAdapt(tree1, result, ptPlugins, context)

        if (!isPastTyper)
          signalDone(context.asInstanceOf[analyzer.Context], tree, result)

        result
      }

      try runTyper() catch {
        case ex: TypeError =>
          tree.clearType()
          // The only problematic case are (recoverable) cyclic reference errors which can pop up almost anywhere.
          typingStack.printTyping(tree, "caught %s: while typing %s".format(ex, tree)) //DEBUG
          reportTypeError(context, tree.pos, ex)
          setError(tree)
        case ex: Exception =>
          // @M causes cyclic reference error
          devWarning(s"exception when typing $tree, pt=$ptPlugins")
          if (context != null && context.unit.exists && tree != null)
            logError("AT: " + tree.pos, ex)
          throw ex
      }
    }

    def atOwner(owner: Symbol): Typer =
      newTyper(context.make(owner = owner))

    def atOwner(tree: Tree, owner: Symbol): Typer =
      newTyper(context.make(tree, owner))

    /** Types expression or definition `tree`.
     */
    def typed(tree: Tree): Tree = {
      val ret = typed(tree, context.defaultModeForTyped, WildcardType)
      ret
    }

    def typedByValueExpr(tree: Tree, pt: Type = WildcardType): Tree = typed(tree, EXPRmode | BYVALmode, pt)

    def typedPos(pos: Position, mode: Mode, pt: Type)(tree: Tree) = typed(atPos(pos)(tree), mode, pt)
    def typedPos(pos: Position)(tree: Tree) = typed(atPos(pos)(tree))
    // TODO: see if this formulation would impose any penalty, since
    // it makes for a lot less casting.
    // def typedPos[T <: Tree](pos: Position)(tree: T): T = typed(atPos(pos)(tree)).asInstanceOf[T]

    /** Types expression `tree` with given prototype `pt`.
     */
    def typed(tree: Tree, pt: Type): Tree =
      typed(tree, context.defaultModeForTyped, pt)

    def typed(tree: Tree, mode: Mode): Tree =
      typed(tree, mode, WildcardType)

    /** Types qualifier `tree` of a select node.
     *  E.g. is tree occurs in a context like `tree.m`.
     */
    def typedQualifier(tree: Tree, mode: Mode, pt: Type): Tree =
      typed(tree, PolyQualifierModes | mode.onlyTypePat, pt) // TR: don't set BYVALmode, since qualifier might end up as by-name param to an implicit

    /** Types qualifier `tree` of a select node.
     *  E.g. is tree occurs in a context like `tree.m`.
     */
    def typedQualifier(tree: Tree, mode: Mode): Tree =
      typedQualifier(tree, mode, WildcardType)

    def typedQualifier(tree: Tree): Tree = typedQualifier(tree, NOmode, WildcardType)

    /** Types function part of an application */
    def typedOperator(tree: Tree): Tree = typed(tree, OperatorModes)

    // the qualifier type of a supercall constructor is its first parent class
    private def typedSelectOrSuperQualifier(qual: Tree) =
      context withinSuperInit typed(qual, PolyQualifierModes)

    /** Types a pattern with prototype `pt` */
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
      typingInPattern(context.withImplicitsDisabledAllowEnrichment(typed(tree, PATTERNmode, pt))) match {
        case tpt if tpt.isType => PatternMustBeValue(tpt, pt); tpt
        case pat               => pat
      }
    }

    /** Types a (fully parameterized) type tree */
    def typedType(tree: Tree, mode: Mode): Tree =
      typed(tree, mode.forTypeMode, WildcardType)

    /** Types a (fully parameterized) type tree */
    def typedType(tree: Tree): Tree = typedType(tree, NOmode)

    /** Types a higher-kinded type tree -- pt denotes the expected kind and must be one of `Kind.WildCard` and `Kind.FromParams` */
    def typedHigherKindedType(tree: Tree, mode: Mode, pt: Type): Tree =
      if (pt != Kind.Wildcard && pt.typeParams.isEmpty) typedType(tree, mode) // kind is known and it's *
      else context withinTypeConstructorAllowed typed(tree, NOmode, pt)

    def typedHigherKindedType(tree: Tree, mode: Mode): Tree =
      context withinTypeConstructorAllowed typed(tree)

    /** Types a type constructor tree used in a new or supertype */
    def typedTypeConstructor(tree: Tree, mode: Mode): Tree = {
      val result = typed(tree, mode.forTypeMode | FUNmode, WildcardType)

      // get rid of type aliases for the following check (#1241)
      result.tpe.dealias match {
        case restpe @ TypeRef(pre, _, _) if !phase.erasedTypes && !pre.isStable && !context.unit.isJava =>
          // The isJava exception if OK only because the only type constructors scalac gets
          // to see are those in the signatures. These do not need a unique object as a prefix.
          // The situation is different for new's and super's, but scalac does not look deep
          // enough to see those. See #3938
          ConstructorPrefixError(tree, restpe)
        case _ =>
          // must not normalize: type application must be (bounds-)checked (during RefChecks), see #2208
          // during uncurry (after refchecks), all types are normalized
          result
      }
    }

    def typedTypeConstructor(tree: Tree): Tree = typedTypeConstructor(tree, NOmode)

    def computeType(tree: Tree, pt: Type): Type = {
      // macros employ different logic of `computeType`
      assert(!context.owner.isMacro, context.owner)
      val tree1 = typed(tree, pt)
      transformed(tree) = tree1
      val tpe = packedType(tree1, context.owner)
      checkExistentialsFeature(tree.pos, tpe, "inferred existential type")
      tpe
    }

    def computeMacroDefType(ddef: DefDef, pt: Type): Type = {
      assert(context.owner.isMacro, context.owner)
      assert(ddef.symbol.isMacro, ddef.symbol)

      val rhs1 =
        if (transformed contains ddef.rhs) {
          // macro defs are typechecked in `methodSig` (by calling this method) in order to establish their link to macro implementation asap
          // if a macro def doesn't have explicitly specified return type, this method will be called again by `assignTypeToTree`
          // here we guard against this case
          transformed(ddef.rhs)
        } else {
          val rhs1 = typedMacroBody(this, ddef)
          transformed(ddef.rhs) = rhs1
          rhs1
        }

      val isMacroBodyOkay = !ddef.symbol.isErroneous && !(rhs1 exists (_.isErroneous)) && rhs1 != EmptyTree
      val shouldInheritMacroImplReturnType = ddef.tpt.isEmpty
      if (isMacroBodyOkay && shouldInheritMacroImplReturnType) {
        val commonMessage = "macro defs must have explicitly specified return types"
        def reportFailure() = {
          ddef.symbol.setFlag(IS_ERROR)
          context.error(ddef.pos, commonMessage)
        }
        def reportWarning(inferredType: Type) = {
          val explanation = s"inference of $inferredType from macro impl's c.Expr[$inferredType] is deprecated and is going to stop working in 2.12"
          context.deprecationWarning(ddef.pos, ddef.symbol, s"$commonMessage ($explanation)")
        }
        computeMacroDefTypeFromMacroImplRef(ddef, rhs1) match {
          case ErrorType => ErrorType
          case NothingTpe => NothingTpe
          case NoType => reportFailure(); AnyTpe
          case tpe => reportWarning(tpe); tpe
        }
      } else AnyTpe
    }

    def transformedOr(tree: Tree, op: => Tree): Tree = transformed remove tree match {
      case Some(tree1) => tree1
      case _           => op
    }

    def transformedOrTyped(tree: Tree, mode: Mode, pt: Type): Tree = transformed remove tree match {
      case Some(tree1) => tree1
      case _           => typed(tree, mode, pt)
    }
  }
}

object TypersStats {
  import scala.reflect.internal.TypesStats._
  val typedIdentCount     = Statistics.newCounter("#typechecked identifiers")
  val typedSelectCount    = Statistics.newCounter("#typechecked selections")
  val typedApplyCount     = Statistics.newCounter("#typechecked applications")
  val rawTypeFailed       = Statistics.newSubCounter ("  of which in failed", rawTypeCount)
  val subtypeFailed       = Statistics.newSubCounter("  of which in failed", subtypeCount)
  val findMemberFailed    = Statistics.newSubCounter("  of which in failed", findMemberCount)
  val failedSilentNanos   = Statistics.newSubTimer("time spent in failed", typerNanos)
  val failedApplyNanos    = Statistics.newSubTimer("  failed apply", typerNanos)
  val failedOpEqNanos     = Statistics.newSubTimer("  failed op=", typerNanos)
  val isReferencedNanos   = Statistics.newSubTimer("time spent ref scanning", typerNanos)
  val visitsByType        = Statistics.newByClass("#visits by tree node", "typer")(Statistics.newCounter(""))
  val byTypeNanos         = Statistics.newByClass("time spent by tree node", "typer")(Statistics.newStackableTimer("", typerNanos))
  val byTypeStack         = Statistics.newTimerStack()
}
