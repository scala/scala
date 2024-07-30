/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.nsc
package typechecker

import scala.annotation._
import scala.collection.mutable
import mutable.ListBuffer
import scala.reflect.internal.{Chars, TypesStats}
import scala.reflect.internal.util.{CodeAction, FreshNameCreator, ListOfNil, Statistics}
import scala.tools.nsc.Reporting.{MessageFilter, Suppression, WConf, WarningCategory}, WarningCategory.Scala3Migration
import scala.util.chaining._
import symtab.Flags._
import Mode._

/** A provider of methods to assign types to trees.
 *
 *  @author  Martin Odersky
 */
trait Typers extends Adaptations with Tags with TypersTracking with PatternTypers {
  self: Analyzer =>

  import global._
  import definitions._
  import statistics._

  final def forArgMode(fun: Tree, mode: Mode) =
    if (treeInfo.isSelfOrSuperConstrCall(fun)) mode | SCCmode else mode

  final val shortenImports = false

  // All typechecked RHS of ValDefs for right-associative operator desugaring
  private val rightAssocValDefs = new mutable.AnyRefMap[Symbol, Tree]
  // Symbols of ValDefs for right-associative operator desugaring which are passed by name and have been inlined
  private val inlinedRightAssocValDefs = new mutable.HashSet[Symbol]

  // For each class, we collect a mapping from constructor param accessors that are aliases of their superclass
  // param accessors. At the end of the typer phase, when this information is available all the way up the superclass
  // chain, this is used to determine which are true aliases, ones where the field can be elided from this class.
  // And yes, if you were asking, this is yet another binary fragility, as we bake knowledge of the super class into
  // this class.
  private val superConstructorCalls: mutable.AnyRefMap[Symbol, collection.Map[Symbol, Symbol]] = perRunCaches.newAnyRefMap()

  // allows override of the behavior of the resetTyper method w.r.t comments
  def resetDocComments() = clearDocComments()

  def resetTyper(): Unit = {
    //println("resetTyper called")
    resetContexts()
    resetImplicits()
    resetDocComments()
    rightAssocValDefs.clear()
    inlinedRightAssocValDefs.clear()
    superConstructorCalls.clear()
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
  class SilentTypeError private(val errors: List[AbsTypeError], val warnings: List[ContextWarning]) extends SilentResult[Nothing] {
    override def isEmpty = true
    def err: AbsTypeError = errors.head
    def reportableErrors = errors match {
      case (e1: AmbiguousImplicitTypeError) :: _ =>
        List(e1) // DRYer error reporting for neg/t6436b.scala
      case all =>
        all
    }
  }
  object SilentTypeError {
    def apply(errors: AbsTypeError*): SilentTypeError = apply(errors.toList, Nil)
    def apply(errors: List[AbsTypeError], warnings: List[ContextWarning]): SilentTypeError = new SilentTypeError(errors, warnings)
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

  private final val InterpolatorCodeRegex  = """\$\{\s*(.*?)\s*\}""".r
  private final val InterpolatorIdentRegex = """\$[\w]+""".r // note that \w doesn't include $

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
    def privates[T <: Tree](typer: Typer, owner: Symbol, tree: T): T =
      if (owner.isJavaDefined) tree else check(typer, owner, EmptyScope, WildcardType, tree)

    @tailrec
    private def check[T <: Tree](typer: Typer, owner: Symbol, scope: Scope, pt: Type, tree: T): T = {
      this.owner = owner
      this.scope = scope
      hiddenSymbols = Nil
      import typer.TyperErrorGen._
      val tp1 = apply(tree.tpe)
      if (hiddenSymbols.isEmpty) tree setType tp1
      else if (hiddenSymbols exists (_.isErroneous)) HiddenSymbolWithError(tree)
      else if (isFullyDefined(pt)) tree setType pt
      else if (tp1.typeSymbol.isAnonymousClass)
        check(typer, owner, scope, pt, tree setType tp1.typeSymbol.classBound)
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
      def checkNoEscape(sym: Symbol): Unit = {
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

  private final val typerFreshNameCreators = perRunCaches.newAnyRefMap[Symbol, FreshNameCreator]()
  def freshNameCreatorFor(context: Context) = typerFreshNameCreators.getOrElseUpdate(context.outermostContextAtCurrentPos.enclClassOrMethod.owner, new FreshNameCreator)

  abstract class Typer(context0: Context) extends TyperDiagnostics with Adaptation with Tag with PatternTyper with TyperContextErrors {
    private def unit = context.unit
    import typeDebug.ptTree
    import TyperErrorGen._
    implicit def fresh: FreshNameCreator = freshNameCreatorFor(context)

    private def transformed: mutable.Map[Tree, Tree] = unit.transformed

    val infer = new Inferencer {
      def context = Typer.this.context
      // See scala/bug#3281 re undoLog
      override def isCoercible(tp: Type, pt: Type) = undoLog undo viewExists(tp, pt)
    }

    /** Overridden to false in scaladoc and/or interactive. */
    def canAdaptConstantTypeToLiteral = true
    def canTranslateEmptyListToNil    = true
    def missingSelectErrorTree(tree: Tree, @unused qual: Tree, @unused name: Name): Tree = tree

    // used to exempt synthetic accessors (i.e. those that are synthesized by the compiler to access a field)
    // from skolemization because there's a weird bug that causes spurious type mismatches
    // (it seems to have something to do with existential abstraction over values
    // https://github.com/scala/scala-dev/issues/165
    // when we're past typer, lazy accessors are synthetic, but before they are user-defined
    // to make this hack less hacky, we could rework our flag assignment to allow for
    // requiring both the ACCESSOR and the SYNTHETIC bits to trigger the exemption
    private def isSyntheticAccessor(sym: Symbol) = sym.isAccessor && (!sym.isLazy || isPastTyper)

    private val fixableFunctionMembers = List(nme.tupled, TermName("curried"))

    // when type checking during erasure, generate erased types in spots that aren't transformed by erasure
    // (it erases in TypeTrees, but not in, e.g., the type a Function node)
    def phasedAppliedType(sym: Symbol, args: List[Type]) = {
      val tp = appliedType(sym, args)
      if (phase.erasedTypes) erasure.specialScalaErasureFor(sym)(tp) else tp
    }

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

          val res =
            if (paramFailed || (paramTp.isErroneous && {paramFailed = true; true})) SearchFailure
            else inferImplicitFor(paramTp, fun, context, reportAmbiguous = context.reportErrors)
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
      case x => throw new MatchError(x)
    }

    def viewExists(from: Type, to: Type): Boolean = (
         !from.isError
      && !to.isError
      && context.implicitsEnabled
      && (inferView(context.tree, from, to, reportAmbiguous = false) != EmptyTree)
      // scala/bug#8230 / scala/bug#8463 We'd like to change this to `saveErrors = false`, but can't.
      // For now, we can at least pass in `context.tree` rather then `EmptyTree` so as
      // to avoid unpositioned type errors.
    )

    // Get rid of any special ProtoTypes, so that implicit search won't have to deal with them
    private def normalizeProtoForView(proto: Type): Type = proto match {
      case proto: OverloadedArgProto => proto.underlying
      case pt                        => pt
    }

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
    def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean = true, saveErrors: Boolean = true): Tree =
      if (isPastTyper || from.isInstanceOf[MethodType] || from.isInstanceOf[OverloadedType] || from.isInstanceOf[PolyType]) EmptyTree
      else {
        debuglog(s"Inferring view from $from to $to for $tree (reportAmbiguous= $reportAmbiguous, saveErrors=$saveErrors)")

        val fromNoAnnot = from.withoutAnnotations
        val toNorm = normalizeProtoForView(to)
        val result = inferImplicitView(fromNoAnnot, toNorm, tree, context, reportAmbiguous, saveErrors) match {
          case fail if fail.isFailure => inferImplicitView(byNameType(fromNoAnnot), toNorm, tree, context, reportAmbiguous, saveErrors)
          case ok => ok
        }

        if (result.subst != EmptyTreeTypeSubstituter) {
          result.subst traverse tree
          notifyUndetparamsInferred(result.subst.from, result.subst.to)
        }
        result.tree
      }

    import infer._

    private var namerCache: Namer = null
    def namer: Namer = {
      if ((namerCache eq null) || namerCache.context != context)
        namerCache = newNamer(context)
      namerCache
    }

    var context = context0
    def context1 = context

    def dropExistential(tp: Type): Type = tp match {
      case ExistentialType(tparams, _) =>
        new SubstWildcardMap(tparams).apply(tp)
      case TypeRef(_, sym, _) if sym.isAliasType =>
        val tp0 = tp.dealias
        if (tp eq tp0) {
          devWarning(s"dropExistential did not progress dealiasing $tp, see scala/bug#7126")
          tp
        } else {
          val tp1 = dropExistential(tp0)
          if (tp1 eq tp0) tp else tp1
        }
      case _ => tp
    }

    private def errorNotClass(tpt: Tree, found: Type)  = { ClassTypeRequiredError(tpt, found); false }
    private def errorNotStable(tpt: Tree, found: Type) = { TypeNotAStablePrefixError(tpt, found); false }

    /** Check that `tpt` refers to a non-refinement class or module type */
    def checkClassOrModuleType(tpt: Tree): Boolean = {
      val tpe = unwrapToClass(tpt.tpe)
      def isModule = tpe match {
        case SingleType(_, sym) => sym.isModule
        case _ => false
      }
      isNonRefinementClassType(tpe) || isModule || errorNotClass(tpt, tpe)
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

    class NonCyclicStack {
      // for diverging types, neg/t510.scala
      private val maxRecursion = 42

      // For each abstract type symbol (type member, type parameter), keep track of seen types represented by that symbol
      private lazy val map = collection.mutable.HashMap[Symbol, mutable.ListBuffer[Type]]()

      def lockSymbol[T](sym: Symbol, tp: Type)(body: => T): T = {
        val stk = map.getOrElseUpdate(sym, ListBuffer.empty)
        stk.prepend(tp)
        try body
        finally stk.remove(0)
      }

      def isUnlocked(sym: Symbol, tp: Type): Boolean =
        !sym.isNonClassType || !map.get(sym).exists(tps => tps.length > maxRecursion || tps.contains(tp))
    }

    /** Check that type `tp` is not a subtype of itself
     */
    def checkNonCyclic(pos: Position, tp: Type, stack: NonCyclicStack = new NonCyclicStack): Boolean = {
      def checkNotLocked(sym: Symbol) =
        stack.isUnlocked(sym, tp) || { CyclicAliasingOrSubtypingError(pos, sym); false }

      tp match {
        case TypeRef(pre, sym, args) =>
          checkNotLocked(sym) && {
            !sym.isNonClassType ||
              stack.lockSymbol(sym, tp) {
                checkNonCyclic(pos, appliedType(pre.memberInfo(sym), args), stack)
              }
          }

        case SingleType(_, sym) =>
          checkNotLocked(sym)
        case st: SubType =>
          checkNonCyclic(pos, st.supertype, stack)
        case ct: CompoundType =>
          ct.parents forall (x => checkNonCyclic(pos, x, stack))
        case _ =>
          true
      }
    }

    def checkNonCyclic(sym: Symbol): Unit = {
      if (!checkNonCyclic(sym.pos, sym.tpe_*)) sym.setInfo(ErrorType)
    }

    def checkNonCyclic(defn: ValOrDefDef, tpt: Tree): Unit = {
      if (!checkNonCyclic(defn.pos, tpt.tpe)) {
        tpt setType ErrorType
        defn.symbol.setInfo(ErrorType)
      }
    }

    def reenterValueParams(vparamss: List[List[ValDef]]): Unit = {
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
    def qualifyingClass(tree: Tree, qual: Name, packageOK: Boolean, immediate: Boolean) =
      context.enclClass.owner.ownersIterator.find(o => qual.isEmpty || o.isClass && o.name == qual) match {
        case Some(c) if packageOK || !c.isPackageClass => c
        case _ =>
          QualifyingClassError(tree, qual)
          // Delay `setError` in namer, scala/bug#10748
          if (immediate) setError(tree) else unit.addPostUnitCheck(() => setError(tree))
          NoSymbol
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
    final def typerWithLocalContext[T](c: Context)(f: Typer => T): T = {
      try f(newTyper(c))
      finally c.reporter.propagateErrorsTo(context.reporter)
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
        || !(tree.tpe <:< pt) && (ptSym.isAbstractType && pt.lowerBound.isStable || ptSym.isRefinementClass)
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
    private def makeAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Any /*Type | (Tree, Type)*/ =
      if (!unit.isJava && context.isInPackageObject(sym, pre.typeSymbol)) {
        val qual = typedQualifier { atPos(tree.pos.makeTransparent) {
          tree match {
            case Ident(_) =>
              val packageObject =
                if (!sym.isOverloaded && sym.owner.isModuleClass) sym.owner.sourceModule // historical optimization, perhaps no longer needed
                else pre.typeSymbol.packageObject
              Ident(packageObject)
            case Select(qual, _)             => Select(qual, nme.PACKAGEkw)
            case SelectFromTypeTree(qual, _) => Select(qual, nme.PACKAGEkw)
            case x                           => throw new MatchError(x)
          }
        }}
        val tree1 = atPos(tree.pos) {
          tree match {
            case Ident(name)                 => Select(qual, name)
            case Select(_, name)             => Select(qual, name)
            case SelectFromTypeTree(_, name) => SelectFromTypeTree(qual, name)
            case x                           => throw new MatchError(x)
          }
        }
        (checkAccessible(tree1, sym, qual.tpe, qual, unit.isJava), qual.tpe)
      } else {
        checkAccessible(tree, sym, pre, site, unit.isJava)
      }

    /** Post-process an identifier or selection node, performing the following:
     *  1. Check that non-function pattern expressions are stable (ignoring volatility concerns -- scala/bug#6815)
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
        narrowIf(tree, condition = true)
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

    @deprecated("Use the overload accepting a Type.", "2.12.9")
    def member(qual: Tree, name: Name): Symbol = member(qual.tpe, name)
    /** The member with given name of given qualifier type */
    def member(qual: Type, name: Name): Symbol = {
      def callSiteWithinClass(clazz: Symbol) = context.enclClass.owner hasTransOwner clazz
      val includeLocals = qual match {
        case ThisType(clazz) if callSiteWithinClass(clazz)                => true
        case SuperType(clazz, _) if callSiteWithinClass(clazz.typeSymbol) => true
        case _                                                            => phase.next.erasedTypes
      }
      if (includeLocals) qual member name
      else qual nonLocalMember name
    }

    def silent[T](op: Typer => T,
                  reportAmbiguousErrors: Boolean = context.ambiguousErrors,
                  newtree: Tree = context.tree): SilentResult[T] = {
      val findMemberStart = if (settings.areStatisticsEnabled) statistics.startCounter(findMemberFailed) else null
      val subtypeStart = if (settings.areStatisticsEnabled) statistics.startCounter(subtypeFailed) else null
      val failedSilentStart = if (settings.areStatisticsEnabled) statistics.startTimer(failedSilentNanos) else null
      def stopStats() = {
        if (settings.areStatisticsEnabled) statistics.stopCounter(findMemberFailed, findMemberStart)
        if (settings.areStatisticsEnabled) statistics.stopCounter(subtypeFailed, subtypeStart)
        if (settings.areStatisticsEnabled) statistics.stopTimer(failedSilentNanos, failedSilentStart)
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
          context1.pendingStabilizers = context.pendingStabilizers
          val typer1 = newTyper(context1)
          val result = op(typer1)
          context.undetparams = context1.undetparams
          context.savedTypeBounds = context1.savedTypeBounds
          context.pendingStabilizers = context1.pendingStabilizers

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
      isPastTyper || {
        val featureName = {
          val nestedOwners = featureTrait.owner.ownerChain.takeWhile(_ != languageFeatureModule.moduleClass).reverse
          nestedOwners.map(s => s"${s.name}.").mkString + featureTrait.name
        }
        settings.language.contains(featureName) || {
          def action(): Boolean = {
            if (!immediate)
            debuglog(s"deferred check of feature $featureTrait")
          def hasImport = inferImplicitByType(featureTrait.tpe, context).isSuccess
           hasImport || {
              val Some(AnnotationInfo(_, List(Literal(Constant(featureDesc: String)), Literal(Constant(required: Boolean))), _)) =
                featureTrait.getAnnotation(LanguageFeatureAnnot): @unchecked
              context.featureWarning(pos, featureName, featureDesc, featureTrait, construct, required)
              false
            }
          }
          if (immediate) action()
          else { unit.addPostUnitCheck(() => action()); true }
        }
      }

    def checkExistentialsFeature(pos: Position, tpe: Type, prefix: String) = tpe match {
      case extp: ExistentialType if !extp.isRepresentableWithWildcards && !tpe.isError =>
        checkFeature(pos, currentRun.runDefinitions.ExistentialsFeature, prefix+" "+tpe)
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
     *  (4.1) If the method has only implicit parameters, pass implicit arguments (see adaptToImplicitMethod)
     *  (4.2) otherwise, if the method is 0-ary and it can be auto-applied (see checkCanAutoApply), apply it to ()
     *  (4.3) otherwise, if the method is not a constructor, and can be eta-expanded (see checkCanEtaExpand), eta-expand
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
     *  (14) When in mode EXPRmode, do SAM conversion
     *  (15) When in mode EXPRmode, apply a view
     *  If all this fails, error
     *
     *  Note: the `original` tree parameter is for re-typing implicit method invocations (see below)
     *  and should not be used otherwise. TODO: can it be replaced with a tree attachment?
     */
    protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
      def hasUndets           = !context.undetparams.isEmpty
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
            if (original != EmptyTree && !pt.isWildcard) {
              typer1 silent { tpr =>
                val withImplicitArgs = tpr.applyImplicitArgs(tree)
                if (tpr.context.reporter.hasErrors) tree // silent will wrap it in SilentTypeError anyway
                else tpr.typed(withImplicitArgs, mode, pt)
              } orElse { originalErrors =>
                // Re-try typing (applying to implicit args) without expected type. Add in 53d98e7d42 to
                // for better error message (scala/bug#2180, https://www.scala-lang.org/old/node/3453.html)
                val resetTree = resetAttrs(original)
                resetTree match {
                  case treeInfo.Applied(fun, _, _) =>
                    if (fun.symbol != null && fun.symbol.isError)
                      // scala/bug#9041 Without this, we leak error symbols past the typer!
                      // because the fallback typechecking notices the error-symbol,
                      // refuses to re-attempt typechecking, and presumes that someone
                      // else was responsible for issuing the related type error!
                      fun.setSymbol(NoSymbol)
                }
                debuglog(s"fallback on implicits: $tree/$resetTree")
                // scala/bug#10066 Need to patch the enclosing tree in the context to make translation of Dynamic
                // work during fallback typechecking below.
                val resetContext: Context = {
                  object substResetForOriginal extends AstTransformer {
                    override def transform(tree: Tree): Tree = {
                      if (tree eq original) resetTree
                      else super.transform(tree)
                    }
                  }
                  context.make(substResetForOriginal.transform(context.tree))
                }
                typerWithLocalContext(resetContext) { typer1 =>
                  typer1.silent { typer1 =>
                    val tree1 = typer1.typed(resetTree, mode)
                    // Q: `typed` already calls `pluginsTyped` and `adapt`. the only difference here is that
                    // we pass `EmptyTree` as the `original`. intended? added in 2009 (53d98e7d42) by martin.
                    tree1 setType pluginsTyped(tree1.tpe, typer1, tree1, mode, pt)
                    if (tree1.isEmpty) tree1 else typer1.adapt(tree1, mode, pt, original = EmptyTree)
                  } orElse { _ =>
                    originalErrors.foreach(context.issue)
                    setError(tree)
                  }
                }
              }
            }
            else
              typer1.typed(typer1.applyImplicitArgs(tree), mode, pt)
          )
      }

      def adaptMethodTypeToExpr(mt: MethodType): Tree = {
        val meth =
          tree match {
            // a partial named application is a block (see comment in EtaExpansion)
            // How about user-written blocks? Can they ever have a MethodType?
            case Block(_, tree1) => tree1.symbol
            case _               => tree.symbol
          }


        val arity = mt.params.length

        def warnTree = original orElse tree

        def warnEtaZero(): true = {
          if (settings.warnEtaZero) {
            context.warning(tree.pos,
              s"""An unapplied 0-arity method was eta-expanded (due to the expected type $pt), rather than applied to `()`.
                 |Write ${Apply(warnTree, Nil)} to invoke method ${meth.decodedName}, or change the expected type.""".stripMargin,
              WarningCategory.LintEtaZero)
          }
          true
        }

        def warnEtaSam(): true = {
          if (settings.warnEtaSam || currentRun.isScala3) {
            val sam = samOf(pt)
            if (sam.exists) {
              val samClazz = sam.owner
              val isJavaClass = samClazz.isJava && !samClazz.isInterface
              if (!samClazz.hasAnnotation(definitions.FunctionalInterfaceClass)) {
                val ft = samToFunctionType(pt)
                val sample = Function(meth.paramss.head.map(ValDef(_)), Apply(meth, meth.paramss.head.map(p => Ident(p.name)): _*))
                val places = Apply(meth, meth.paramss.head.map(_ => Ident(nme.USCOREkw)): _*)
                val advice = if (isJavaClass) "" else s"\n$samClazz should be annotated with `@FunctionalInterface` if eta-expansion is desired."
                context.warning(tree.pos,
                  sm"""Eta-expansion to expected type $pt, which is not a function type but is SAM-convertible to $ft.$advice
                      |Avoid eta-expansion by writing the function literal `$sample` or `$places`.
                      |This warning can be filtered with `-Wconf:cat=lint-eta-sam`.""",
                  WarningCategory.LintEtaSam)
              }
            }
          }
          true
        }

        // note that isFunctionProto(pt) does not work properly for Function0
        lazy val ptUnderlying =
          (pt match {
            case oapt: OverloadedArgProto => oapt.underlying
            case pt                       => pt
          }).dealiasWiden

        // (4.3) condition for eta-expansion by arity & -Xsource level
        //
        // for arity == 0:
        //   - if Function0 is expected -- SAM types do not eta-expand because it could be an accidental SAM scala/bug#9489
        // for arity > 0:
        //   - 2.13: if function or sam type is expected
        //   - 3.0: unconditionally
        //
        // warnings:
        //   - for arity == 0: eta-expansion of zero-arg methods was deprecated (scala/bug#7187)
        //   - for arity > 0: expected type is a SAM that is not annotated with `@FunctionalInterface`
        def checkCanEtaExpand(): Boolean = {
          def expectingSamOfArity = {
            val sam = samOf(ptUnderlying)
            sam.exists && sam.info.params.lengthIs == arity
          }

          val expectingFunctionOfArity = {
            val ptSym = ptUnderlying.typeSymbolDirect
            (ptSym eq FunctionClass(arity)) || (arity > 0 && (ptSym eq FunctionClass(1))) // allowing for tupling conversion
          }

          if (arity == 0)
            expectingFunctionOfArity && warnEtaZero()
          else
            expectingFunctionOfArity || expectingSamOfArity && warnEtaSam() || currentRun.isScala3
        }

        def matchNullaryLoosely: Boolean = {
          def test(sym: Symbol) = sym.isJavaDefined || sym.owner == AnyClass
          test(meth) || meth.overrides.exists(test)
        }

        // (4.2) condition for auto-application
        //
        // Currently the condition is more involved to give slack to Scala methods overriding Java-defined ones;
        // I (moors) think we should resolve that by introducing slack in overriding e.g. a Java-defined `def toString()` by a Scala-defined `def toString`.
        // This also works better for dealing with accessors overriding Java-defined methods. The current strategy in methodSig is problematic:
        // > // Add a () parameter section if this overrides some method with () parameters
        // > val vparamSymssOrEmptyParamsFromOverride =
        // This means an accessor that overrides a Java-defined method gets a MethodType instead of a NullaryMethodType, which breaks lots of assumptions about accessors)
        def checkCanAutoApply(): Boolean = {
          if (!isPastTyper && !matchNullaryLoosely) {
            val msg =
              s"""Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method ${meth.decodedName},
                 |or remove the empty argument list from its definition (Java-defined methods are exempt).
                 |In Scala 3, an unapplied method like this will be eta-expanded into a function.""".stripMargin
            val action = runReporting.codeAction("add `()`", tree.pos.focusEnd, "()", msg)
            context.deprecationWarning(tree.pos, NoSymbol, msg, "2.13.3", action)
          }
          true
        }

        if (!meth.isConstructor && checkCanEtaExpand()) typedEtaExpansion(tree, mode, pt)
        else if (arity == 0 && checkCanAutoApply()) {
          val apply = Apply(tree, Nil).setPos(tree.pos).updateAttachment(AutoApplicationAttachment)
          if (tree.hasAttachment[PostfixAttachment.type]) apply.updateAttachment(InfixAttachment)
          adapt(typed(apply), mode, pt, original)
        } else
          if (context.implicitsEnabled) MissingArgsForMethodTpeError(tree, meth) // `context.implicitsEnabled` implies we are not in a pattern
          else UnstableTreeError(tree)
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
        val adapted = adaptToName(unmarkDynamicRewrite(tree), nme.apply)
        val qual = gen.stabilize(adapted)
        val t = atPos(tree.pos)(Select(qual setPos tree.pos.makeTransparent, nme.apply))
        wrapErrors(t, _.typed(t, mode, pt))
      }
      def adaptConstant(value: Constant): Tree = {
        val sym = tree.symbol
        if (sym != null && !context.unit.isJava && sym.isDeprecated)
          context.deprecationWarning(tree.pos, sym)
        tree match {
          case Literal(`value`) => tree
          case _ =>
            // If the original tree is not a literal, make it available to plugins in an attachment
            treeCopy.Literal(tree, value).updateAttachment(OriginalTreeAttachment(tree))
        }
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
      // scala/bug#6029 shows another case where we also fail (in uncurry), but this time the expected
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
          case Nil => AdaptTypeError(tree, tree.tpe, pt)
          case _   => logResult(msg)(adapt(tree, mode, deriveTypeWithWildcards(boundOrSkolems)(pt), original = EmptyTree))
        }
      }

      // if user wrote case companion C for expected function type, use C.apply or (C.apply _).tupled
      def adaptApplyInsertion(): Tree = doAdaptApplyInsertion(retry = false)

      def doAdaptApplyInsertion(retry: Boolean): Tree =
        if (!isPastTyper && tree.symbol != null && tree.symbol.isModule && tree.symbol.companion.isCase && isFunctionType(pt))
          silent(_.typed(atPos(tree.pos)(Select(tree, nme.apply)), mode, if (retry) WildcardType else pt)) match {
            case SilentResultValue(applicator) =>
              val arity = definitions.functionArityFromType(applicator.tpe)
              if (arity < 0) EmptyTree
              else functionOrPfOrSamArgTypes(pt) match {
                case arg :: Nil if definitions.isTupleType(arg) && arg.typeArgs.lengthCompare(arity) == 0 =>
                  val tupled = typed(atPos(tree.pos)(Select(applicator, nme.tupled)), mode, pt)
                  if (!tupled.isErroneous) {
                    val msg = s"The method `apply` is inserted. The auto insertion will be deprecated, please write `(${tree.symbol.name}.apply _).tupled` explicitly."
                    context.deprecationWarning(tree.pos, tree.symbol, msg, "2.13.13")
                    tupled
                  }
                  else EmptyTree
                case args if args.lengthCompare(arity) == 0 =>
                  val msg = s"The method `apply` is inserted. The auto insertion will be deprecated, please write `${tree.symbol.name}.apply` explicitly."
                  context.deprecationWarning(tree.pos, tree.symbol, msg, "2.13.13")
                  applicator
                case _ => EmptyTree
              }
            case _ if !retry => doAdaptApplyInsertion(retry = true)
            case _ => EmptyTree
          }
        else EmptyTree

      def adaptExprNotFunMode(): Tree = {
        def lastTry(err: AbsTypeError = null): Tree = {
          debuglog("error tree = " + tree)
          if (settings.isDebug && settings.explaintypes.value) explainTypes(tree.tpe, pt)
          if (err ne null) context.issue(err)
          if (tree.tpe.isErroneous || pt.isErroneous) setError(tree)
          else
            adaptApplyInsertion() orElse adaptMismatchedSkolems()
        }

        // TODO: should we even get to fallbackAfterVanillaAdapt for an ill-typed tree?
        if (mode.typingExprNotFun && !tree.tpe.isErroneous) {
          @inline def tpdPos(transformed: Tree) = typedPos(tree.pos, mode, pt)(transformed)
          @inline def tpd(transformed: Tree)    = typed(transformed, mode, pt)

          @inline def warnValueDiscard(): Unit =
            if (!isPastTyper && settings.warnValueDiscard.value && !treeInfo.isThisTypeResult(tree) && !treeInfo.hasExplicitUnit(tree))
              tree.updateAttachment(DiscardedValue)
          @inline def warnNumericWiden(tpSym: Symbol, ptSym: Symbol): Unit = if (!isPastTyper) {
            val targetIsWide = ptSym == FloatClass || ptSym == DoubleClass
            val isInharmonic = {
              def intWidened = tpSym == IntClass && ptSym == FloatClass
              def longWidened = tpSym == LongClass && targetIsWide
              intWidened || longWidened
            }
            if (isInharmonic) {
              // not `context.deprecationWarning` because they are not buffered in silent mode
              val msg = s"Widening conversion from ${tpSym.name} to ${ptSym.name} is deprecated because it loses precision. Write `.to${ptSym.name}` instead."
              val orig = tree.pos.source.sourceAt(tree.pos)
              context.warning(tree.pos, msg, WarningCategory.Deprecation,
                runReporting.codeAction("add conversion", tree.pos, s"${CodeAction.maybeWrapInParens(orig)}.to${ptSym.name}", msg))
            } else {
              object warnIntDiv extends Traverser {
                def isInt(t: Tree) = ScalaIntegralValueClasses(t.tpe.typeSymbol)
                override def traverse(tree: Tree): Unit = tree match {
                  case Apply(Select(q, nme.DIV), _) if isInt(q) =>
                    val msg = s"integral division is implicitly converted (widened) to floating point. Add an explicit `.to${ptSym.name}`."
                    context.warning(tree.pos, msg, WarningCategory.LintIntDivToFloat,
                      runReporting.codeAction("add conversion", tree.pos, s"(${tree.pos.source.sourceAt(tree.pos)}).to${ptSym.name}", msg))
                  case Apply(Select(a1, _), List(a2)) if isInt(tree) && isInt(a1) && isInt(a2) => traverse(a1); traverse(a2)
                  case Select(q, _) if isInt(tree) && isInt(q) => traverse(q)
                  case _ =>
                }
              }
              if (targetIsWide && settings.lintIntDivToFloat) warnIntDiv(tree)
              if (settings.warnNumericWiden.value) context.warning(tree.pos, "implicit numeric widening", WarningCategory.WFlagNumericWiden)
            }
          }

          // The <: Any requirement inhibits attempts to adapt continuation types to non-continuation types.
          val anyTyped = tree.tpe <:< AnyTpe

          pt.dealias match {
            case TypeRef(_, UnitClass, _) if anyTyped => // (12)
              warnValueDiscard() ; tpdPos(gen.mkUnitBlock(tree))
            case TypeRef(_, numValueCls, _) if anyTyped && isNumericValueClass(numValueCls) && isNumericSubType(tree.tpe, pt) => // (10) (11)
              warnNumericWiden(tree.tpe.widen.typeSymbol, numValueCls) ; tpdPos(Select(tree, s"to${numValueCls.name}"))
            case dealiased if dealiased.annotations.nonEmpty && canAdaptAnnotations(tree, this, mode, pt) => // (13)
              tpd(adaptAnnotations(tree, this, mode, pt))
            case _ =>
              if (hasUndets) instantiate(tree, mode, pt)
              else {
                // (14) sam conversion
                // TODO: figure out how to avoid partially duplicating typedFunction (samMatchingFunction)
                // Could we infer the SAM type, assign it to the tree and add the attachment,
                // all in one fell swoop at the end of typedFunction?
                val didInferSamType = inferSamType(tree, pt, mode)

                if (didInferSamType) tree
                else {  // (15) implicit view application
                  val coercion =
                    if (context.implicitsEnabled) inferView(tree, tree.tpe, pt)
                    else EmptyTree
                  if (coercion ne EmptyTree) {
                    def msg = s"inferred view from ${tree.tpe} to $pt via $coercion: ${coercion.tpe}"
                    if (settings.logImplicitConv.value) context.echo(tree.pos, msg)
                    else debuglog(msg)

                    val viewApplied = new ApplyImplicitView(coercion, List(tree)) setPos tree.pos
                    val silentContext = context.makeImplicit(context.ambiguousErrors)
                    val typedView = newTyper(silentContext).typed(viewApplied, mode, pt)

                    silentContext.reporter.firstError match {
                      case None => typedView
                      case Some(err) => lastTry(err)
                    }
                  } else lastTry()
                }
              }
          }
        } else lastTry()
      }


      def vanillaAdapt(tree: Tree) = {
        def applyPossible = {
          def applyMeth = member(adaptToName(tree, nme.apply).tpe, nme.apply)
          def hasPolymorphicApply = applyMeth.alternatives exists (_.tpe.typeParams.nonEmpty)
          def hasMonomorphicApply = applyMeth.alternatives exists (_.tpe.paramSectionCount > 0)

          def badDynamicApply() = {
            tree match {
              case Apply(fun, _) => DynamicRewriteError(tree, ApplyWithoutArgsError(tree, fun))
              case _             => ()
            }
            false
          }
          if (acceptsApplyDynamic(tree.tpe))
            !isDynamicRewrite(tree) || badDynamicApply()
          else if (mode.inTappMode)
            tree.tpe.typeParams.isEmpty && hasPolymorphicApply
          else
            hasMonomorphicApply
        }
        def shouldInsertApply(tree: Tree) = mode.typingExprFun && {
          tree.tpe match {
            case _: MethodType | _: OverloadedType | _: PolyType => false
            case _                                               => applyPossible
          }
        }
        if (tree.isType) // (6)
          adaptType()
        else if (mode.typingExprNotFun &&
                 treeInfo.isMacroApplication(tree) &&
                 !isMacroExpansionSuppressed(tree))
          macroExpand(this, tree, mode, pt)
        else if (mode.typingConstructorPattern)
          typedConstructorPattern(tree, pt)
        else if (shouldInsertApply(tree)) // (8)
          insertApply()
        else if (hasUndetsInMonoMode) // (9)
          // This used to have
          //     assert(!context.inTypeConstructorAllowed, context)
          // but that's not guaranteed to be true in the face of erroneous code; errors in typedApply might mean we
          // never get around to inferring them, and they leak out and wind up here.
          instantiatePossiblyExpectingUnit(tree, mode, pt)
        else if (isScalaRepeatedParamType(tree.tpe) && !isScalaRepeatedParamType(pt))
          // TODO: we really shouldn't use T* as a first class types (e.g. for repeated case fields),
          //  but we can't allow T* to conform to other types (see isCompatible) because that breaks overload resolution
          adapt(tree.setType(repeatedToSeq(tree.tpe)), mode, pt, original = EmptyTree)
        else if (tree.tpe <:< pt) {
          val sym = tree.symbol
          if (sym != null && !isPastTyper && currentRun.isScala3 && isFunctionType(pt) && sym.isModule && sym.isSynthetic && sym.companion.isCase)
            context.warning(tree.pos, s"Synthetic case companion used as a function. In Scala 3 (or with -Xsource-features:case-companion-function), case companions no longer extend FunctionN. Use ${sym.name}.apply instead.", Scala3Migration)
          tree
        } else if (mode.inPatternMode && { inferModulePattern(tree, pt); isPopulated(tree.tpe, approximateAbstracts(pt)) })
          tree
        else {
          val constFolded = constfold(tree, pt, context.owner)
          if (constFolded.tpe <:< pt) adapt(constFolded, mode, pt, original) // set stage for (0)
          else adaptExprNotFunMode() // (10) -- (15)
        }
      }

      // begin adapt
      if (isMacroImplRef(tree)) {
        if (treeInfo.isMacroApplication(tree)) adapt(unmarkMacroImplRef(tree), mode, pt, original)
        else tree
      } else tree.tpe match {
        case AnnotatedType(_, _) if canAdaptAnnotations(tree, this, mode, pt) => // (-1)
          adaptAnnotations(tree, this, mode, pt)
        case ct @ FoldableConstantType(value) if mode.inNone(TYPEmode | FUNmode) && (ct <:< pt) && canAdaptConstantTypeToLiteral => // (0)
          adaptConstant(value)
        case OverloadedType(_, _) if !mode.inFunMode => // (1)
          inferExprAlternative(tree, pt): Unit
          adaptAfterOverloadResolution(tree, mode, pt, original)
        case NullaryMethodType(restpe) => // (2)
          if (hasUndets && settings.lintUniversalMethods && (isCastSymbol(tree.symbol) || isTypeTestSymbol(tree.symbol)) && context.undetparams.exists(_.owner == tree.symbol))
            context.warning(tree.pos, s"missing type argument to ${tree.symbol}", WarningCategory.LintUniversalMethods)
          val resTpDeconst = // keep constant types when they are safe to fold. erasure eliminates constant types modulo some exceptions, so keep those.
            if (isBeforeErasure && tree.symbol.isAccessor && tree.symbol.hasFlag(STABLE) && treeInfo.isExprSafeToInline(tree)) restpe
            else restpe.deconst
          adapt(tree setType resTpDeconst, mode, pt, original)
        case TypeRef(_, ByNameParamClass, arg :: Nil) if mode.inExprMode => // (2)
          adapt(tree setType arg, mode, pt, original)
        case tp if mode.typingExprNotLhs && isExistentialType(tp) && !isSyntheticAccessor(context.owner) =>
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
          val tree1 =
            if (tree.isType) tree
            else TypeApply(tree, tparams1.map(tparam => TypeTree(tparam.tpeHK).setPos(tree.pos.focus))).setPos(tree.pos)

          context.undetparams ++= tparams1
          notifyUndetparamsAdded(tparams1)
          adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt, original)

        case mt: MethodType if mode.typingExprNotFunNotLhs && mt.isImplicit => // (4.1)
          adaptToImplicitMethod(mt)
        case mt: MethodType if mode.typingExprNotFunNotLhs && !hasUndetsInMonoMode && !treeInfo.isMacroApplicationOrBlock(tree) => // (4.2) - (4.3)
          adaptMethodTypeToExpr(mt)
        case _ =>
          vanillaAdapt(tree)
      }
    }

    // This just exists to help keep track of the spots where we have to adapt a tree after
    // overload resolution. These proved hard to find during the fix for scala/bug#8267.
    def adaptAfterOverloadResolution(tree: Tree, mode: Mode, pt: Type = WildcardType, original: Tree = EmptyTree): Tree = {
      adapt(tree, mode, pt, original)
    }

    def instantiate(tree: Tree, mode: Mode, pt: Type): Tree = {
      inferExprInstance(tree, context.extractUndetparams(), pt, useWeaklyCompatible = true)
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
        val valueDiscard = atPos(tree.pos)(gen.mkUnitBlock(instantiate(tree, mode, WildcardType)))
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
        && !qtpe.isWildcard
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
          case EmptyTree => qual
          case coercion  =>
            if (settings.logImplicitConv.value)
              context.echo(qual.pos, s"applied implicit conversion from ${qual.tpe} to ${searchTemplate} = ${coercion.symbol.defString}")

            if (currentRun.isScala3 && coercion.symbol == currentRun.runDefinitions.Predef_any2stringaddMethod)
              if (!currentRun.sourceFeatures.any2StringAdd)
                runReporting.warning(qual.pos, s"Converting to String for concatenation is not supported in Scala 3 (or with -Xsource-features:any2stringadd).", Scala3Migration, coercion.symbol)
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
    def adaptToArguments(qual: Tree, name: Name, args: List[Tree], pt: Type, reportAmbiguous: Boolean = true, saveErrors: Boolean = true): Tree = {
      def doAdapt(restpe: Type) =
        //util.trace("adaptToArgs "+qual+", name = "+name+", argtpes = "+(args map (_.tpe))+", pt = "+pt+" = ")
        adaptToMember(qual, HasMethodMatching(name, args map (_.tpe), normalizeProtoForView(restpe)), reportAmbiguous, saveErrors)

      if (pt.isWildcard)
        doAdapt(pt)
      else silent(_ => doAdapt(pt)) filter (_ != qual) orElse (_ =>
        logResult(s"fallback on implicits in adaptToArguments: $qual.$name")(doAdapt(WildcardType))
      )
    }

    /** Try to apply an implicit conversion to `qual` so that it contains
     *  a method `name`. If that's ambiguous try taking arguments into
     *  account using `adaptToArguments`.
     */
    def adaptToMemberWithArgs(tree: Tree, qual: Tree, name: Name, mode: Mode, reportAmbiguous: Boolean = true, saveErrors: Boolean = true): Tree = {
      def onError(reportError: => Tree): Tree = context.tree match {
        case Apply(tree1, args) if (tree1 eq tree) && args.nonEmpty =>
          silent(_.typedArgs(args.map(_.duplicate), mode))
            .filter(!_.exists(_.isErrorTyped))
            .fold(reportError)(adaptToArguments(qual, name, _, WildcardType, reportAmbiguous, saveErrors))
        case _ =>
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
      if (member(qual.tpe, name) != NoSymbol) qual
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
              //see https://github.com/scala/bug/issues/6359
              implRestriction(tree, "nested object")
            //see https://github.com/scala/bug/issues/6444
            //see https://github.com/scala/bug/issues/6463
            case cd: ClassDef if !cd.symbol.isAnonymousClass => // Don't warn about partial functions, etc. scala/bug#7571
              implRestriction(tree, "nested class") // avoiding Type Tests that might check the $outer pointer.
            case Select(sup @ Super(qual, mix), selector) if selector != nme.CONSTRUCTOR && qual.symbol == clazz && mix != tpnme.EMPTY =>
              //see https://github.com/scala/bug/issues/6483
              implRestriction(sup, "qualified super reference")
            case _ =>
          }
          super.traverse(tree)
        }
      }
      for (stat <- body) {
        def notAllowed(what: String) = context.error(stat.pos, s"$what is not allowed in $where")
        stat match {
          // see https://github.com/scala/bug/issues/6444
          // see https://github.com/scala/bug/issues/6463
          case ClassDef(mods, _, _, _) if isValueClass =>
            implRestriction(stat, s"nested ${ if (mods.isTrait) "trait" else "class" }")
          case _: Import | _: ClassDef | _: TypeDef | EmptyTree => // OK
          case DefDef(_, name, _, _, _, rhs) =>
            if (stat.symbol.isAuxiliaryConstructor)
              notAllowed("secondary constructor")
            else if (isValueClass && (name == nme.equals_ || name == nme.hashCode_) && !stat.symbol.isSynthetic)
              notAllowed(s"redefinition of $name method. See SIP-15, criterion 5.")
            else if (stat.symbol != null && stat.symbol.isParamAccessor)
              notAllowed("additional parameter")
            checkEphemeralDeep.traverse(rhs)
          case _: ValDef =>
            notAllowed("field definition")
          case _: ModuleDef =>
            //see https://github.com/scala/bug/issues/6359
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
          case List(List(_)) =>
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
                /* check all base classes, since derived value classes might lurk in refinement parents */
                if (acc.tpe.typeSymbol.baseClasses exists (_.isDerivedValueClass))
                  context.error(acc.pos, "value class may not wrap another user-defined value class")
                def referencesUnderlying(sym: Symbol) = sym != null && sym.accessedOrSelf == paramAccessor
                checkEphemeral(clazz, body.filterNot(stat => referencesUnderlying(stat.symbol)))
            }
          case _ =>
            context.error(clazz.pos, "value class needs to have exactly one val parameter")
        }
      }

      for (tparam <- clazz.typeParams)
        if (tparam hasAnnotation definitions.SpecializedClass)
          context.error(tparam.pos, "type parameter of value class may not be specialized")
    }

    private def warnMultiargInfix(tree: Tree): Unit =
      context.warning(tree.pos, "multiarg infix syntax looks like a tuple and will be deprecated", WarningCategory.LintMultiargInfix)

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
      val app @ treeInfo.Applied(core, _, argss) = treeInfo.dissectApplied(encodedtpt)
      val decodedtpt = app.callee
      val argssAreTrivial = argss == Nil || argss == ListOfNil

      // we cannot avoid cyclic references with `initialize` here, because when type macros arrive,
      // we'll have to check the probe for isTypeMacro anyways.
      // therefore I think it's reasonable to trade a more specific "inherits itself" error
      // for a generic, yet understandable "cyclic reference" error
      val probe = {
        val p = typedTypeConstructor(core.duplicate).tpe.typeSymbol
        if (p == null) NoSymbol
        else p.initialize
      }

      def cookIfNeeded(tpt: Tree) = if (context.unit.isJava) tpt modifyType rawToExistential else tpt
      cookIfNeeded(if (probe.isTrait || inMixinPosition) {
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
            val treeInfo.Applied(Select(ctor, nme.CONSTRUCTOR), _, _) = supercall: @unchecked
            ctor setType supertpe // this is an essential hack, otherwise it will occasionally fail to typecheck
            atPos(supertpt.pos.focus)(supercall)
          } match {
            case EmptyTree => MissingTypeArgumentsParentTpeError(supertpt); supertpt
            case tpt       => TypeTree(tpt.tpe) setPos supertpt.pos  // scala/bug#7224: don't .focus positions of the TypeTree of a parent that exists in source
          }
        }

        val supertptWithTargs = if (supertparams.isEmpty || context.unit.isJava) supertpt else inferParentTypeArgs

        // this is the place where we tell the typer what argss should be used for the super call
        // if argss are nullary or empty, then (see the docs for `typedPrimaryConstrBody`)
        // the super call dummy is already good enough, so we don't need to do anything
        if (argssAreTrivial) supertptWithTargs else supertptWithTargs updateAttachment SuperArgsAttachment(argss)
      })
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
            case EmptyTree               => EmptyTree
            case x                       => throw new MatchError(x)
          }) orElse cunit
          val cbody1 = treeCopy.Block(cbody, preSuperStats, superCall1)
          val clazz = context.owner
            assert(clazz != NoSymbol, templ)
          // scala/bug#9086 The position of this symbol is material: implicit search will avoid triggering
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
            foreach2(preSuperStats, preSuperVals)((ldef, gdef) => gdef.tpt setType ldef.symbol.tpe)

          if (superCall1 == cunit) EmptyTree
          else cbody2 match { // ???
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
        val supertpt :: rest = parents: @unchecked // parents is always non-empty here - it only grows
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
          if (isPossibleSyntheticParent(sym)) xs.filter(_.symbol != sym)
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

          supertpts mapConserve (tpt => checkNoEscaping.privates(this, context.owner, tpt))
        }
        catch {
          case ex: TypeError if !global.propagateCyclicReferences =>
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
    def validateParentClasses(parents: List[Tree], selfType: Type, clazzIsTrait: Boolean): Unit = {
      val pending = ListBuffer[AbsTypeError]()
      def validateDynamicParent(parent: Symbol, parentPos: Position) =
        if (parent == DynamicClass) checkFeature(parentPos, currentRun.runDefinitions.DynamicsFeature)

      def validateParentClass(parent: Tree, superclazz: Symbol) =
        if (!parent.isErrorTyped) { // redundant
          val psym = parent.tpe.typeSymbol.initialize

          if (!context.unit.isJava)
            checkStablePrefixClassType(parent)

          if (psym != superclazz) {
            if (context.unit.isJava && psym.isJavaAnnotation) {
              // allowed
            } else if (psym.isTrait) {
              val ps = psym.info.parents
              if (!ps.isEmpty && !superclazz.isSubClass(ps.head.typeSymbol))
                pending += ParentSuperSubclassError(parent, superclazz, ps.head.typeSymbol, psym)
              if (!clazzIsTrait) {
                def hasTraitParams(sym: Symbol) =
                  sym.isScala3Defined && sym.isTrait && sym.hasAttachment[DottyParameterisedTrait]
                // TODO perhaps there can be a flag to skip this when we know there can be no Scala 3 definitions
                // or otherwise use an optimised representation for trait parameters
                (parent.tpe :: ps).collectFirst {
                  case p if hasTraitParams(p.typeSymbol) =>
                    p.typeSymbol.attachments.get[DottyParameterisedTrait].foreach( attach =>
                      pending += ParentIsScala3TraitError(parent, p.typeSymbol, attach.params, psym)
                    )
                }
              }
            } else {
              pending += ParentNotATraitMixinError(parent, psym)
            }
          }

          if (psym.isFinal)
            pending += ParentFinalInheritanceError(parent, psym)

          val sameSourceFile = context.unit.source.file == psym.sourceFile

          if (!isPastTyper && psym.hasDeprecatedInheritanceAnnotation &&
            !sameSourceFile && !context.owner.ownerChain.exists(_.isDeprecated)) {
            val version = psym.deprecatedInheritanceVersion.getOrElse("")
            val since   = if (version.isEmpty) version else s" (since $version)"
            val message = psym.deprecatedInheritanceMessage.map(msg => s": $msg").getOrElse("")
            val report  = s"inheritance from ${psym.fullLocationString} is deprecated$since$message"
            context.deprecationWarning(parent.pos, psym, report, version)
          }

          val parentTypeOfThis = parent.tpe.dealias.typeOfThis

          if (!phase.erasedTypes &&
              !context.owner.isSynthetic &&   // don't check synthetic concrete classes for virtuals (part of DEVIRTUALIZE)
              !context.unit.isJava &&         // don't check self types for Java (scala/bug#11917)
              !selfType.isErroneous &&
              !parent.tpe.isErroneous &&
              !(selfType <:< parentTypeOfThis)
          ) {
            pending += ParentSelfTypeConformanceError(parent, selfType)
            if (settings.explaintypes.value) explainTypes(selfType, parentTypeOfThis)
          }

          if (parents exists (p => p != parent && p.tpe.typeSymbol == psym && !psym.isError))
            pending += ParentInheritedTwiceError(parent, psym)

          validateDynamicParent(psym, parent.pos)
        }

      if (!parents.isEmpty && parents.forall(!_.isErrorTyped)) {
        val superclazz = parents.head.tpe.typeSymbol
        parents.foreach(validateParentClass(_, superclazz))
      }

      pending.foreach(ErrorUtils.issueTypeError)
    }

    def checkFinitary(classinfo: ClassInfoType): Unit = {
      val clazz = classinfo.typeSymbol

      for (tparam <- clazz.typeParams) {
        if (classinfo.expansiveRefs(tparam) contains tparam) {
          val newinfo = ClassInfoType(
            classinfo.parents map (_.instantiateTypeParams(List(tparam), List(AnyRefTpe))),
            classinfo.decls,
            clazz)
          updatePolyClassInfo(clazz, newinfo)
          FinitaryError(tparam)
        }
      }
    }

    private def updatePolyClassInfo(clazz: Symbol, newinfo: ClassInfoType): clazz.type = {
      clazz.setInfo {
        clazz.info match {
          case PolyType(tparams, _) => PolyType(tparams, newinfo)
          case _ => newinfo
        }
      }
    }

    def typedClassDef(cdef: ClassDef): Tree = {
      val clazz = cdef.symbol
      currentRun.profiler.beforeTypedImplDef(clazz)
      try {
        val typedMods = typedModifiers(cdef.mods)
        assert(clazz != NoSymbol, cdef)
        reenterTypeParams(cdef.tparams)
        val tparams1 = cdef.tparams.mapConserve(typedTypeDef)
        val impl1 = newTyper(context.make(cdef.impl, clazz, newScope)).typedTemplate(cdef.impl, typedParentTypes(cdef.impl))
        val impl2 = finishMethodSynthesis(impl1, clazz, context)
        if (clazz.isTrait && clazz.info.parents.nonEmpty && clazz.info.firstParent.typeSymbol == AnyClass)
          checkEphemeral(clazz, impl2.body)

        warnTypeParameterShadow(tparams1, clazz)

        treeCopy.ClassDef(cdef, typedMods, cdef.name, tparams1, impl2)
          .setType(NoType)
      } finally {
        currentRun.profiler.afterTypedImplDef(clazz)
      }
    }

    def typedModuleDef(mdef: ModuleDef): Tree = {
      // initialize all constructors of the linked class: the type completer (Namer.methodSig)
      // might add default getters to this object. example: "object T; class T(x: Int = 1)"
      val linkedClass = companionSymbolOf(mdef.symbol, context)
      if (linkedClass != NoSymbol)
        linkedClass.info.decl(nme.CONSTRUCTOR).alternatives foreach (_.initialize)

      val clazz = mdef.symbol.moduleClass
      currentRun.profiler.beforeTypedImplDef(clazz)
      try {

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
              TypeTree(SerializableTpe).setPos(clazz.pos.focus) :: Nil
            }
            )
        })

        val impl2 = finishMethodSynthesis(impl1, clazz, context)

        if (mdef.symbol == PredefModule)
          ensurePredefParentsAreInSameSourceFile(impl2)

        treeCopy.ModuleDef(mdef, typedMods, mdef.name, impl2) setType NoType
      } finally {
        currentRun.profiler.afterTypedImplDef(clazz)
      }
    }

    private def ensurePredefParentsAreInSameSourceFile(template: Template) = {
      val PredefModuleFile = PredefModule.associatedFile
      if (template.parents.exists(p => p.symbol != AnyRefClass && p.symbol.associatedFile != PredefModuleFile))
        context.error(template.pos, s"All parents of Predef must be defined in $PredefModuleFile.")
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

    def typedTemplate(templ0: Template, parents1: List[Tree]): Template = {
      val templ = templ0
      // please FIXME: uncommenting this line breaks everything
      // val templ = treeCopy.Template(templ0, templ0.body, templ0.self, templ0.parents)
      val clazz = context.owner

      val parentTypes = parents1.map(_.tpe)

      // The parents may have been normalized by typedParentTypes.
      // We must update the info as well, or we won't find the super constructor for our now-first parent class
      // Consider `class C ; trait T extends C ; trait U extends T`
      // `U`'s info will start with parent `T`, but `typedParentTypes` will return `List(C, T)` (`== parents1`)
      // now, the super call in the primary ctor will fail to find `C`'s ctor, since it bases its search on
      // `U`'s info, not the trees.
      //
      // For correctness and performance, we restrict this rewrite to anonymous classes,
      // as others have their parents in order already (it seems!), and we certainly
      // don't want to accidentally rewire superclasses for e.g. the primitive value classes.
      //
      // TODO: Find an example of a named class needing this rewrite, I tried but couldn't find one.
      if (clazz.isAnonymousClass && clazz.info.parents != parentTypes) {
//        println(s"updating parents of $clazz from ${clazz.info.parents} to $parentTypes")
        updatePolyClassInfo(clazz, ClassInfoType(parentTypes, clazz.info.decls, clazz))
      }

      clazz.annotations.foreach(_.completeInfo())
      if (templ.symbol == NoSymbol)
        templ setSymbol clazz.newLocalDummy(templ.pos)
      val self1 = (templ.self: @unchecked) match {
        case vd @ ValDef(_, _, tpt, EmptyTree) =>
          val tpt1 = checkNoEscaping.privates(
            this,
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

      val selfType =
        if (clazz.isAnonymousClass && !phase.erasedTypes)
          intersectionType(clazz.info.parents, clazz.owner)
        else
          clazz.typeOfThis

      // the following is necessary for templates generated later
      assert(clazz.info.decls != EmptyScope, clazz)
      val body1 = pluginsEnterStats(this, namer.expandMacroAnnotations(templ.body))
      enterSyms(context.outer.make(templ, clazz, clazz.info.decls), body1)
      if (!templ.isErrorTyped) // if `parentTypes` has invalidated the template, don't validate it anymore
        validateParentClasses(parents1, selfType, clazz.isTrait)
      if (clazz.isCase)
        validateNoCaseAncestor(clazz)
      if (clazz.isTrait && hasSuperArgs(parents1.head))
        ConstrArgsInParentOfTraitError(parents1.head, clazz)

      if (!phase.erasedTypes && !clazz.info.resultType.isError) // @S: prevent crash for duplicated type members
        checkFinitary(clazz.info.resultType.asInstanceOf[ClassInfoType])

      val bodyWithPrimaryCtor = {
        val primaryCtor = treeInfo.firstConstructor(body1)
        val primaryCtor1 = primaryCtor match {
          case DefDef(_, _, _, _, _, Block(earlyVals :+ global.pendingSuperCall, unit)) =>
            val argss = superArgs(parents1.head) getOrElse Nil
            val pos = wrappingPos(parents1.head.pos, primaryCtor :: argss.flatten).makeTransparent
            val superCall = atPos(pos)(PrimarySuperCall(argss))
            deriveDefDef(primaryCtor)(_ => Block(earlyVals :+ superCall, unit) setPos pos) setPos pos
          case _ => primaryCtor
        }
        body1 mapConserve { case `primaryCtor` => primaryCtor1; case stat => stat }
      }

      val body3 = typedStats(bodyWithPrimaryCtor, templ.symbol)

      if (clazz.info.firstParent.typeSymbol == AnyValClass)
        validateDerivedValueClass(clazz, body3)

      if (!clazz.isTrait && clazz.isNonBottomSubClass(ConstantAnnotationClass)) {
        val ctors = body3.iterator.collect { case method: DefDef if method.symbol.isConstructor => method }
        val primary = ctors.next() // there is always a primary constructor
        if (primary.symbol.paramss.lengthIs != 1) ConstantAnnotationNeedsSingleArgumentList(primary, clazz)
        ctors.foreach(AuxConstrInConstantAnnotation(_, clazz))
      }

      if (clazz.isTrait) {
        for (decl <- clazz.info.decls)
          if (decl.isTerm && decl.isEarlyInitialized)
            context.warning(decl.pos, "Implementation restriction: early definitions in traits are not initialized before the super class is initialized.", WarningCategory.Other)
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
      currentRun.profiler.beforeTypedImplDef(sym)
      try {
        val valDefTyper = {
          val maybeConstrCtx =
            if ((sym.isParameter || sym.isEarlyInitialized) && sym.owner.isConstructor) context.makeConstructorContext
            else context
          newTyper(maybeConstrCtx.makeNewScope(vdef, sym))
        }
        valDefTyper.typedValDefImpl(vdef)
      } finally {
        currentRun.profiler.afterTypedImplDef(sym)
      }
    }

    // use typedValDef instead. this version is called after creating a new context for the ValDef
    private def typedValDefImpl(vdef: ValDef): ValDef = {
      val sym = vdef.symbol.initialize
      val typedMods = if (nme.isLocalName(sym.name) && sym.isPrivateThis && !vdef.mods.isPrivateLocal) {
        // scala/bug#10009 This tree has been given a field symbol by `enterGetterSetter`, patch up the
        // modifiers accordingly so that we can survive resetAttrs and retypechecking.
        // Similarly, we use `sym.name` rather than `vdef.name` below to use the local name.
        typedModifiers(vdef.mods.copy(flags = sym.flags, privateWithin = tpnme.EMPTY))
      } else typedModifiers(vdef.mods)

      sym.annotations.foreach(_.completeInfo())
      sym.filterAnnotations(_ != UnmappableAnnotation)

      val tpt1 = checkNoEscaping.privates(this, sym, transformedOr(vdef.tpt, typedType(vdef.tpt)))
      checkNonCyclic(vdef, tpt1)

      // allow trait accessors: it's the only vehicle we have to hang on to annotations that must be passed down to
      // the field that's mixed into a subclass
      if (sym.hasAnnotation(definitions.VolatileAttr) && !((sym hasFlag MUTABLE | LAZY) || (sym hasFlag ACCESSOR) && sym.owner.isTrait))
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
            val subst = new SubstTypeMap(tparams, WildcardType.fillList(tparams.length)) {
              @tailrec
              override def matches(sym: Symbol, sym1: Symbol) =
                if (sym.isSkolem) matches(sym.deSkolemize, sym1)
                else if (sym1.isSkolem) matches(sym, sym1.deSkolemize)
                else super.matches(sym, sym1)
            }
            // allow defaults on by-name parameters
            if (sym hasFlag BYNAMEPARAM)
              if (tpt1.tpe.typeArgs.isEmpty) WildcardType // during erasure tpt1 is Function0
              else subst(tpt1.tpe.typeArgs.head)
            else subst(tpt1.tpe)
          } else tpt1.tpe
          transformedOrTyped(vdef.rhs, EXPRmode | BYVALmode, tpt2)
        }
      val vdef1 = treeCopy.ValDef(vdef, typedMods, sym.name, tpt1, checkDead(context, rhs1)) setType NoType
      if (sym.isSynthetic && sym.name.startsWith(nme.RIGHT_ASSOC_OP_PREFIX))
        rightAssocValDefs += ((sym, vdef1.rhs))
      if (vdef.hasAttachment[PatVarDefAttachment.type])
        sym.updateAttachment(PatVarDefAttachment)
      if (sym.isSynthetic && sym.owner.isClass && (tpt1.tpe eq UnitTpe) && vdef.hasAttachment[PatVarDefAttachment.type] && sym.isPrivateThis && vdef.mods.isPrivateLocal && !sym.enclClassChain.exists(_.isInterpreterWrapper)) {
        context.warning(vdef.pos, s"Pattern definition introduces Unit-valued member of ${sym.owner.name}; consider wrapping it in `locally { ... }`.", WarningCategory.OtherMatchAnalysis)
      }
      vdef1
    }

    /** Analyze the super constructor call to record information used later to compute parameter aliases */
    def analyzeSuperConstructor(meth: Symbol, vparamss: List[List[ValDef]], rhs: Tree): Unit = if (!rhs.isErrorTyped) {
      val clazz = meth.owner
      debuglog(s"computing param aliases for $clazz:${clazz.primaryConstructor.tpe}:$rhs")
      val pending = ListBuffer[AbsTypeError]()

      // !!! This method is redundant with other, less buggy ones.
      def decompose(call: Tree): (Tree, List[Tree]) = call match {
        case Apply(fn, args) =>
          // an object cannot be allowed to pass a reference to itself to a superconstructor
          // because of initialization issues; scala/bug#473, scala/bug#3913, scala/bug#6928.
          foreachSubTreeBoundTo(args, clazz) { tree =>
            if (tree.symbol.isModule)
              pending += SuperConstrReferenceError(tree)
            tree match {
              case This(_) =>
                pending += SuperConstrArgsThisReferenceError(tree)
              case _ => ()
            }
          }
          val (superConstr, preArgs) = decompose(fn)
          val params = fn.tpe.params
          // appending a dummy tree to represent Nil for an empty varargs (is this really necessary?)
          val applyArgs = if (args.sizeCompare(params) < 0) args :+ EmptyTree else args take params.length

          assert(sameLength(applyArgs, params) || call.isErrorTyped,
            s"arity mismatch but call is not error typed: $clazz (params=$params, args=$applyArgs)")

          (superConstr, preArgs ::: applyArgs)
        case Block(_ :+ superCall, _) =>
          decompose(superCall)
        case _ =>
          (call, Nil)
      }

      // associate superclass paramaccessors with their aliases
      val (superConstr, superArgs) = decompose(rhs)
      if (superConstr.symbol.isPrimaryConstructor) {
        val superClazz = superConstr.symbol.owner
        if (!superClazz.isJavaDefined) {
          val superParamAccessors = superClazz.constrParamAccessors
          if (sameLength(superParamAccessors, superArgs)) {
            val accToSuperAcc = mutable.AnyRefMap[Symbol, Symbol]()
            for ((superAcc, superArg@Ident(name)) <- superParamAccessors zip superArgs) {
              if (mexists(vparamss)(_.symbol == superArg.symbol)) {
                val ownAcc = clazz.info decl name suchThat (_.isParamAccessor) match {
                  case acc if !acc.isDeferred && acc.hasAccessorFlag => acc.accessed
                  case acc => acc
                }
                ownAcc match {
                  case acc: TermSymbol if !acc.isVariable && !isByNameParamType(acc.info) =>
                    accToSuperAcc(acc) = superAcc
                  case _ =>
                }
              }
            }
            if (!accToSuperAcc.isEmpty) {
              superConstructorCalls(clazz) = accToSuperAcc
            }
          }
        }
      }

      pending.foreach(ErrorUtils.issueTypeError)
    }

    // Check for scala/bug#4842.
    private def checkSelfConstructorArgs(ddef: DefDef, clazz: Symbol): Unit = {
      val pending = ListBuffer[AbsTypeError]()
      ddef.rhs match {
        case Block(stats, expr) =>
          val selfConstructorCall = stats.headOption.getOrElse(expr)
          foreachSubTreeBoundTo(List(selfConstructorCall), clazz) {
            case tree @ This(_) =>
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
      def parentString = meth.owner.parentSymbols.filter(_ != ObjectClass) match {
        case Nil => ""
        case xs  => xs.map(_.nameString).mkString(" (of ", " with ", ")")
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

      def failStruct(pos: Position, member: String, referTo: String): Unit =
        context.error(pos, s"$member in structural refinement may not refer to $referTo")
      def failStructAbstractType(pos: Position, member: String): false = {
        failStruct(pos, member, referTo="an abstract type defined outside that refinement")
        false
      }
      def failStructTypeMember(pos: Position, member: String): false = {
        failStruct(pos, member, referTo="a type member of that refinement")
        false
      }

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
              (    (!sym.hasTransOwner(meth.owner) && failStructAbstractType(paramPos, what))
                || (!sym.hasTransOwner(meth) && failStructTypeMember(paramPos, what))
                || checkAbstract(sym.info.upperBound, "Type bound")
              )
            }
            tp0.dealiasWidenChain forall (t => check(t.typeSymbol))
          }
          checkAbstract(paramType, "Parameter type")

          if (sym.isDerivedValueClass)
            failStruct(paramPos, member="Parameter type", referTo="a user-defined value class")
          if (paramType.isInstanceOf[ThisType] && sym == meth.owner)
            failStruct(paramPos, member="Parameter type", referTo="the type of that refinement (self type)")
        }
      }
      if (resultType.typeSymbol.isDerivedValueClass)
        failStruct(ddef.tpt.pos, member="Result type", referTo="a user-defined value class")
    }

    def typedDefDef(ddef: DefDef): DefDef = {
      val meth = ddef.symbol.initialize
      currentRun.profiler.beforeTypedImplDef(meth)
      try {

        reenterTypeParams(ddef.tparams)
        reenterValueParams(ddef.vparamss)

        // for `val` and `var` parameter, look at `target` meta-annotation
        if (!isPastTyper && meth.isPrimaryConstructor) {
          for (vparams <- ddef.vparamss; vd <- vparams) {
            if (vd.mods.isParamAccessor) {
              val sym = vd.symbol
              sym.setAnnotations(sym.annotations.filter(AnnotationInfo.mkFilter(ParamTargetClass, defaultRetention = true)))
            }
          }
        }

        val tparams1 = ddef.tparams.mapConserve(typedTypeDef)
        val vparamss1 = ddef.vparamss.mapConserve(_.mapConserve(typedValDef))

        warnTypeParameterShadow(tparams1, meth)

        meth.annotations.foreach(_.completeInfo())
        // we only have to move annotations around for accessors -- see annotSig as used by AccessorTypeCompleter and ValTypeCompleter
        if (meth.isAccessor) meth.filterAnnotations(_ != UnmappableAnnotation)

        for (vparams1 <- vparamss1; vparam1 <- vparams1 dropRight 1)
          if (isRepeatedParamType(vparam1.symbol.tpe))
            StarParamNotLastError(vparam1)

        val tpt1 = checkNoEscaping.privates(this, meth, transformedOr(ddef.tpt, typedType(ddef.tpt)))
        checkNonCyclic(ddef, tpt1)
        ddef.tpt.setType(tpt1.tpe)
        val typedMods = typedModifiers(ddef.mods)
        val rhsAtOwner = // introduced for async, but could be universally handy
          ddef.getAndRemoveAttachment[ChangeOwnerAttachment] match {
            case None => ddef.rhs
            case Some(ChangeOwnerAttachment(originalOwner)) => ddef.rhs.changeOwner(originalOwner, ddef.symbol)
          }
        var rhs1 =
          if (ddef.name == nme.CONSTRUCTOR && !ddef.symbol.hasStaticFlag) { // need this to make it possible to generate static ctors
            if (!meth.isPrimaryConstructor &&
              (!meth.owner.isClass ||
                meth.owner.isModuleClass ||
                meth.owner.isAnonOrRefinementClass))
              InvalidConstructorDefError(ddef)
            typed(rhsAtOwner)
          } else if (meth.isMacro) {
            // typechecking macro bodies is sort of unconventional
            // that's why we employ our custom typing scheme orchestrated outside of the typer
            transformedOr(rhsAtOwner, typedMacroBody(this, ddef))
          } else {
            transformedOrTyped(rhsAtOwner, EXPRmode, tpt1.tpe)
          }

        if (meth.isClassConstructor && !isPastTyper && !meth.owner.isSubClass(AnyValClass) && !meth.isJava) {
          // There are no supercalls for AnyVal or constructors from Java sources, which
          // would blow up in analyzeSuperConstructor; there's nothing to be computed for them anyway.
          if (meth.isPrimaryConstructor)
            analyzeSuperConstructor(meth, vparamss1, rhs1)
          else
            checkSelfConstructorArgs(ddef, meth.owner)
        }

        if (tpt1.tpe.typeSymbol != NothingClass && !context.returnsSeen && rhs1.tpe.typeSymbol != NothingClass)
          rhs1 = checkDead(context, rhs1)

        if (!isPastTyper) {
          if (meth.owner.isClass && meth.paramss.exists(ps => ps.exists(_.hasDefault) && isRepeatedParamType(ps.last.tpe)))
            StarWithDefaultError(meth)

          for (pp <- meth.paramss; p <- pp)
            p.deprecatedParamName match {
              case Some(nme.NO_NAME) | None =>
              case Some(alt) =>
                if (mexists(meth.paramss)(p1 => p != p1 && (p1.name == alt || p1.deprecatedParamName.contains(alt))))
                  DeprecatedParamNameError(p, alt)
            }

          if (settings.multiargInfix && !meth.isConstructor && meth.owner.isClass && !meth.isDeprecated && !meth.hasAnnotation(UnusedClass) && !meth.ownerChain.exists(_.isDeprecated) && !meth.isSynthetic)
            meth.paramss match {
              case (h :: _ :: _) :: Nil if !h.isImplicit && Chars.isOperatorPart(meth.name.decoded.head) =>
                warnMultiargInfix(ddef)
              case _ =>
            }

          if (meth.isStructuralRefinementMember)
            checkMethodStructuralCompatible(ddef)

          if (meth.isImplicit) {
            if (!meth.isSynthetic) meth.paramss match {
              case List(param) :: _ if !param.isImplicit =>
                checkFeature(ddef.pos, currentRun.runDefinitions.ImplicitConversionsFeature, meth.toString)
              case _ =>
            }
            if (meth.isGetter && !meth.isLocalToBlock && meth.accessed.hasAttachment[FieldTypeInferred.type]) {
              meth.accessed.removeAttachment[FieldTypeInferred.type]
              InferredImplicitError(ddef, meth.accessed.tpe.resultType, context)
            }
          }
        }
        treeCopy.DefDef(ddef, typedMods, ddef.name, tparams1, vparamss1, tpt1, rhs1) setType NoType
      } finally {
        currentRun.profiler.afterTypedImplDef(meth)
      }
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
      tdef.symbol.annotations.foreach(_.completeInfo())

      warnTypeParameterShadow(tparams1, tdef.symbol)

      // @specialized should not be pickled when compiling with -no-specialize
      if (settings.nospecialization.value && currentRun.compiles(tdef.symbol)) {
        tdef.symbol.removeAnnotation(definitions.SpecializedClass)
        tdef.symbol.deSkolemize.removeAnnotation(definitions.SpecializedClass)
      }

      val rhs1 = checkNoEscaping.privates(this, tdef.symbol, typedType(tdef.rhs))
      checkNonCyclic(tdef.symbol)
      if (tdef.symbol.owner.isType)
        rhs1.tpe match {
          case TypeBounds(lo1, hi1) if !(lo1 <:< hi1) => LowerBoundError(tdef, lo1, hi1)
          case _                                      => ()
        }

      treeCopy.TypeDef(tdef, typedMods, tdef.name, tparams1, rhs1) setType NoType
    }

    private def enterLabelDef(stat: Tree): Unit = {
      stat match {
        case ldef @ LabelDef(_, _, _) =>
          if (ldef.symbol == NoSymbol)
            ldef.symbol = namer.enterInScope(
              context.owner.newLabel(ldef.name, ldef.pos) setInfo MethodType(Nil, UnitTpe))
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
            context.owner.newLabel(ldef.name, ldef.pos) setInfo MethodType(Nil, restpe))
          val LabelDef(_, _, rhs1) = resetAttrs(ldef): @unchecked
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
        val block = treeCopy.Block(block0, pluginsEnterStats(this, namer.expandMacroAnnotations(block0.stats)), block0.expr)
        for (stat <- block.stats) enterLabelDef(stat)

        if (phaseId(currentPeriod) <= currentRun.typerPhase.id) {
          // This is very tricky stuff, because we are navigating the Scylla and Charybdis of
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
              lazy val visibleMembers =
                pt.members match {
                  case _: ErrorScope => classDecls.toList
                  case ms => ms
                }

              def matchesVisibleMember(member: Symbol) = visibleMembers exists { vis =>
                (member.name == vis.name) &&
                (member.tpe <:< vis.tpe.substThis(vis.owner, classDef.symbol))
              }
              // The block is an anonymous class definitions/instantiation pair
              //   -> members that are hidden by the type of the block are made private
              classDecls foreach { toHide =>
                if (toHide.isTerm
                    && toHide.isPossibleInRefinement
                    && toHide.isPublic
                    && !matchesVisibleMember(toHide)) {
                  (toHide
                   resetFlag (PROTECTED | LOCAL)
                   setFlag (PRIVATE | SYNTHETIC_PRIVATE)
                   setPrivateWithin NoSymbol)

                  syntheticPrivates += toHide
                }
              }

            case _ =>
          }
        }
        val statsTyped = typedStats(block.stats, context.owner)
        val expr1 = typed(block.expr, mode &~ (FUNmode | QUALmode | APPSELmode), pt)

        // Remove ValDef for right-associative by-value operator desugaring which has been inlined into expr1
        val statsTyped2 = statsTyped match {
          case (vd: ValDef) :: Nil if inlinedRightAssocValDefs.remove(vd.symbol) => Nil
          case _ => statsTyped
        }

        treeCopy.Block(block, statsTyped2, expr1)
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
      val selector1  = checkDead(context, typedByValueExpr(selector))
      val selectorTp = packCaptured(selector1.tpe.widen).skolemizeExistential(context.owner, selector)
      val casesTyped = typedCases(cases, selectorTp, pt)

      def initChildren(sym: Symbol): Unit =
        if (sym.isJava && sym.isSealed)
          sym.attachments.get[PermittedSubclassSymbols] match {
            case Some(PermittedSubclassSymbols(permits)) =>
              for (child <- permits if child.isJava)
                initChildren(child.initialize)
            case _ =>
              val seen = mutable.HashSet.empty[Symbol]
              def populate(): Unit =
                patmat.javaClassesByUnit.get(sym.pos.source) match {
                  case Some(classes) =>
                    classes.find(!seen(_)) match {
                      case Some(unseen) =>
                        seen += unseen
                        unseen.initialize.companionSymbol.moduleClass.initialize
                        if (unseen.hasAttachment[PermittedSubclassSymbols]) initChildren(unseen)
                        populate()
                      case _ =>
                    }
                  case _ =>
                }
              populate()
          }
      initChildren(selectorTp.typeSymbol)

      def finish(cases: List[CaseDef], matchType: Type) = {
        if (!isPastTyper && settings.warnPatternShadow && !context.owner.isSynthetic)
          for (cdef <- cases; bind @ Bind(name, _) <- cdef.pat if !bind.hasAttachment[NoWarnAttachment.type])
            context.lookupSymbol(name, _ => true) match {
              case LookupSucceeded(_, sym) => bind.updateAttachment(PatShadowAttachment(sym))
              case _ =>
            }
        treeCopy.Match(tree, selector1, cases).setType(matchType)
      }

      if (isFullyDefined(pt))
        finish(casesTyped, pt)
      else packedTypes(casesTyped) match {
        case packed if sameWeakLubAsLub(packed) => finish(casesTyped, lub(packed))
        case packed                             =>
          val lub = weakLub(packed)
          finish(casesTyped.map(adaptCase(_, mode, lub)), lub)
      }
    }

    /** synthesize and type check a PartialFunction implementation based on the match in `tree`
     *
     *  `param => sel match { cases }` becomes:
     *
     *  new AbstractPartialFunction[\$argTp, \$matchResTp] {
     *    def applyOrElse[A1 <: \$argTp, B1 >: \$matchResTp](\$param: A1, default: A1 => B1): B1 =
     *       \$selector match { \$cases }
     *    def isDefinedAt(x: \$argTp): Boolean =
     *       \$selector match { \$casesTrue }
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
      val (argTp, resTp) = partialFunctionArgResTypeFromProto(pt)

      // if argTp isn't fully defined, we can't translate --> error
      // NOTE: resTp still might not be fully defined
      if (!isFullyDefined(argTp)) {
        MissingParameterTypeAnonMatchError(tree, pt)
        return setError(tree)
      }

      // targs must conform to Any for us to synthesize an applyOrElse (fallback to apply otherwise -- typically for @cps annotated targs)
      val targsValidParams = (argTp <:< AnyTpe) && (resTp <:< AnyTpe)

      val anonClass = context.owner newAnonymousFunctionClass tree.pos addAnnotation SerialVersionUIDAnnotation

      import CODE._

      val Match(sel, cases) = tree: @unchecked

      // need to duplicate the cases before typing them to generate the apply method, or the symbols will be all messed up
      val casesTrue = cases.map(deriveCaseDef(_)(x => atPos(x.pos.focus)(TRUE)).duplicate)

      // must generate a new tree every time
      def selector(paramSym: Symbol): Tree = gen.mkUnchecked(
        if (sel != EmptyTree) sel.duplicate
        else atPos(tree.pos.focusStart)(
          // scala/bug#6925: subsume type of the selector to `argTp`
          // we don't want/need the match to see the `A1` type that we must use for variance reasons in the method signature
          //
          // this failed: replace `selector` by `Typed(selector, TypeTree(argTp))` -- as it's an upcast, this should never fail,
          //   `(x: A1): A` doesn't always type check, even though `A1 <: A`, due to singleton types (test/files/pos/t4269.scala)
          // hence the cast, which will be erased in posterasure
          // (the cast originally caused  extremely weird types to show up
          //  in test/scaladoc/run/scala/bug#5933.scala because `variantToSkolem` was missing `tpSym.initialize`)
          gen.mkCastPreservingAnnotations(Ident(paramSym), argTp)
        ))

      def mkParam(methodSym: Symbol, tp: Type = argTp) =
        methodSym.newValueParameter(paramName, paramPos.focus, SYNTHETIC) setInfo tp

      def mkDefaultCase(body: Tree) =
        atPos(tree.pos.makeTransparent) {
          CaseDef(Bind(nme.DEFAULT_CASE, Ident(nme.WILDCARD)), body)
        }

      def synthMethodTyper(methodSym: MethodSymbol) = {
        val ctx = context.makeNewScope(context.tree, methodSym)
        // scala/bug#10291 make sure `Return`s are linked to the original enclosing method, not the one we're synthesizing
        ctx.enclMethod = context.enclMethod
        newTyper(ctx)
      }

      // `def applyOrElse[A1 <: $argTp, B1 >: $matchResTp](x: A1, default: A1 => B1): B1 =
      //  ${`$selector match { $cases; case default$ => default(x) }`
      def applyOrElseMethodDef = {
        val methodSym = anonClass.newMethod(nme.applyOrElse, tree.pos, FINAL | OVERRIDE)

        // create the parameter that corresponds to the function's parameter
        val A1 = methodSym.newTypeParameter(newTypeName("A1")).setInfo(TypeBounds.upper(argTp))
        val x = mkParam(methodSym, A1.tpe)

        // applyOrElse's default parameter:
        val B1 = methodSym.newTypeParameter(newTypeName("B1")).setInfo(TypeBounds.empty)
        val default = methodSym.newValueParameter(newTermName("default"), tree.pos.focus, SYNTHETIC) setInfo functionType(List(A1.tpe), B1.tpe)

        val paramSyms = List(x, default)
        methodSym setInfo genPolyType(List(A1, B1), MethodType(paramSyms, B1.tpe))

        val methodBodyTyper = synthMethodTyper(methodSym)
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
          val cdef = mkDefaultCase(methodBodyTyper.typed1(REF(default).APPLY(REF(x)), mode, B1.tpe).setType(B1.tpe))
          val List(defaultCase) = methodBodyTyper.typedCases(List(cdef), argTp, B1.tpe): @unchecked
          treeCopy.Match(match0, match0.selector, match0.cases :+ defaultCase)
        }
        match_ setType B1.tpe

        // scala/bug#6187 Do you really want to know? Okay, here's what's going on here.
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
          val tt @ TypeTree() = vd.tpt: @unchecked
          tt setOriginal (originals(param) setPos param.pos.focus)
          vd
        }

        val defdef = newDefDef(methodSym, match_)(vparamss = mapParamss(methodSym)(newParam), tpt = TypeTree(B1.tpe))

        (defdef, matchResTp)
      }

      // `def isDefinedAt(x: $argTp): Boolean = ${`$selector match { $casesTrue; case default$ => false } }`
      def isDefinedAtMethod = {
        val methodSym = anonClass.newMethod(nme.isDefinedAt, tree.pos.makeTransparent, FINAL)
        val paramSym = mkParam(methodSym)

        val methodBodyTyper = synthMethodTyper(methodSym) // should use the DefDef for the context's tree, but it doesn't exist yet (we need the typer we're creating to create it)
        if (!paramSynthetic) methodBodyTyper.context.scope enter paramSym
        methodSym setInfo MethodType(List(paramSym), BooleanTpe)

        val defaultCase = mkDefaultCase(FALSE)
        val match_ = methodBodyTyper.typedMatch(selector(paramSym), casesTrue :+ defaultCase, mode, BooleanTpe)

        DefDef(methodSym, match_)
      }

      // only used for @cps annotated partial functions
      // `def apply(x: $argTp): $matchResTp = $selector match { $cases }`
      def applyMethod = {
        val methodSym = anonClass.newMethod(nme.apply, tree.pos, FINAL | OVERRIDE)
        val paramSym = mkParam(methodSym)

        methodSym setInfo MethodType(List(paramSym), AnyTpe)

        val methodBodyTyper = synthMethodTyper(methodSym)
        if (!paramSynthetic) methodBodyTyper.context.scope enter paramSym

        val match_ = methodBodyTyper.typedMatch(selector(paramSym), cases, mode, resTp)

        val matchResTp = match_.tpe
        methodSym setInfo MethodType(List(paramSym), matchResTp) // patch info

        (DefDef(methodSym, match_), matchResTp)
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
          Apply(Select(New(Ident(anonClass.name).setSymbol(anonClass)), nme.CONSTRUCTOR), Nil)
        ))
      }

      if (typedBlock.isErrorTyped) typedBlock
      else // Don't leak implementation details into the type, see scala/bug#6575
        typedPos(tree.pos, mode, pt) {
          Typed(typedBlock, TypeTree(typedBlock.tpe baseType PartialFunctionClass))
        }
    }

    /** Synthesize and type check the implementation of a type with a Single Abstract Method.
      *
      * Based on a type checked Function node `{ (p1: T1, ..., pN: TN) => body } : S`
      * where `S` is the expected type that defines a single abstract method (call it `apply` for the example),
      * that has signature `(p1: T1', ..., pN: TN'): T'`, synthesize the instantiation of the following anonymous class
      *
      * {{{
      *   new S {
      *    def apply\$body(p1: T1, ..., pN: TN): T = body
      *    def apply(p1: T1', ..., pN: TN'): T' = apply\$body(p1,..., pN)
      *   }
      * }}}
      *
      * The `apply` method is identified by the argument `sam`; `S` corresponds to the argument `pt`,
      * If `pt` is not fully defined, we derive `samClassTpFullyDefined` by inferring any unknown type parameters.
      *
      * The types T1' ... TN' and T' are derived from the method signature of the sam method,
      * as seen from the fully defined `samClassTpFullyDefined`.
      *
      * The function's body is put in a (static) method in the class definition to enforce scoping.
      * S's members should not be in scope in `body`. (Putting it in the block outside the class runs into implementation problems described below)
      *
      * The restriction on implicit arguments (neither S's constructor, nor sam may take an implicit argument list),
      * is to keep the implementation of type inference (the computation of `samClassTpFullyDefined`) simple.
      *
      * Impl notes:
      *   - `fun` has a FunctionType, but the expected type `pt` is some SAM type -- let's remedy that
      *   - `fun` is fully attributed, so we'll have to wrangle some symbols into shape (owner change, vparam syms)
      *   - after experimentation, it works best to type check function literals fully first and then adapt to a sam type,
      *     as opposed to a sam-specific code paths earlier on in type checking (in typedFunction).
      *     For one, we want to emit the same bytecode regardless of whether the expected
      *     function type is a built-in FunctionN or some SAM type
      *
      */
    def inferSamType(fun: Tree, pt: Type, mode: Mode): Boolean =
      if (pt.isInstanceOf[OverloadedArgProto]) inferSamType(fun, pt.underlying, mode) // scala/bug#12560
      else fun match {
        case fun@Function(vparams, _) if !isFunctionType(pt) =>
          // TODO: can we ensure there's always a SAMFunction attachment, instead of looking up the sam again???
          // seems like overloading complicates things?
          val sam = samOfProto(pt)

          if (!samMatchesFunctionBasedOnArity(sam, vparams)) false
          else {
            def fullyDefinedMeetsExpectedFunTp(pt: Type): Boolean = isFullyDefined(pt) && {
              val samMethType = pt memberInfo sam
              fun.tpe <:< functionType(samMethType.paramTypes, samMethType.resultType)
            }

            val samTp =
              if (!sam.exists) NoType
              else if (fullyDefinedMeetsExpectedFunTp(pt)) pt
              else try {
                val ptFullyDefined = instantiateSamFromFunction(fun.tpe, pt, sam)
                if (ptFullyDefined <:< pt && fullyDefinedMeetsExpectedFunTp(ptFullyDefined)) {
                  debuglog(s"sam fully defined expected type: $ptFullyDefined from $pt for ${fun.tpe}")
                  ptFullyDefined
                } else {
                  debuglog(s"Could not define type $pt using ${fun.tpe} <:< ${pt memberInfo sam} (for $sam)")
                  NoType
                }
              } catch {
                case e@(_: NoInstance | _: TypeError) =>
                  debuglog(s"Error during SAM synthesis: could not define type $pt using ${fun.tpe} <:< ${pt memberInfo sam} (for $sam)\n$e")
                  NoType
              }

            if (samTp eq NoType) false
            else {
              /* Make a synthetic class symbol to represent the synthetic class that
             * will be spun up by LMF for this function. This is necessary because
             * it's possible that the SAM method might need bridges, and they have
             * to go somewhere. Erasure knows to compute bridges for these classes
             * just as if they were real templates extending the SAM type. */
              val synthCls = fun.symbol.owner.newClassWithInfo(
                name = tpnme.ANON_CLASS_NAME,
                parents = ObjectTpe :: samTp :: Nil,
                scope = newScope,
                pos = sam.pos,
                newFlags = SYNTHETIC | ARTIFACT
              )

              synthCls.info.decls.enter {
                val newFlags = (sam.flags & ~DEFERRED) | SYNTHETIC
                sam.cloneSymbol(synthCls, newFlags).setInfo(samTp memberInfo sam)
              }

              fun.setType(samTp)

              /* Arguably I should do `fun.setSymbol(samCls)` rather than leaning
             * on an attachment, but doing that confounds lambdalift's free var
             * analysis in a way which does not seem to be trivially reparable. */
              fun.updateAttachment(SAMFunction(samTp, sam, synthCls))

              true
            }
          }
        case _ => false
      }

    /**
      * Deconstruct an expected function-ish type `pt` into `numVparams` argument prototypes and a result prototype.
      *
      * If the expected type `pt` does not denote a function-ish type with arity `numVparams`,
      * still return the expected number of ErrorType/NoType argument protos, and WildcardType for the result.
      *
      * @return (argProtos, resProto) where argProtos.lengthCompare(numVparams) == 0
      */
    private def argsResProtosFromFun(pt: Type, numVparams: Int): (List[Type], Type) =
      pt match {
        case pt: OverloadedArgProto if pt.hofParamTypes.lengthCompare(numVparams) == 0 => (pt.hofParamTypes, WildcardType)
        case _                                                                         =>
          val FunctionSymbol = FunctionClass(numVparams)

          // In case of any non-trivial type slack between `pt` and the built-in function types, we go the SAM route,
          // as a subclass could have (crazily) implemented the apply method and introduced another abstract method
          // to serve as the vehicle.
          val ptNorm = pt.typeSymbol match {
            case NoSymbol                              => NoType
            case FunctionSymbol | PartialFunctionClass => pt
            case _                                     =>
              val sam = samOf(pt)
              if (sam.exists && sam.info.params.lengthCompare(numVparams) == 0)
                wildcardExtrapolation(methodToExpressionTp(pt memberInfo sam))
              else pt // allow type slack (pos/6221)
          }

          unwrapWrapperTypes(ptNorm baseType FunctionSymbol) match {
            case TypeRef(_, _, args :+ res) => (args, res) // if it's a TypeRef, we know its symbol will be FunctionSymbol
            case _ =>
              val dummyPt = if (pt == ErrorType) ErrorType else NoType
              (List.fill(numVparams)(dummyPt), WildcardType) // dummyPt is in CBN position
          }
      }


    /** Type check a function literal.
     *
     * Based on the expected type pt, potentially synthesize an instance of
     *   - PartialFunction,
     *   - a type with a Single Abstract Method.
     */
    private def typedFunction(fun: Function, mode: Mode, pt: Type): Tree = {
      val vparams = fun.vparams
      val numVparams = vparams.length
      if (numVparams > definitions.MaxFunctionArity) MaxFunctionArityError(fun, s", but $numVparams given")
      else {
        val (argProtos, resProto) = argsResProtosFromFun(pt, numVparams)

        // After typer, no need for further checks, parameter type inference or PartialFunction synthesis.
        if (isPastTyper) doTypedFunction(fun, resProto)
        else {
          val paramsMissingType = mutable.ArrayBuffer.empty[ValDef] //.sizeHint(numVparams) probably useless, since initial size is 16 and max fun arity is 22

          // first, try to define param types from expected function's arg types if needed
          foreach2(vparams, argProtos) { (vparam, argpt) =>
            // TODO: do we need to exclude vparam.symbol.isError? (I don't think so,
            // because I don't see how we could recurse after the `setError(vparam)` call below
            if (vparam.tpt.isEmpty) {
              if (isFullyDefined(argpt)) vparam.tpt setType argpt
              else if (vparam.hasAttachment[BooleanParameterType.type]) vparam.tpt.setType(definitions.BooleanTpe) // `if (_)`
              else paramsMissingType += vparam

              if (!vparam.tpt.pos.isDefined) vparam.tpt setPos vparam.pos.focus
            }
          }

          if (paramsMissingType.nonEmpty && pt != ErrorType) {
            // If we can resolve the missing parameter type by undoing eta-expansion and recursing, do that -- otherwise, report error and bail
            typedFunctionUndoingEtaExpansion(fun, mode, resProto) orElse {
              // we ran out of things to try, missing parameter types are an irrevocable error
              var issuedMissingParameterTypeError = false
              paramsMissingType.foreach { vparam =>
                setError(vparam) //  see neg/t8675b.scala (we used to set vparam.tpt to ErrorType, but that isn't as effective)
                MissingParameterTypeError(fun, vparam, pt, withTupleAddendum = !issuedMissingParameterTypeError)
                issuedMissingParameterTypeError = true
              }

              setError(fun)
            }
          } else if (numVparams == 1 && pt.typeSymbol == PartialFunctionClass) { // dodge auto-tupling with the == 1
            // translate `x => x match { <cases> }` : PartialFunction to
            // `new PartialFunction { def applyOrElse(x, default) = x match { <cases> } def isDefinedAt(x) = ... }`
            val funBody = fun.body match {
              case Match(sel, _) if sel ne EmptyTree => fun.body
              case funBody                           =>
                atPos(funBody.pos.makeTransparent) {
                  Match(EmptyTree, List(CaseDef(Bind(nme.DEFAULT_CASE, Ident(nme.WILDCARD)), funBody)))
                }
            }
            // go to outer context -- must discard the context that was created for the Function since we're discarding the function
            // thus, its symbol, which serves as the current context.owner, is not the right owner
            // you won't know you're using the wrong owner until lambda lift crashes (unless you know better than to use the wrong owner)
            val outerTyper = newTyper(context.outer)
            val p = vparams.head
            if (p.tpt.tpe == null) p.tpt setType outerTyper.typedType(p.tpt).tpe

            outerTyper.synthesizePartialFunction(p.name, p.pos, paramSynthetic = false, funBody, mode, pt)
          } else doTypedFunction(fun, resProto)
        }
      }
    }

    /** Retry typedFunction when parameter types are missing, and they might be recovered from
      * the method selection that was eta-expanded into `fun`.
      *
      * When typing `(a1: T1, ..., aN: TN) => m(a1,..., aN)`, where some Ti are not fully defined,
      * type `m` directly (undoing eta-expansion of method m) to determine the argument types.
      * We have to be careful to use the result of typing the method selection, as its tree
      * may be rewritten.
      *
      * This tree is the result from one of:
      *   - manual eta-expansion with named arguments (x => f(x));
      *   - wildcard-style eta expansion (`m(_, _,)`);
      *   - (I don't think it can result from etaExpand, because we know the argument types there.)
      *
      * Note that method values are a separate thing (`m _`): they have the idiosyncratic shape
      * of `Typed(expr, Function(Nil, EmptyTree))`
      *
      * @return EmptyTree on failure, or a typed version of `fun` if we are successful
      */
    private def typedFunctionUndoingEtaExpansion(fun: Function, mode: Mode, resProto: Type) = {
      val vparams = fun.vparams

      fun.body match {
        // we can compare arguments and parameters by name because there cannot be a binder between
        // the function's valdefs and the Apply's arguments
        // If all vparams are constrained by the method application, see if we can derive expected types for them.
        // Note that not all method arguments need be references to a function param.
        case Apply(meth, args) =>
          // Map param with missing param type to the argument it's passed as in the eta-expanded method application
          // This list specifies a way to compute the expected parameter type for each of our function's arguments in order.
          // Either we already know it, and then we have a Type, or we don't, and then it's an index `idx` into
          // the arguments passed to `meth`, so we can derive it from its MethodType
          // (based on where the function's parameter is applied to `meth`)
          val formalsFromApply =
            vparams.map { vd =>
              if (!vd.tpt.isEmpty) {
                if (!vd.tpt.isTyped) {
                  val vd1 = typedValDef(vd)
                  if (vd1.isErroneous) Left(-1)
                  else Right(vd1.tpt.tpe)
                }
                else Right(vd.tpt.tpe)
              }
              else Left(args.indexWhere {
                case Ident(name) => name == vd.name
                case _           => false // TODO: this does not catch eta-expansion of an overloaded method that involves numeric widening scala/bug#9738 (and maybe similar patterns?)
              })
            }

          // If some of the vparams without type annotation was not applied to `meth`,
          // we're not going to learn enough from typing `meth` to determine them.
          if (formalsFromApply.contains(Left(-1))) EmptyTree
          else {
            // We're looking for a method (as indicated by FUNmode in the silent typed below),
            // so let's make sure our expected type is a MethodType (of the right arity, but we can't easily say more about the argument types)
            val methArgs = NoSymbol.newSyntheticValueParams(WildcardType.fillList(args.length))

            silent(_.typed(meth, mode.forFunMode, MethodType(methArgs, resProto))).fold(EmptyTree: Tree) { methTyped =>
              // if context.undetparams is not empty, the method was polymorphic,
              // so we need the missing arguments to infer its type. See #871
              if (context.undetparams.isEmpty) {
                // If we are sure this function type provides all the necessary info, so that we won't have
                // any undetermined argument types, recurse below (`typedFunction(fun, mode, ptUnrollingEtaExpansion)`)
                // and rest assured we won't end up right back here (and keep recursing).
                //
                // Be careful to reuse methTyped -- it may have changed from meth (scala/bug#9745)!
                //
                // TODO: CBN / varargs / implicits? should we use formalTypes?
                methodToExpressionTp(methTyped.tpe) match { // we don't know how many of the vparams of our function were actually applied to the method
                  case TypeRef(_, _, argProtos :+ _) =>
                    val argProtosRecovered =
                      formalsFromApply.map {
                        case Left(idx) =>
                          val argPt = if (argProtos.isDefinedAt(idx)) argProtos(idx) else NoType // bounds check should not be needed due to expected type `MethodType(methArgs, resProto)` above
                          if (isFullyDefined(argPt)) argPt else NoType
                        case Right(tp) => tp
                      }

                    if (argProtosRecovered contains NoType) EmptyTree // cannot safely recurse
                    else {
                      val funPt = functionType(argProtosRecovered, resProto)
                      // recursion is safe because now all parameter types can be derived from `argProtosRecovered` in the prototype `funPt` passed to typedFunction
                      typedFunction(treeCopy.Function(fun, vparams, treeCopy.Apply(fun.body, methTyped, args)), mode, funPt)
                    }
                  case _ => EmptyTree
                }
              } else EmptyTree
            }
          }
        case _ => EmptyTree
      }
    }

    // Assuming the expected number of parameters, which all have type annotations, do the happy path.
    private def doTypedFunction(fun: Function, bodyPt: Type) = {
      val vparams = fun.vparams
      val vparamSyms = vparams map { vparam =>
        enterSym(context, vparam)
        if (context.retyping) context.scope enter vparam.symbol
        vparam.symbol
      }
      val vparamsTyped = vparams mapConserve typedValDef
      val bodyTyped = typed(fun.body, bodyPt)

      val funSym = FunctionClass(vparams.length)
      val funTp =
        if (phase.erasedTypes) funSym.tpe
        else {
          val resTp =
            packedType(bodyTyped, fun.symbol).resultType.deconst match {
              case ct: ConstantType if (bodyPt eq WildcardType) || (ct.widen <:< bodyPt) => ct.widen
              case tp                                                                    => tp
            }

          appliedType(funSym, vparamSyms.map(_.tpe) :+ resTp)
        }

      treeCopy.Function(fun, vparamsTyped, bodyTyped) setType funTp
    }

    // #2624: need to infer type arguments for eta expansion of a polymorphic method
    // context.undetparams contains clones of meth.typeParams (fresh ones were generated in etaExpand)
    // need to run typer on tree0, since etaExpansion sets the tpe's of its subtrees to null
    // can't type with the expected type, as we can't recreate the setup in (3) without calling typed
    // (note that (3) does not call typed to do the polymorphic type instantiation --
    //  it is called after the tree has been typed with a polymorphic expected result type)
    def typedEtaExpansion(tree: Tree, mode: Mode, pt: Type): Tree = {
      debuglog(s"eta-expanding $tree: ${tree.tpe} to $pt")

      val expansion = etaExpand(tree, context.owner)
      if (context.undetparams.isEmpty) typed(expansion, mode, pt)
      else instantiate(typed(expansion, mode), mode, pt)
    }

    def typedRefinement(templ: Template): Unit = {
      val stats = templ.body
      if (!stats.isEmpty) {
        namer.enterSyms(stats)

        // need to delay rest of typedRefinement to avoid cyclic reference errors
        debuglog(s"deferred typed refinement")
        unit.addPostUnitCheck { () =>
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
    }

    def typedImport(imp: Import): Import = transformed.remove(imp) match {
      case Some(imp1: Import) => imp1
      case _                  => log(s"unhandled import: $imp in $unit"); imp
    }

    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val inBlock = exprOwner == context.owner
      def includesTargetPos(tree: Tree) =
        tree.pos.isRange && context.unit.exists && (tree.pos includes context.unit.targetPos)
      val localTarget = stats exists includesTargetPos
      def typedStat(stat: Tree): Tree = stat match {
        case s if context.owner.isRefinementClass && !treeInfo.isDeclarationOrTypeDef(s) => OnlyDeclarationsError(s)
        case imp @ Import(_, _) =>
          imp.symbol.initialize
          if (!imp.symbol.isError) {
            context = context.make(imp)
            typedImport(imp)
          } else EmptyTree
        // skip typechecking of statements in a sequence where some other statement includes the targetposition
        case s if localTarget && !includesTargetPos(s) => s
        case _ =>
          val localTyper = if (inBlock || (stat.isDef && !stat.isInstanceOf[LabelDef])) this
                           else newTyper(context.make(stat, exprOwner))
          // XXX this creates a spurious dead code warning if an exception is thrown
          // in a constructor, even if it is the only thing in the constructor.
          val result = checkDead(context, localTyper.typedByValueExpr(stat))

          if (treeInfo.isSelfOrSuperConstrCall(result)) {
            context.inConstructorSuffix = true
            if (treeInfo.isSelfConstrCall(result)) {
              val called = result.symbol
              val defined = exprOwner.enclMethod
              if (called == defined) ConstructorRecursesError(stat)
              else {
                val calledPos = called.pos.pointOrElse(0)
                val definedPos = defined.pos.pointOrElse(0)
                if (calledPos > definedPos) ConstructorsOrderError(stat)
                else if (calledPos == definedPos) {
                  // Trees generated by a macro have the same position
                  // Trees typechecked by a ToolBox have no position
                  val constructors = defined.owner.info.decl(nme.CONSTRUCTOR).alternatives
                  if (constructors.indexOf(called) > constructors.indexOf(defined)) ConstructorsOrderError(stat)
                }
              }
            }
          }
          result
      }

      /* From the spec (refchecks checks other conditions regarding erasing to the same type and default arguments):
       *
       * A block expression [... its] statement sequence may not contain two definitions or
       * declarations that bind the same name --> `inBlock`
       *
       * It is an error if a template directly defines two matching members.
       *
       * A member definition $M$ _matches_ a member definition $M'$, if $M$ and $M'$ bind the same name,
       * and one of following holds:
       *   1. Neither $M$ nor $M'$ is a method definition.
       *   2. $M$ and $M'$ define both monomorphic methods with equivalent argument types.
       *   3. $M$ defines a parameterless method and $M'$ defines a method with an empty parameter list `()` or _vice versa_.
       *   4. $M$ and $M'$ define both polymorphic methods with equal number of argument types $\overline T$, $\overline T'$
       *      and equal numbers of type parameters $\overline t$, $\overline t'$, say,
       *      and  $\overline T' = [\overline t'/\overline t]\overline T$.
       */
      def checkNoDoubleDefs(scope: Scope): Unit = {
        var e = scope.elems
        while ((e ne null) && e.owner == scope) {
          val sym = e.sym
          var e1 = scope.lookupNextEntry(e)
          while ((e1 ne null) && e1.owner == scope) {
            val sym1 = e1.sym

            def nullaryNilary: Boolean = {
              def nn(m: Symbol): Boolean = m.isParamAccessor || m.hasAccessorFlag || !m.isMethod || {
                m.tpe match {
                  case MethodType(Nil, _) | NullaryMethodType(_) => true
                  case _ => false
                }
              }
              nn(sym) && nn(sym1)
            }
            val conflicted = inBlock || (!sym.isMethod && !sym1.isMethod) || nullaryNilary || sym.tpe.matches(sym1.tpe)

            // default getters are defined twice when multiple overloads have defaults.
            // The error for this is deferred until RefChecks.checkDefaultsInOverloaded
            if (conflicted && !sym.isErroneous && !sym1.isErroneous && !sym.hasDefault) {
              log(sm"""Double definition detected:
                      |  ${(sym.getClass, sym.info, sym.ownerChain)}
                      |  ${(sym1.getClass, sym1.info, sym1.ownerChain)}""")

              DefDefinedTwiceError(sym, sym1)
              scope.unlink(e1) // need to unlink to avoid later problems with lub; see #scala/bug#2779
            }
            e1 = scope.lookupNextEntry(e1)
          }
          e = e.next
        }
      }

      def addSynthetics(stats: List[Tree], scope: Scope): List[Tree] = {
        var newStats = new ListBuffer[Tree]
        var moreToAdd = true
        while (moreToAdd) {
          val initElems = scope.elems
          // scala/bug#5877 The decls of a package include decls of the package object. But we don't want to add
          //         the corresponding synthetics to the package class, only to the package object class.
          // scala/bug#6734 Locality test below is meaningless if we're not even in the correct tree.
          //         For modules that are synthetic case companions, check that case class is defined here.
          // scala/bug#10783 ditto for synthetic companions of derived value classes.
          def shouldAdd(sym: Symbol): Boolean = {
            def classDefinedHere(s: Symbol): Boolean = stats exists {
              case t: ClassDef => t.symbol eq s
              case _           => false
            }
            def shouldAddAsModule: Boolean =
              classDefinedHere(companionSymbolOf(sym, context))

            (!sym.isModule || shouldAddAsModule) && (inBlock || !context.isInPackageObject(sym, context.owner))
          }
          for (sym <- scope)  context.unit.synthetics.get(sym) match {
            // OPT: shouldAdd is usually true. Call it here, rather than in the outer loop
            case Some(tree) if shouldAdd(sym) =>
              // if the completer set the IS_ERROR flag, retract the stat (currently only used by applyUnapplyMethodCompleter)
              if (!sym.initialize.hasFlag(IS_ERROR)) {
                newStats += typedStat(tree) // might add even more synthetics to the scope
                tree.getAndRemoveAttachment[CaseApplyInheritAccess.type].foreach(_ =>
                  runReporting.warning(tree.pos, "access modifiers for `apply` method are copied from the case class constructor under Scala 3 (or with -Xsource-features:case-apply-copy-access)", Scala3Migration, sym))
              }
              context.unit.synthetics -= sym
            case _ => ()
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
              mods.hasDefault && syntName.decodedName.startsWith(statName)

            // synt is companion module
            case (ClassDef(_, className, _, _), ModuleDef(_, moduleName, _)) =>
              className.toTermName == moduleName

            // synt is implicit def for implicit class (#6278)
            case (ClassDef(cmods, cname, _, _), DefDef(dmods, dname, _, _, _, _)) =>
              cmods.isImplicit && dmods.isImplicit && cname.toTermName == dname

            // ValDef and Accessor
            case (ValDef(_, cname, _, _), DefDef(_, dname, _, _, _, _)) =>
              cname.getterName == dname.getterName

            case _ => false
          }

          def matching(stat: Tree): List[Tree] = {
            val (pos, neg) = newStats.partition(synt => matches(stat, synt))
            newStats = neg
            pos.toList
          }

          // sorting residual stats for stability (scala/bug#10343, synthetics generated by other synthetics)
          (stats foldRight List[Tree]())((stat, res) => {
            stat :: matching(stat) ::: res
          }) ::: newStats.sortBy(_.symbol.name).toList
        }
      }

      val stats1 = stats mapConserve typedStat
      if (phase.erasedTypes) stats1
      else {
        val scope = if (inBlock) context.scope else context.owner.info.decls

        // As packages are open, it doesn't make sense to check double definitions here. Furthermore,
        // it is expensive if the package is large. Instead, such double definitions are checked in `Namers.enterInScope`
        if (!context.owner.isPackageClass)
          checkNoDoubleDefs(scope)

        // Note that Java units don't have synthetics, but there's no point in making a special case (for performance or correctness),
        // as we only type check Java units when running Scaladoc on Java sources.
        addSynthetics(stats1, scope)
      }
    }

    def typedArg(arg: Tree, mode: Mode, newmode: Mode, pt: Type): Tree = {
      val typedMode = mode.onlySticky | newmode
      val t = withCondConstrTyper(mode.inSccMode)(_.typed(arg, typedMode, pt))
      checkDead.inMode(context, typedMode, t)
    }

    def typedArgs(args: List[Tree], mode: Mode) =
      args mapConserve (arg => typedArg(arg, mode, NOmode, WildcardType))

    /** Does function need to be instantiated, because a missing parameter
     *  in an argument closure overlaps with an uninstantiated formal?
     */
    def needsInstantiation(tparams: List[Symbol], formals: List[Type], args: List[Tree]) = {
      def isLowerBounded(tparam: Symbol) = !tparam.info.lowerBound.typeSymbol.isBottomClass

      tparams.forall(isLowerBounded) && exists2(formals, args) {
        case (formal, Function(vparams, _)) if vparams.exists(_.tpt.isEmpty) =>
          val arity = vparams.length
          arity <= MaxFunctionArity && (formal.baseType(FunctionClass(arity)) match {
            case TypeRef(_, _, formalargs) =>
              exists2(formalargs, vparams) { (formal, vparam) =>
                vparam.tpt.isEmpty && tparams.exists(formal.contains)
              }
            case _ =>
              false
          })
        case _ =>
          false
      }
    }

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
              // No need for phasedAppliedType, as we don't get here during erasure --
              // overloading resolution happens during type checking.
              // During erasure, the condition above (fun.symbol.isOverloaded) is false.
              functionType(vparams map (_ => AnyTpe), shapeType(body))
            case Match(EmptyTree, _) => // A partial function literal
              appliedType(PartialFunctionClass, AnyTpe :: NothingTpe :: Nil)
            case NamedArg(Ident(name), rhs) =>
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
            // retracted synthetic apply in favor of user-defined apply
            def isRetracted(alt: Symbol) = alt.isError && alt.isSynthetic
            // loose arity check: based on args, prefer no tupling, assume no args: _*,
            // but keep alt with repeated params or default args, this is a loose fitting
            def isLooseFit(alt: Symbol)  =
              isApplicableBasedOnArity(pre memberType alt, argtypes.length, varargsStar = false, tuplingAllowed = false) || alt.tpe.params.exists(_.hasDefault)
            sym.filter(alt => !isRetracted(alt) && isLooseFit(alt)) match {
              case _: NoSymbol =>
              case sym1        => sym = sym1
            }
          }
          if (sym == NoSymbol) fun
          else adaptAfterOverloadResolution(fun setSymbol sym setType pre.memberType(sym), mode.forFunMode)
        } else fun
      }

      val fun = preSelectOverloaded(fun0)
      val argslen = args.length

      fun.tpe match {
        case OverloadedType(pre, alts) =>
          def handleOverloaded = {
            val undetparams = context.undetparams

            val argTpes: ListBuffer[Type] = ListBuffer.empty[Type]
            val args1: List[Tree] = context.savingUndeterminedTypeParams() {
              val amode = forArgMode(fun, mode)

              mapWithIndex(args) { (arg, argIdx) =>
                def typedArg0(tree: Tree, argIdxOrName: Either[Int, Name] = Left(argIdx)) = {
                  typedArg(tree, amode, BYVALmode, OverloadedArgProto(argIdxOrName, pre, alts)(undetparams))
                }

                arg match {
                  // scala/bug#8197/scala/bug#4592 call for checking whether this named argument could be interpreted as an assign
                  // infer.checkNames must not use UnitType: it may not be a valid assignment, or the setter may return another type from Unit
                  // TODO: just make it an error to refer to a non-existent named arg, as it's far more likely to be
                  //       a typo than an assignment passed as an argument
                  case NamedArg(lhs@Ident(name), rhs) =>
                    // named args: only type the righthand sides ("unknown identifier" errors otherwise)
                    // the assign is untyped; that's ok because we call doTypedApply
                    val rhsTyped = typedArg0(rhs, Right(name))
                    argTpes += NamedType(name, rhsTyped.tpe.deconst)
                    treeCopy.NamedArg(arg, lhs, rhsTyped)
                  case treeInfo.WildcardStarArg(_) =>
                    val argTyped = typedArg0(arg)
                    argTpes += RepeatedType(argTyped.tpe.deconst)
                    argTyped
                  case _ =>
                    val argTyped = typedArg0(arg)
                    argTpes += argTyped.tpe.deconst
                    argTyped
                }
              }
            }
            if (context.reporter.hasErrors)
              setError(tree)
            else {
              // warn about conversions applied to blocks (#9386) in lieu of fixing
              def checkConversionsToBlockArgs(appl: Tree): Unit =
                if (settings.warnByNameImplicit) {
                  val treeInfo.Applied(_, _, argss) = appl
                  val needsAdjust =
                    argss.find {
                      case (aiv: ApplyImplicitView) :: Nil =>
                        aiv.args match {
                          case Block(_ :: _, _) :: Nil => true
                          case _ => false
                        }
                      case _ => false
                    }
                  needsAdjust.foreach(ts => context.warning(ts.head.pos, "Overloaded implicit conversions that take a by-name parameter are applied to the entire block, not just the result expression.", WarningCategory.LintBynameImplicit))
                }
              inferMethodAlternative(fun, undetparams, argTpes.toList, pt)
              doTypedApply(tree, adaptAfterOverloadResolution(fun, mode.forFunMode, WildcardType), args1, mode, pt).tap(checkConversionsToBlockArgs)
            }
          }
          handleOverloaded

        case _ if currentRun.runDefinitions.isPolymorphicSignature(fun.symbol) =>
          // Mimic's Java's treatment of polymorphic signatures as described in
          // https://docs.oracle.com/javase/specs/jls/se11/html/jls-15.html#jls-15.12.3
          //
          // One can think of these methods as being infinitely overloaded. We create
          // a fictitious new cloned method symbol for each call site that takes on a signature
          // governed by a) the argument types and b) the expected type
          val args1 = typedArgs(args, forArgMode(fun, mode))
          val clone = fun.symbol.cloneSymbol.withoutAnnotations
          val cloneParams = args1.map(arg => clone.newValueParameter(freshTermName()).setInfo(arg.tpe.deconst))
          val resultType =
            if (fun.symbol.tpe.resultType.typeSymbol != ObjectClass) fun.symbol.tpe.resultType
            else if (isFullyDefined(pt)) pt
            else ObjectTpe
          clone.modifyInfo(mt => copyMethodType(mt, cloneParams, resultType))
          val fun1 = fun.setSymbol(clone).setType(clone.info)
          doTypedApply(tree, fun1, args1, mode, resultType).setType(resultType)

        case mt @ MethodType(params, _) =>
          val paramTypes = mt.paramTypes
          // repeat vararg as often as needed, remove by-name
          val formals = formalTypes(paramTypes, argslen)

          /* Try packing all arguments into a Tuple and apply `fun` to that.
           * This is the last thing which is tried (after default arguments).
           */
          def tryTupleApply: Tree =
            if (phase.erasedTypes || !eligibleForTupleConversion(paramTypes, argslen)) EmptyTree
            else {
              val tupleArgs = List(atPos(tree.pos.makeTransparent)(gen.mkTuple(args)))
              // expected one argument, but got 0 or >1 ==>  try applying to tuple
              // the inner "doTypedApply" does "extractUndetparams" => restore when it fails
              val savedUndetparams = context.undetparams
              // May warn or error if a Unit or tuple was inserted.
              def validate(t: Tree): Tree = {
                // regardless of typer's mode
                val invalidAdaptation = t.symbol != null && !checkValidAdaptation(t, args)
                // only bail if we're typing an expression (and not inside another application)
                if (invalidAdaptation && mode.typingExprNotFun) EmptyTree
                else t.removeAttachment[MultiargInfixAttachment.type]       // don't warn if we tupled
              }
              def reset(): Tree = {
                context.undetparams = savedUndetparams
                EmptyTree
              }
              silent(_.doTypedApply(tree, fun, tupleArgs, mode, pt)).fold(reset())(validate)
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
              tryTupleApply orElse duplErrorTree {
                val (_, argPos) = removeNames(Typer.this)(args, params)
                TooManyArgsNamesDefaultsError(tree, fun, formals, args, argPos)
              }
            } else if (lencmp == 0) {
              // we don't need defaults. names were used, so this application is transformed
              // into a block (@see transformNamedApplication in NamesDefaults)
              val (namelessArgs, argPos) = removeNames(Typer.this)(args, params)
              if (namelessArgs exists (_.isErroneous)) {
                duplErrTree
              } else if (!allArgsArePositional(argPos) && !sameLength(formals, params))
                // !allArgsArePositional indicates that named arguments are used to re-order arguments
                duplErrorTree(MultipleVarargError(tree))
              else if (allArgsArePositional(argPos) && NamedApplyBlock.unapply(fun).isEmpty) {
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

              // scala/bug#8111 transformNamedApplication eagerly shuffles around the application to preserve
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
              def rollbackNamesDefaultsOwnerChanges(): Unit = {
                symsOwnedByContextOwner foreach (_.owner = context.owner)
              }

              val fun1 = transformNamedApplication(Typer.this, mode, pt)(fun, x => x)
              if (fun1.isErroneous) duplErrTree
              else {
                val NamedApplyBlock(NamedApplyInfo(qual, targs, previousArgss, _, _)) = fun1: @unchecked
                val blockIsEmpty = fun1 match {
                  case Block(Nil, _) =>
                    // if the block does not have any ValDef we can remove it. Note that the call to
                    // "transformNamedApplication" is always needed in order to obtain targs/previousArgss
                    fun1.attachments.remove[NamedApplyInfo]
                    true
                  case _ => false
                }
                val (allArgs, missing) = addDefaults(args, qual, targs, previousArgss, params, fun.pos.focus, context)
                val funSym = fun1 match { case Block(_, expr) => expr.symbol case x => throw new MatchError(x) }
                val lencmp2 = compareLengths(allArgs, formals)

                if (!sameLength(allArgs, args) && callToCompanionConstr(context, funSym)) {
                  duplErrorTree(ModuleUsingCompanionClassDefaultArgsError(tree))
                } else if (lencmp2 > 0) {
                  removeNames(Typer.this)(allArgs, params) // #3818
                  duplErrTree
                } else if (lencmp2 == 0) {
                  // useful when a default doesn't match parameter type, e.g. def f[T](x:T="a"); f[Int]()
                  checkNotMacro()
                  context.set(ContextMode.DiagUsedDefaults)
                  def checkRecursive(res: Tree): Unit =
                    if (settings.warnRecurseWithDefault && !res.isErroneous && context.owner.hasTransOwner(funSym))
                      context.warning(res.pos, "Recursive call used default arguments instead of passing current argument values.", WarningCategory.LintRecurseWithDefault)

                  doTypedApply(tree, if (blockIsEmpty) fun else fun1, allArgs, mode, pt).tap(checkRecursive)
                } else {
                  rollbackNamesDefaultsOwnerChanges()
                  tryTupleApply orElse {
                    removeNames(Typer.this)(allArgs, params) // report bad names
                    duplErrorTree(NotEnoughArgsError(tree, fun, missing))
                  }
                }
              }
            }
          }

          if (!sameLength(formals, args) ||   // wrong nb of arguments
              (args exists isNamedArg) ||     // uses a named argument
              NamedApplyBlock.unapply(fun).isDefined) {       // fun was transformed to a named apply block =>
                                              // integrate this application into the block
            if (isApplyDynamicNamed(fun) && isDynamicRewrite(fun)) typedNamedApply(tree, fun, args, mode, pt)
            else tryNamesDefaults
          } else {
            val tparams = context.extractUndetparams()
            if (tparams.isEmpty) { // all type params are defined
              def handleMonomorphicCall: Tree = {
                // no expected type when jumping to a match label -- anything goes (this is ok since we're typing the translation of well-typed code)
                // ... except during erasure: we must take the expected type into account as it drives the insertion of casts!
                // I've exhausted all other semi-clean approaches I could think of in balancing GADT magic, scala/bug#6145, CPS type-driven transforms and other existential trickiness
                // (the right thing to do -- packing existential types -- runs into limitations in subtyping existential types,
                //  casting breaks scala/bug#6145,
                //  not casting breaks GADT typing as it requires sneaking ill-typed trees past typer)
                def noExpectedType = !phase.erasedTypes && fun.symbol.isLabel && treeInfo.isSynthCaseSymbol(fun.symbol)

                val args1 =
                  if (noExpectedType)
                    typedArgs(args, forArgMode(fun, mode))
                  else
                    typedArgsForFormals(args, paramTypes, forArgMode(fun, mode))

                // instantiate dependent method types, must preserve singleton types where possible (stableTypeFor) -- example use case:
                // val foo = "foo"; def precise(x: String)(y: x.type): x.type = {...}; val bar : foo.type = precise(foo)(foo)
                // precise(foo) : foo.type => foo.type
                val restpe = mt.resultType(mapList(args1)(arg => gen stableTypeFor arg orElse arg.tpe))
                def ifPatternSkipFormals(tp: Type) = tp match {
                  case MethodType(_, rtp) if mode.inPatternMode => rtp
                  case _ => tp
                }

                // Inline RHS of ValDef for right-associative by-value operator desugaring.
                // Remove the ValDef also if the argument is a constant-folded reference to it.
                var (args2, pos2) = (args1, tree.pos)
                args1 match {
                  case List(lit: Literal) =>
                    lit.attachments.get[OriginalTreeAttachment] match {
                      case Some(OriginalTreeAttachment(id: Ident)) if rightAssocValDefs.contains(id.symbol) =>
                        inlinedRightAssocValDefs += id.symbol
                        rightAssocValDefs.subtractOne(id.symbol)
                      case _ =>
                    }

                  case List(id: Ident) if rightAssocValDefs.contains(id.symbol) =>
                    mt.params match {
                      case List(p) if p.isByNameParam =>
                        inlinedRightAssocValDefs += id.symbol
                        val rhs = rightAssocValDefs.remove(id.symbol).get
                        args2 = rhs.changeOwner(id.symbol -> context.owner) :: Nil
                        pos2 = wrappingPos(tree :: rhs :: Nil)
                      case _ =>
                    }
                  case _ =>
                }

                if (!isPastTyper && args.isEmpty && canTranslateEmptyListToNil && currentRun.runDefinitions.isListApply(fun))
                  atPos(tree.pos)(gen.mkNil.setType(restpe))
                else {
                  // annoying issue with classOf that shouldn't be deconsted after typers (during fields phase)
                  val resTp = ifPatternSkipFormals(if (isPastTyper) restpe else restpe.deconst)
                  constfold(treeCopy.Apply(tree, fun, args2) setType resTp setPos pos2, context.owner)
                }
              }
              if (settings.warnDeadCode.value) {
                val sym = fun.symbol
                if (sym != null && sym.isMethod && !sym.isConstructor) {
                  val suppress = sym == Object_synchronized || (sym.isLabel && treeInfo.isSynthCaseSymbol(sym))
                  context.withSuppressDeadArgWarning(suppress) {
                    handleMonomorphicCall
                  }
                } else handleMonomorphicCall
              } else handleMonomorphicCall
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
                  debuglog("infer method inst " + fun + ", tparams = " + tparams + ", args = " + args1.map(_.tpe) + ", pt = " + pt + ", lobounds = " + tparams.map(_.tpe.lowerBound) + ", parambounds = " + tparams.map(_.info)) //debug
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

        // scala/bug#7877 `isTerm` needed to exclude `class T[A] { def unapply(..) }; ... case T[X] =>`
        case HasUnapply(_) if mode.inPatternMode && fun.isTerm =>
          doTypedUnapply(tree, fun0, fun, args, mode, pt)

        case _ =>
          if (treeInfo.isMacroApplication(tree)) duplErrorTree(MacroTooManyArgumentListsError(tree, fun.symbol))
          else duplErrorTree(ApplyWithoutArgsError(tree, fun))
      }
    }

    /**
     * Convert an annotation constructor call into an AnnotationInfo.
     */
    def typedAnnotation(ann: Tree, annotee: Option[Tree], mode: Mode = EXPRmode): AnnotationInfo = {
      var hasError: Boolean = false
      var unmappable: Boolean = false
      val pending = ListBuffer[AbsTypeError]()
      def ErroneousAnnotation = new ErroneousAnnotation().setOriginal(ann)

      def rangeFinder(): (Int, Int) =
        if (settings.Yrangepos.value && annotee.get.pos.isDefined) {
          val p = annotee.get.pos
          (p.start, p.end)
        } else {
          // compute approximate range
          var s = unit.source.length
          var e = 0
          object setRange extends ForeachTreeTraverser({ child =>
            val pos = child.pos
            if (pos.isDefined) {
              s = s min pos.start
              e = e max pos.end
            }
          }) {
            // in `@nowarn @ann(deprecatedMethod) def foo`, the deprecation warning should show
            override def traverseModifiers(mods: Modifiers): Unit = ()
          }
          setRange(annotee.get)
          (s, e max s)
        }
      def registerNowarn(info: AnnotationInfo): Unit = {
        if (annotee.isDefined && NowarnClass.exists && info.matches(NowarnClass) && !runReporting.suppressionExists(info.pos)) {
          val filters = (info.assocs: @unchecked) match {
            case Nil => List(MessageFilter.Any)
            case (_, LiteralAnnotArg(s)) :: Nil =>
              if (s.stringValue.isEmpty) Nil
              else {
                val (ms, fs) = s.stringValue.split('&').map(WConf.parseFilter(_, runReporting.rootDirPrefix)).toList.partitionMap(identity)
                if (ms.nonEmpty)
                  reporter.error(info.pos, s"Invalid message filter:\n${ms.mkString("\n")}")
                fs
              }
          }
          val (start, end) = rangeFinder()
          runReporting.addSuppression(Suppression(info.pos, filters, start, end))
        }
      }
      def registerDeprecationSuppression(info: AnnotationInfo): Unit =
        if (annotee.isDefined && info.matches(DeprecatedAttr) && !runReporting.suppressionExists(info.pos)) {
          val (start, end) = rangeFinder()
          runReporting.addSuppression(Suppression(info.pos, List(MessageFilter.Category(WarningCategory.Deprecation)), start, end, synthetic = true))
        }

      def finish(res: AnnotationInfo): AnnotationInfo = {
        if (hasError) {
          pending.foreach(ErrorUtils.issueTypeError)
          ErroneousAnnotation
        } else {
          registerNowarn(res)
          registerDeprecationSuppression(res)
          res
        }
      }

      def reportAnnotationError(err: AbsTypeError) = {
        pending += err
        hasError = true
        ErroneousAnnotation
      }

      // begin typedAnnotation
      val treeInfo.Applied(fun0, _, argss) = ann
      if (fun0.isErroneous) return finish(ErroneousAnnotation)
      val typedFun = context.withinAnnotation(typed(fun0, mode.forFunMode))
      if (typedFun.isErroneous) return finish(ErroneousAnnotation)

      val Select(New(annTpt), _) = typedFun: @unchecked
      val annType = annTpt.tpe // for a polymorphic annotation class, this type will have unbound type params (see context.undetparams)
      val annTypeSym = annType.typeSymbol
      val isJava = annTypeSym.isJavaDefined

      val isAnnotation = annTypeSym.isJavaAnnotation || annType <:< AnnotationClass.tpe
      if (!isAnnotation) {
        reportAnnotationError(DoesNotExtendAnnotation(typedFun, annTypeSym))
        return finish(ErroneousAnnotation)
      }
      if (currentRun.isScala3 && (/*annTypeSym.eq(SpecializedClass) ||*/ annTypeSym.eq(ElidableMethodClass)))
        context.warning(ann.pos, s"@${annTypeSym.fullNameString} is ignored in Scala 3", Scala3Migration)

      /* Calling constfold right here is necessary because some trees (negated
       * floats and literals in particular) are not yet folded.
       */
      def tryConst(tr: Tree, pt: Type): Option[LiteralAnnotArg] = {
        // The typed tree may be relevantly different than the tree `tr`,
        // e.g. it may have encountered an implicit conversion.
        val ttree = if (isJava) typed(constfold(tr, context.owner), pt) else tr
        val const: Constant = ttree match {
          case l @ Literal(c) if !l.isErroneous => c
          case tree => tree.tpe match {
            case ConstantType(c) => c
            case _               => null
          }
        }

        def isDefaultArg(tree: Tree) = tree match {
          case treeInfo.Applied(fun, _, _) => fun.symbol != null && fun.symbol.isDefaultGetter
          case _ => false
        }

        if (const == null) {
          if (unit.isJava) unmappable = true
          else if (!isDefaultArg(ttree)) reportAnnotationError(AnnotationNotAConstantError(ttree))
          None
        } else if (const.value == null) {
          reportAnnotationError(AnnotationArgNullError(tr)); None
        } else
          Some(LiteralAnnotArg(const))
      }

      /* Converts an untyped tree to a ClassfileAnnotArg. If the conversion fails,
       * an error message is reported and None is returned.
       */
      @tailrec
      def tree2ConstArg(tree: Tree, pt: Type): Option[ClassfileAnnotArg] = tree match {
        case Apply(Select(New(_), nme.CONSTRUCTOR), _) if pt.typeSymbol == ArrayClass && unit.isJava =>
          // In Java, a single value may be passed for array annotation parameters
          tree2ConstArg(Apply(Select(gen.mkAttributedRef(ArrayModule), nme.apply), List(tree)), pt)

        case Apply(Select(New(_), nme.CONSTRUCTOR), _) if pt.typeSymbol == ArrayClass =>
          reportAnnotationError(ArrayConstantsError(tree)); None

        case Apply(Select(New(tpt), nme.CONSTRUCTOR), _) if isJava =>
          val annInfo = typedAnnotation(tree, None, mode)
          val annType = annInfo.atp

          if (!annType.typeSymbol.isSubClass(pt.typeSymbol))
            reportAnnotationError(AnnotationTypeMismatchError(tpt, pt, annType))
          else if (!annType.typeSymbol.isJavaDefined)
            reportAnnotationError(NestedAnnotationError(tree, annType))

          if (annInfo.atp.isErroneous) { hasError = true; None }
          else Some(NestedAnnotArg(annInfo))

        // use of Array.apply[T: ClassTag](xs: T*): Array[T]
        // and    Array.apply(x: Int, xs: Int*): Array[Int]       (and similar)
        case treeInfo.Applied(fun, targs, args :: _) =>
          val typedFun = if (isJava) typed(fun, mode.forFunMode) else fun
          if (typedFun.symbol.owner == ArrayModule.moduleClass && typedFun.symbol.name == nme.apply)
            pt match {
              case _ if !isJava =>
                trees2ConstArg(args, targs.headOption.map(_.tpe).getOrElse(WildcardType))
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

        case tree if unit.isJava && pt.typeSymbol == ArrayClass =>
          /* If we get here, we have a Java array annotation argument which was passed
           * as a single value, and needs to be wrapped. */
          trees2ConstArg(tree :: Nil, pt.typeArgs.head)

        case tree =>
          tryConst(tree, pt)
      }
      def trees2ConstArg(trees: List[Tree], pt: Type): Option[ArrayAnnotArg] = {
        traverseOpt(trees)(tree2ConstArg(_, pt))
          .map(args => ArrayAnnotArg(args.toArray))
      }

      @inline def constantly = {
        // Arguments of Java annotations and ConstantAnnotations are checked to be constants and
        // stored in the `assocs` field of the resulting AnnotationInfo
        if (argss.lengthIs > 1) {
          reportAnnotationError(MultipleArgumentListForAnnotationError(ann))
        } else {
          val annScopeJava = annType.decls.filter(sym => sym.isMethod && !sym.isConstructor && sym.isJavaDefined)

          val names = mutable.Set[Symbol]()
          names ++= annScopeJava.iterator

          def hasValue = names exists (_.name == nme.value)
          val namedArgs = argss match {
            case List(List(arg)) if !isNamedArg(arg) && hasValue => gen.mkNamedArg(nme.value, arg) :: Nil
            case List(args)                                      => args
            case x                                               => throw new MatchError(x)
          }

          val nvPairs = namedArgs map {
            case arg @ NamedArg(Ident(name), rhs) =>
              val sym = annScopeJava.lookup(name)
              if (sym == NoSymbol) {
                reportAnnotationError(UnknownAnnotationNameError(arg, name))
                (nme.ERROR, None)
              } else if (!names.contains(sym)) {
                reportAnnotationError(DuplicateValueAnnotationError(arg, name))
                (nme.ERROR, None)
              } else {
                names -= sym
                sym.cookJavaRawInfo() // #3429
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
          else if (unmappable) UnmappableAnnotation
          else {
            if (annTypeSym == JavaDeprecatedAttr && !context.unit.isJava && settings.lintDeprecation)
              context.warning(ann.pos, """Prefer the Scala annotation over Java's `@Deprecated` to provide a message and version: @deprecated("message", since = "MyLib 1.0")""", WarningCategory.LintDeprecation)
            AnnotationInfo(annType, Nil, nvPairs.map(p => (p._1, p._2.get))).setOriginal(Apply(typedFun, namedArgs).setPos(ann.pos))
          }
        }
      }
      @inline def statically = {
        val typedAnn: Tree = {
          // local dummy fixes scala/bug#5544
          val localTyper = newTyper(context.make(ann, context.owner.newLocalDummy(ann.pos)))
          localTyper.typed(ann, mode)
        }
        @tailrec
        def annInfo(t: Tree): AnnotationInfo = t match {
          case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
            // `tpt.tpe` is more precise than `annType`, since it incorporates the types of `args`
            AnnotationInfo(tpt.tpe, args, Nil).setOriginal(typedAnn).setPos(t.pos)

          case Block(_, expr) =>
            if (!annTypeSym.isNonBottomSubClass(ConstantAnnotationClass))
              context.warning(t.pos, "Usage of named or default arguments transformed this annotation\n"+
                                "constructor call into a block. The corresponding AnnotationInfo\n"+
                                "will contain references to local values and default getters instead\n"+
                                "of the actual argument trees", WarningCategory.Other)
            annInfo(expr)

          case Apply(fun, args) =>
            context.warning(t.pos, "Implementation limitation: multiple argument lists on annotations are\n"+
                                   "currently not supported; ignoring arguments "+ args, WarningCategory.Other)
            annInfo(fun)

          case _ =>
            reportAnnotationError(UnexpectedTreeAnnotationError(t, typedAnn))
        }

        if ((typedAnn.tpe == null) || typedAnn.tpe.isErroneous) ErroneousAnnotation
        else {
          val info = annInfo(typedAnn)
          // check message and since args to deprecated
          // when deprecated gets more params, those other args may also be defaults.
          def usesDefault = typedAnn match {
            case Block(vals, _) => info.args.exists {
              case Ident(n) => vals.exists {
                case ValDef(_, `n`, _, rhs) => treeInfo.isDefaultGetter(rhs)
                case _                      => false
              }
              case _ =>
                false
            }
            case _ =>
              info.args.exists(treeInfo.isDefaultGetter)
          }
          if (annTypeSym == DeprecatedAttr && settings.lintDeprecation && argss.head.lengthIs < 2 && usesDefault)
            context.warning(ann.pos, """Specify both message and version: @deprecated("message", since = "MyLib 1.0")""", WarningCategory.LintDeprecation)
          info
        }
      }

      finish {
        if (isJava)
          constantly
        else {
          val info = statically
          if (!info.isErroneous && annTypeSym.isNonBottomSubClass(ConstantAnnotationClass)) {
            var namedArgs: Map[Name, Tree] = Map.empty
            val treeInfo.Applied(constr, _, _) = info.original match {
              case Block(stats, call) =>
                // when named / default args are used
                namedArgs = Map.from(stats collect {
                  case ValDef(_, name, _, rhs) => (name, rhs)
                })
                call
              case call => call
            }
            val params = constr.symbol.paramss.headOption.getOrElse(Nil)
            val assocs = info.args.zip(params) map {
              case (arg, param) =>
                val origArg = arg match {
                  case Ident(n) => namedArgs.getOrElse(n, arg)
                  case _ => arg
                }
                (param.name, tree2ConstArg(origArg, param.tpe.resultType))
            }
            if (hasError) ErroneousAnnotation
            else if (unmappable) UnmappableAnnotation
            else AnnotationInfo(info.atp, Nil, assocs.collect { case (n, Some(arg)) => (n, arg) }).setOriginal(info.original).setPos(info.pos)
          } else
            info
        }
      }
    }

    def typedMacroAnnotation(cdef: ClassDef) = {
      val clazz = cdef.symbol
      if (!isPastTyper) {
        if (clazz != null && (clazz isNonBottomSubClass AnnotationClass)) {
          val macroTransform = clazz.info.member(nme.macroTransform)
          if (macroTransform != NoSymbol) {
            clazz.setFlag(MACRO)
            if (clazz.getAnnotation(CompileTimeOnlyAttr).isEmpty) clazz.addAnnotation(AnnotationInfo(CompileTimeOnlyAttr.tpe, List(Literal(Constant(MacroAnnotationNotExpandedMessage)) setType StringClass.tpe), Nil))
            def flavorOk = macroTransform.isMacro
            def paramssOk = mmap(macroTransform.paramss)(p => (p.name, p.info)) == List(List((nme.annottees, scalaRepeatedType(AnyTpe))))
            def tparamsOk = macroTransform.typeParams.isEmpty
            def everythingOk = flavorOk && paramssOk && tparamsOk
            if (!everythingOk) MacroAnnotationShapeError(clazz)
            if (!(clazz isNonBottomSubClass StaticAnnotationClass)) MacroAnnotationMustBeStaticError(clazz)
            // TODO: revisit the decision about @Inherited
            if (clazz.hasAnnotation(InheritedAttr)) MacroAnnotationCannotBeInheritedError(clazz)
            if (!clazz.isStatic) MacroAnnotationCannotBeMemberError(clazz)
          }
        }
      }
      cdef
    }

    /** Compute an existential type from raw hidden symbols `syms` and type `tp`
     */
    def packSymbols(hidden: List[Symbol], tp: Type): Type = global.packSymbols(hidden, tp, context.owner)

    def isReferencedFrom(ctx: Context, sym: Symbol): Boolean =
       ctx.owner.isTerm && ctx.scope.exists(dcl => dcl.isInitialized && dcl.info.contains(sym)) || {
          var ctx1 = ctx.outer
          while ((ctx1 != NoContext) && (ctx1.scope eq ctx.scope))
            ctx1 = ctx1.outer

          (ctx1 != NoContext) && isReferencedFrom(ctx1, sym)
       }

    def isCapturedExistential(sym: Symbol) =
      (sym hasAllFlags EXISTENTIAL | CAPTURED) && {
        val start = if (settings.areStatisticsEnabled) statistics.startTimer(isReferencedNanos) else null
        try !isReferencedFrom(context, sym)
        finally if (settings.areStatisticsEnabled) statistics.stopTimer(isReferencedNanos, start)
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
      val localSyms = mutable.LinkedHashSet[Symbol]()
      val boundSyms = mutable.LinkedHashSet[Symbol]()
      def isLocal(sym: Symbol): Boolean =
        if (sym == NoSymbol || sym.isRefinementClass || sym.isLocalDummy) false
        else if (owner == NoSymbol) tree exists (defines(_, sym))
        else containsDef(owner, sym) || isRawParameter(sym) || isCapturedExistential(sym)
      def containsLocal(tp: Type): Boolean =
        tp exists (t => isLocal(t.typeSymbol) || isLocal(t.termSymbol))

      val dealiasLocals = new TypeMap {
        @tailrec
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, _) =>
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
      def addLocals(tp: Type): Unit = {
        val remainingSyms = new ListBuffer[Symbol]
        def addIfLocal(sym: Symbol, tp: Type): Unit = {
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
      if (!checkClassOrModuleType(tpt) && noGen) tpt
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
          case _ =>
            debuglog(s"cannot reconstruct the original for $tree, because $tpt1 is not a TypeTree")
            tree
        }
        TypeTree(newExistentialType(tparams, tp)) setOriginal original
      }
      )
    }

    // lifted out of typed1 because it's needed in typedImplicit0
    protected def typedTypeApply(tree: Tree, mode: Mode, fun: Tree, args: List[Tree]): Tree = fun.tpe match {
      case OverloadedType(_, _) =>
        inferPolyAlternatives(fun, mapList(args)(_.tpe))

        // scala/bug#8267 `memberType` can introduce existentials *around* a PolyType/MethodType, see AsSeenFromMap#captureThis.
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
          val isJava = context.unit.isJava
          val isClassOf = fun.symbol.rawname == nme.classOf && currentRun.runDefinitions.isPredefClassOf(fun.symbol)
          if (isJava && fun.symbol.isTerm) args.foreach(_.modifyType(rawToExistential)) // e.g. List.class, parsed as classOf[List]
          val targs = mapList(args)(_.tpe)
          checkBounds(tree, NoPrefix, NoSymbol, tparams, targs, "")
          if (isClassOf)
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

    //
    // START: applyDynamic support
    //
    import treeInfo.{isApplyDynamicName, DynamicUpdate, DynamicApplicationNamed}

    private def acceptsApplyDynamic(tp: Type) = tp.typeSymbol isNonBottomSubClass DynamicClass

    /** Returns `true` if `name` can be selected dynamically on `qual`, `false` if not. */
    private def acceptsApplyDynamicWithType(qual: Tree, name: Name): Boolean =
      // don't selectDynamic selectDynamic, do select dynamic at unknown type
      !isApplyDynamicName(name) && acceptsApplyDynamic(qual.tpe.widen)

    // if the qualifier is a Dynamic, that's all we need to know
    private def isDynamicallyUpdatable(tree: Tree) = tree match {
      case DynamicUpdate(qual, _) => acceptsApplyDynamic(qual.tpe)
      case _                      => false
    }

    private def isApplyDynamicNamed(fun: Tree): Boolean = fun match {
      case DynamicApplicationNamed(qual, _) => acceptsApplyDynamic(qual.tpe.widen)
      case _ => false
        // look deeper?
        // val treeInfo.Applied(methPart, _, _) = fun
        // println("methPart of "+ fun +" is "+ methPart)
        // if (methPart ne fun) isApplyDynamicNamed(methPart)
        // else false
    }

    private def typedNamedApply(orig: Tree, fun: Tree, args: List[Tree], mode: Mode, pt: Type): Tree = {
      def argToBinding(arg: Tree): Tree = arg match {
        case NamedArg(i @ Ident(name), rhs) =>
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
    private def mkInvoke(context: Context, tree: Tree, qual: Tree, name: Name): Option[Tree] = {
      val cxTree = context.enclosingNonImportContext.tree // scala/bug#8364
      debuglog(s"dyna.mkInvoke($cxTree, $tree, $qual, $name)")
      val treeInfo.Applied(treeSelection, _, _) = tree
      def isDesugaredApply = {
        val protoQual = macroExpandee(qual) orElse qual
        treeSelection match {
          case Select(`protoQual`, nme.apply) => true
          case _                              => false
        }
      }

      Option.when(acceptsApplyDynamicWithType(qual, name)) {
        def matches(t: Tree) = isDesugaredApply || treeInfo.dissectCore(t) == treeSelection

        /* Note that the trees which arrive here are potentially some distance from
         * the trees of direct interest. `cxTree` is some enclosing expression which
         * may apparently be arbitrarily larger than `tree`; and `tree` itself is
         * too small, having at least in some cases lost its explicit type parameters.
         * This logic is designed to use `tree` to pinpoint the immediately surrounding
         * Apply/TypeApply/Select node, and only then creates the dynamic call.
         * See scala/bug#6731 among others.
         */
        def findSelection(t: Tree): Option[(TermName, Tree)] = t match {
          case Apply(fn, args) if matches(fn) =>
            val op = if(args.exists(_.isInstanceOf[NamedArg])) nme.applyDynamicNamed else nme.applyDynamic
            // not supported: foo.bar(a1,..., an: _*)
            val fn1 = if(treeInfo.isWildcardStarArgList(args)) DynamicVarArgUnsupported(fn, op) else fn
            Some((op, fn1))
          case Assign(lhs, _) if matches(lhs) => Some((nme.updateDynamic, lhs))
          case _ if matches(t)                => Some((nme.selectDynamic, t))
          case _                              => t.children.flatMap(findSelection).headOption
        }
        findSelection(cxTree).map { case (opName, treeInfo.Applied(_, targs, _)) =>
          val fun = atPos(wrappingPos(qual :: targs)) {
            gen.mkTypeApply(Select(qual, opName) setPos qual.pos, targs)
          }
          if (opName == nme.updateDynamic) suppressMacroExpansion(fun) // scala/bug#7617
          val nameStringLit = {
            val p = if (treeSelection.pos.isDefined) treeSelection.pos.withStart(treeSelection.pos.point).makeTransparent else treeSelection.pos
            atPos(p) {
              Literal(Constant(name.decode))
            }
          }
          markDynamicRewrite {
            atPos(wrappingPos(qual :: fun :: nameStringLit :: Nil)) {
              Apply(fun, List(nameStringLit))
            }
          }
        }.getOrElse {
          // While there may be an error in the found tree itself, it should not be possible to *not find* it at all.
          devWarning(s"Tree $tree not found in the context $cxTree while trying to do a dynamic application")
          setError(tree)
        }
      }
    }
    private def wrapErrors(tree: Tree, typeTree: Typer => Tree): Tree = silent(typeTree) orElse (err => DynamicRewriteError(tree, err.head))
    //
    // END: applyDynamic support
    //

    def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      // Lookup in the given class using the root mirror.
      def lookupInOwner(owner: Symbol, name: Name): Symbol =
        if (mode.inQualMode) rootMirror.missingHook(owner, name) else NoSymbol

      // Lookup in the given qualifier.  Used in last-ditch efforts by typedIdent and typedSelect.
      def lookupInRoot(name: Name): Symbol  = lookupInOwner(rootMirror.RootClass, name)
      def lookupInEmpty(name: Name): Symbol = rootMirror.EmptyPackageClass.info member name

      def lookupInQualifier(qual: Tree, name: Name): Symbol =
        if (name == nme.ERROR || qual.tpe.widen.isErroneous)
          NoSymbol
        else lookupInOwner(qual.tpe.typeSymbol, name) orElse {
          NotAMemberError(tree, qual, name, startingIdentContext)
          NoSymbol
        }

      def startingIdentContext =
        // ignore current variable scope in patterns to enforce linearity
        if (mode.inNone(PATTERNmode | TYPEPATmode)) context
        else context.outer

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
            val ainfo = typedAnnotation(ann, Some(atd), annotMode)
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
            val annotInfo = typedAnnotation(ann, Some(atd), annotMode)
            ann setType arg1.tpe.withAnnotation(annotInfo)
          }
          val atype = ann.tpe
          // For `f(): @inline/noinline` callsites, add the InlineAnnotatedAttachment. TypeApplys
          // are eliminated by erasure, so add it to the underlying function in this case.
          @tailrec
          def setInlineAttachment(t: Tree, att: InlineAnnotatedAttachment): Unit = t match {
            case TypeApply(fun, _) => setInlineAttachment(fun, att)
            case _ => t.updateAttachment(att)
          }
          if (atype.hasAnnotation(definitions.ScalaNoInlineClass)) setInlineAttachment(arg1, NoInlineCallsiteAttachment)
          else if (atype.hasAnnotation(definitions.ScalaInlineClass)) setInlineAttachment(arg1, InlineCallsiteAttachment)
          Typed(arg1, resultingTypeTree(atype)) setPos tree.pos setType atype
        }
      }

      def typedBind(tree: Bind) =
        tree match {
          case Bind(name: TypeName, body)  =>
            assert(body.isEmpty, s"${context.unit} typedBind: ${name.debugString} ${body} ${body.getClass}")
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

          case Bind(name: TermName, body)  =>
            val sym =
              if (tree.symbol != NoSymbol) tree.symbol
              else context.owner.newValue(name, tree.pos)

            if (name != nme.WILDCARD) {
              if (context.inPatAlternative)
                VariableInPatternAlternativeError(tree)

              namer.enterInScope(sym)
            }

            val body1 = typed(body, mode, pt)
            val impliedType = patmat.binderTypeImpliedByPattern(body1, pt) // scala/bug#1503, scala/bug#5204
            val symTp =
              if (treeInfo.isSequenceValued(body)) seqType(impliedType)
              else impliedType
            sym setInfo symTp

            // have to imperatively set the symbol for this bind to keep it in sync with the symbols used in the body of a case
            // when type checking a case we imperatively update the symbols in the body of the case
            // those symbols are bound by the symbols in the Binds in the pattern of the case,
            // so, if we set the symbols in the case body, but not in the patterns,
            // then re-type check the casedef (for a second try in typedApply for example -- scala/bug#1832),
            // we are no longer in sync: the body has symbols set that do not appear in the patterns
            // since body1 is not necessarily equal to body, we must return a copied tree,
            // but we must still mutate the original bind
            tree setSymbol sym
            treeCopy.Bind(tree, name, body1) setSymbol sym setType impliedType
        }

      def typedArrayValue(tree: ArrayValue) = {
        val elemtpt1 = typedType(tree.elemtpt, mode)
        val elems1   = tree.elems mapConserve (elem => typed(elem, mode, elemtpt1.tpe))
        // see run/t6126 for an example where `pt` does not suffice (tagged types)
        val tpe1     = if (isFullyDefined(pt) && !phase.erasedTypes) pt else arrayType(elemtpt1.tpe)

        treeCopy.ArrayValue(tree, elemtpt1, elems1) setType tpe1
      }

      def typedAssign(lhs: Tree, rhs: Tree): Tree = {
        // see scala/bug#7617 for an explanation of why macro expansion is suppressed
        def typedLhs(lhs: Tree) = typed(lhs, EXPRmode | LHSmode | POLYmode)
        val lhs1    = unsuppressMacroExpansion(typedLhs(suppressMacroExpansion(lhs)))
        val varsym  = lhs1.symbol

        // see #2494 for double error message example
        def fail() =
          if (lhs1.isErrorTyped) lhs1
          else AssignmentError(tree, varsym)

        if (varsym == null)
          return fail()

        def shadowsSetter =
          lhs1 match {
            case treeInfo.Applied(Select(qual, _), _, _) if qual.isTyped =>
              qual.tpe.member(varsym.name.setterName).exists
            case _ => false
          }

        def setterRewrite =
          lhs1 match {
            case treeInfo.Applied(Select(qual, _), _, _) =>
              val sel = Select(qual, varsym.name.setterName) setPos lhs.pos
              val app = Apply(sel, List(rhs)) setPos tree.pos
              typed(app, mode, pt)
            case _ => EmptyTree
          }

        val rewritten =
          if (treeInfo.mayBeVarGetter(varsym)) setterRewrite
          else EmptyTree

        if (!rewritten.isEmpty) rewritten
        else if (varsym.isVariable || varsym.isValue && phase.assignsFields) {
          val rhs1 = typedByValueExpr(rhs, lhs1.tpe)
          treeCopy.Assign(tree, lhs1, checkDead(context, rhs1)) setType UnitTpe
        }
        else if (isDynamicallyUpdatable(lhs1)) {
          val t = atPos(lhs1.pos.withEnd(rhs.pos.end)) {
            Apply(lhs1, List(rhs))
          }
          wrapErrors(t, _.typed1(t, mode, pt))
        }
        else if (shadowsSetter) setterRewrite orElse fail()
        else fail()
      }

      def typedIf(tree: If): If = {
        val cond1 = checkDead(context, typedByValueExpr(tree.cond, BooleanTpe))
        // Unibranch if normally has unit value else, but synthetic code may emit empty else.
        if (tree.elsep.isEmpty)
          return treeCopy.If(tree, cond1, typed(tree.thenp, UnitTpe), tree.elsep) setType UnitTpe

        val thenp1 = typed(tree.thenp, pt)
        val elsep1 = typed(tree.elsep, pt)

        // in principle we should pack the types of each branch before lubbing, but lub doesn't really work for existentials anyway
        // in the special (though common) case where the types are equal, it pays to pack before comparing
        // especially virtpatmat needed more aggressive unification of skolemized types
        // this breaks src/library/scala/collection/immutable/TrieIterator.scala (which as of 2.13 doesn't actually exist anymore)
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
        // Important to deconst, otherwise `if (???) 0 else 0` evaluates to 0 (scala/bug#6331)
        else thenp1.tpe.deconst :: elsep1.tpe.deconst :: Nil match {
          case tp :: _ if samePackedTypes     => finish(tp)
          case tpes if sameWeakLubAsLub(tpes) => finish(lub(tpes))
          case tpes                           =>
            val lub = weakLub(tpes)
            treeCopy.If(tree, cond1, adapt(thenp1, mode, lub), adapt(elsep1, mode, lub)) setType lub
        }
      }

      // Type the Match and leave it until phase `patmat`
      // empty-selector matches are transformed into synthetic PartialFunction implementations when the expected type demands it
      def typedVirtualizedMatch(tree: Match): Tree = {
        val selector = tree.selector
        val cases = tree.cases
        if (selector == EmptyTree) {
          if (pt.typeSymbol == PartialFunctionClass)
            synthesizePartialFunction(newTermName(fresh.newName("x")), tree.pos, paramSynthetic = true, tree, mode, pt)
          else {
            val arity = functionArityFromType(pt) match { case -1 => 1 case arity => arity } // scala/bug#8429: consider sam and function type equally in determining function arity

            val params = for (i <- List.range(0, arity)) yield
              atPos(tree.pos.focusStart) {
                ValDef(Modifiers(PARAM | SYNTHETIC),
                       freshTermName("x" + i + "$"), TypeTree(), EmptyTree)
              }
            val ids = for (p <- params) yield Ident(p.name)
            val selector1 = atPos(tree.pos.focusStart) { if (arity == 1) ids.head else gen.mkTuple(ids) }
            // scala/bug#8120 If we don't duplicate the cases, the original Match node will share trees with ones that
            //         receive symbols owned by this function. However if, after a silent mode session, we discard
            //         this Function and try a different approach (e.g. applying a view to the receiver) we end up
            //         with orphaned symbols which blows up far down the pipeline (or can be detected with -Ycheck:typer).
            val body = treeCopy.Match(tree, selector1, (cases map duplicateAndKeepPositions).asInstanceOf[List[CaseDef]])
            typed1(atPos(tree.pos) { Function(params, body) }, mode, pt)
          }
        } else
          typedMatch(selector, cases, mode, pt, tree)
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
          val DefDef(_, name, _, _, restpt, _) = enclMethod.tree: @unchecked
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
              val typedExpr = typed(expr)
              if (!isPastTyper && typedExpr.tpe.typeSymbol != UnitClass)
                context.warning(tree.pos, "enclosing method " + name + s" has result type Unit: return value of type ${typedExpr.tpe} discarded", WarningCategory.Other)
            }
            val res = treeCopy.Return(tree, checkDead(context, expr1)).setSymbol(enclMethod.owner)
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

          if (checkStablePrefixClassType(tpt0)) {
            tpt0.tpe.normalize match { // eta-expand
              case PolyType(undet, appliedToUndet) =>
                context.undetparams = undet // can reuse these type params, they're fresh
                notifyUndetparamsAdded(undet)
                TypeTree().setOriginal(tpt0).setType(appliedToUndet)
              case _                               => tpt0
            }
          }
          else tpt0
        }

        /* If current tree <tree> appears in <val x(: T)? = <tree>>
         * return `tp with x.type' else return `tp`.
         */
        def narrowRhs(tp: Type) = { val sym = context.tree.symbol
          context.tree match {
            case ValDef(mods, _, _, Apply(Select(`tree`, _), _)) if !mods.isMutable && sym != null && sym != NoSymbol =>
              val sym1 =
                if (sym.owner.isClass && sym.getterIn(sym.owner) != NoSymbol) sym.getterIn(sym.owner)
                else sym
              val pre = if (sym1.owner.isClass) sym1.owner.thisType else NoPrefix
              intersectionType(List(tp, singleType(pre, sym1)))
            case _ => tp
          }}

        val tp = tpt1.tpe
        val sym = tp.typeSymbol.initialize

        if ((sym.isAbstractType || sym.hasAbstractFlag)
          && !(sym.isJavaAnnotation && context.inAnnotation))
          IsAbstractError(tree, sym)
        else if (isPrimitiveValueClass(sym)) {
          NotAMemberError(tpt, TypeTree(tp), nme.CONSTRUCTOR, startingIdentContext)
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


      /* Eta expand an expression like `m _`, where `m` denotes a method or a by-name argument
       *
       * The spec says:
       * The expression `$e$ _` is well-formed if $e$ is of method type or if $e$ is a call-by-name parameter.
       *   (1) If $e$ is a method with parameters, `$e$ _` represents $e$ converted to a function type
       *       by [eta expansion](#eta-expansion).
       *   (2) If $e$ is a parameterless method or call-by-name parameter of type `=> $T$`, `$e$ _` represents
       *       the function of type `() => $T$`, which evaluates $e$ when it is applied to the empty parameter list `()`.
       */
      def typedEta(methodValue: Tree): Tree = methodValue.tpe match {
        case tp@(MethodType(_, _) | PolyType(_, MethodType(_, _))) => // (1)
          if (tp.params.lengthCompare(definitions.MaxFunctionArity) > 0) MaxFunctionArityError(methodValue, s"; method ${methodValue.symbol.name} cannot be eta-expanded because it takes ${tp.params.length} arguments")
          else {
            val etaPt =
              pt match {
                case pt: ProtoType =>
                  pt.asFunctionType orElse functionType(WildcardType.fillList(tp.params.length), WildcardType) orElse WildcardType // arity overflow --> NoType
                case _             => pt
              }

            // We know syntactically methodValue can't refer to a constructor because you can't write `this _` for that (right???)
            typedEtaExpansion(methodValue, mode, etaPt)
          }

        case TypeRef(_, ByNameParamClass, _) |  NullaryMethodType(_) => // (2)
          val pos = methodValue.pos
          // must create it here to change owner (normally done by typed's typedFunction)
          val funSym = context.owner.newAnonymousFunctionValue(pos)
          new ChangeOwnerTraverser(context.owner, funSym) traverse methodValue

          val result = typed(Function(Nil, methodValue) setSymbol funSym setPos pos, mode, pt)

          val msg = "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, " +
            "write a function literal `() => m` instead"

          val action = {
            val etaPos = pos.withEnd(pos.end + 2)
            if (pos.source.sourceAt(etaPos).endsWith(" _"))
              runReporting.codeAction("replace by function literal", etaPos, s"() => ${pos.source.sourceAt(pos)}", msg)
            else Nil
          }

          if (currentRun.isScala3)
            context.warning(pos, msg, Scala3Migration, action)
          else
            context.deprecationWarning(pos, NoSymbol, msg, "2.13.2", action)

          result

        case ErrorType =>
          methodValue

        case _ =>
          UnderscoreEtaError(methodValue)
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
          case _: TypeError =>
            // @H some of typer errors can still leak,
            // for instance in continuations
            None
        }
      }

      /* Try to apply function to arguments; if it does not work, try to convert Java raw to existentials, or try to
       * insert an implicit conversion.
       */
      def tryTypedApply(fun: Tree, args: List[Tree]): Tree = {
        val start = if (settings.areStatisticsEnabled) statistics.startTimer(failedApplyNanos) else null

        def onError(typeErrors: Seq[AbsTypeError], warnings: Seq[ContextWarning]): Tree = {
          if (settings.areStatisticsEnabled) statistics.stopTimer(failedApplyNanos, start)

          // If the problem is with raw types, convert to existentials and try again.
          // See #4712 for a case where this situation arises,
          if ((fun.symbol ne null) && fun.symbol.isJavaDefined) {
            val newtpe = rawToExistential(fun.tpe)
            if (fun.tpe ne newtpe) {
              // println("late cooking: "+fun+":"+fun.tpe) // DEBUG
              return tryTypedApply(fun setType newtpe, args)
            }
          }
          // TODO: case to recurse into Function?
          /* Only retry if the error hails from a result expression of `tree`
           * (for instance, it makes no sense to retry on an error from a block statement)
           * compare with `samePointAs` since many synthetic trees are made with
           * offset positions even under -Yrangepos.
           */
          def errorInResult(tree: Tree): Boolean = {
            def pred(tree: Tree) = typeErrors.exists(_.errPos samePointAs tree.pos)
            def loop(tree: Tree): Boolean = pred(tree) || (tree match {
              case Block(_, r)          => loop(r)
              case Match(_, cases)      => cases.exists(pred)
              case CaseDef(_, _, r)     => loop(r)
              case Annotated(_, r)      => loop(r)
              case If(_, t, e)          => loop(t) || loop(e)
              case Try(b, catches, _)   => loop(b) || catches.exists(pred)
              case MethodValue(r)       => loop(r)
              case Select(qual, _)      => loop(qual)
              case Apply(fun, args)     => loop(fun) || args.exists(loop)
              case TypeApply(fun, args) => loop(fun) || args.exists(loop)
              case _                    => false
            })
            loop(tree)
          }
          val retry = typeErrors.forall(_.errPos != null) && (errorInResult(fun) || errorInResult(tree) || args.exists(errorInResult))
          typingStack.printTyping {
            val funStr = s"${ptTree(fun)} and ${args.map(ptTree).mkString(", ")}"
            if (retry) s"second try: $funStr"
            else s"no second try: $funStr because error not in result: ${typeErrors.head.errPos}!=${tree.pos}"
          }
          if (retry) {
            val Select(qual, name) = fun: @unchecked
            tryTypedArgs(args, forArgMode(fun, mode)) match {
              case Some(args1) if !args1.exists(arg => arg.exists(_.isErroneous)) =>
                val qual1 =
                  if (!pt.isError) adaptToArguments(qual, name, args1, pt)
                  else qual
                if (qual1 ne qual) {
                  val tree1 = Apply(Select(qual1, name) setPos fun.pos, args1) setPos tree.pos
                  return context withinSecondTry typed1(tree1, mode, pt)
                }
              case _ => ()
            }
          }
          def adjust(err: AbsTypeError) =
            if (tree.hasAttachment[InterpolatedString.type])
              tree match {
                case Apply(sc @ Ident(nme.StringContextName), _) =>
                  if (sc.isErroneous) err
                  else NormalTypeError(tree, s"${err.errMsg}; signature for interpolation must be `StringContext.apply(String*)`")
                case Apply(Select(_, nm), badargs) =>
                  if (badargs.exists(arg => arg.isErroneous || arg.pos.includes(err.errPos) && arg.pos != err.errPos)) err
                  else NormalTypeError(tree, s"${err.errMsg}; incompatible interpolation method $nm")
                case x => throw new MatchError(x)
              }
            else err
          typeErrors.foreach(err => context.issue(adjust(err)))
          warnings.foreach { case ContextWarning(p, m, c, s, as) => context.warning(p, m, c, s, as) }
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
        val appStart = if (settings.areStatisticsEnabled) statistics.startTimer(failedApplyNanos) else null
        val opeqStart = if (settings.areStatisticsEnabled) statistics.startTimer(failedOpEqNanos) else null

        def isConversionCandidate(qual: Tree, name: Name): Boolean =
          !mode.inPatternMode && nme.isOpAssignmentName(TermName(name.decode)) && !qual.exists(_.isErroneous)

        def reportError(error: SilentTypeError): Tree = {
          error.reportableErrors.foreach(context.issue)
          error.warnings.foreach { case ContextWarning(p, m, c, s, as) => context.warning(p, m, c, s, as) }
          args.foreach(typed(_, mode, ErrorType))
          setError(tree)
        }
        def advice1(convo: Tree, errors: List[AbsTypeError], err: SilentTypeError): List[AbsTypeError] =
          errors.map { e =>
            if (e.errPos samePointAs tree.pos) {
              val header = f"${e.errMsg}%n  Expression does not convert to assignment because:%n    "
              val expansion = f"%n    expansion: ${show(convo)}"
              NormalTypeError(tree, err.errors.flatMap(_.errMsg.linesIterator.toList).mkString(header, f"%n    ", expansion))
            } else e
          }
        def advice2(errors: List[AbsTypeError]): List[AbsTypeError] =
          errors.map { e =>
            if (e.errPos samePointAs tree.pos) {
              val msg = f"${e.errMsg}%n  Expression does not convert to assignment because receiver is not assignable."
              NormalTypeError(tree, msg)
            } else e
          }
        def onError(error: SilentTypeError): Tree = fun match {
          case Select(qual, name) if isConversionCandidate(qual, name) =>
            val qual1 = typedQualifier(qual)
            if (treeInfo.isVariableOrGetter(qual1)) {
              if (settings.areStatisticsEnabled) statistics.stopTimer(failedOpEqNanos, opeqStart)
              val erred = qual1.exists(_.isErroneous) || args.exists(_.isErroneous)
              if (erred) reportError(error) else {
                val convo = convertToAssignment(fun, qual1, name, args)
                if (settings.multiargInfix && tree.hasAttachment[MultiargInfixAttachment.type] && args.lengthCompare(1) > 0)
                  warnMultiargInfix(tree)
                silent(op = _.typed1(convo, mode, pt)) match {
                  case SilentResultValue(t) => t
                  case err: SilentTypeError => reportError(
                    SilentTypeError(advice1(convo, error.errors, err), error.warnings)
                  )
                }
              }
            } else {
              if (settings.areStatisticsEnabled) statistics.stopTimer(failedApplyNanos, appStart)
              val Apply(Select(qual2, _), args2) = tree: @unchecked
              val erred = qual2.exists(_.isErroneous) || args2.exists(_.isErroneous)
              reportError {
                if (erred) error else SilentTypeError(advice2(error.errors), error.warnings)
              }
            }
          case _ =>
            if (settings.areStatisticsEnabled) statistics.stopTimer(failedApplyNanos, appStart)
            reportError(error)
        }
        val silentResult = silent(
          op                    = _.typed(fun, mode.forFunMode, funpt),
          reportAmbiguousErrors = !mode.inExprMode && context.ambiguousErrors,
          newtree               = if (mode.inExprMode) tree else context.tree
        )
        silentResult match {
          case SilentResultValue(fun1) =>
            val fun2 = if (stableApplication) stabilizeFun(fun1, mode, pt) else fun1
            if (settings.areStatisticsEnabled) statistics.incCounter(typedApplyCount)
            val isFirstTry = fun2 match {
              case Select(_, _) => mode.inExprMode && {
                val noSecondTry = (
                     isPastTyper
                  || context.inSecondTry
                  || (fun2.symbol ne null) && fun2.symbol.isConstructor
                  || isImplicitMethodType(fun2.tpe)
                )
                !noSecondTry
              }
              case _ => false
            }
            if (isFirstTry)
              tryTypedApply(fun2, args)
            else
              doTypedApply(tree, fun2, args, mode, pt)
          case err: SilentTypeError => onError(err)
        }
      }

      def typedApply(tree: Apply) = tree match {
        case Apply(blk @ Block(stats, expr), args) =>
          val ap1 = treeCopy.Apply(tree, expr, args).clearType().setPos(tree.pos.makeTransparent)
          val blk1 = treeCopy.Block(blk, stats, ap1).clearType()
          typed1(blk1, mode, pt)
        case Apply(fun, args) =>
          normalTypedApply(tree, fun, args) match {
            case treeInfo.ArrayInstantiation(level, componentType, arg) =>
              // convert new Array[T](len) to evidence[ClassTag[T]].newArray(len)
              // convert new Array^N[T](len) for N > 1 to evidence[ClassTag[Array[...Array[T]...]]].newArray(len)
              // where Array HK gets applied (N-1) times
              val tagType = (1 until level).foldLeft(componentType)((res, _) => arrayType(res))

              val tree1: Tree = resolveClassTag(tree.pos, tagType) match {
                case EmptyTree => MissingClassTagError(tree, tagType)
                case tag       => atPos(tree.pos)(new ApplyToImplicitArgs(Select(tag, nme.newArray), arg :: Nil))
              }
              if (tree1.isErrorTyped) tree1 else typed(tree1, mode, pt)
            case Apply(Select(fun, nme.apply), _) if treeInfo.isSuperConstrCall(fun) => TooManyArgumentListsForConstructor(tree) //scala/bug#5696
            case tree1 if mode.inPatternMode && tree1.tpe.paramSectionCount > 0 =>
              // For a case class C with more than two parameter lists,
              // C(_) is typed as C(_)() which is a method type like ()C.
              // In a pattern, just use the final result type, C in this case.
              // The enclosing context may be case c @ C(_) => or val c @ C(_) = v.
              tree1 modifyType (_.finalResultType)
              tree1
            case tree1 @ Apply(_, args1) if settings.multiargInfix && tree.hasAttachment[MultiargInfixAttachment.type] && args1.lengthCompare(1) > 0 =>
              warnMultiargInfix(tree1)
              tree1
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

        def mkUpdate(table: Tree, indices: List[Tree], args_? : Option[List[Tree]]) =
          gen.evalOnceAll(table :: indices, context.owner, fresh) {
            case tab :: is =>
              def mkCall(name: Name, extraArgs: Tree*) =
                Apply(
                  Select(tab(), name) setPos table.pos,
                  is.map(i => i()) ++ extraArgs
                ) setPos tree.pos

              def mkApplies(core: Tree) = args_?.fold(core) { args =>
                Apply(core, args) setPos wrappingPos(core :: args)
              }
              mkCall(
                nme.update,
                Apply(Select(mkApplies(mkCall(nme.apply)), prefix) setPos fun.pos, args) setPos tree.pos
              )
            case _ => EmptyTree
          }

        val assignment = qual match {
          case Ident(_) =>
            mkAssign(qual)

          case Select(qualqual, _) =>
            gen.evalOnce(qualqual, context.owner, fresh) { qq =>
              val qq1 = qq()
              mkAssign(Select(qq1, qual.symbol) setPos qual.pos)
            }

          case Apply(fn, extra) if qual.isInstanceOf[ApplyToImplicitArgs] =>
            fn match {
              case treeInfo.Applied(Select(table, nme.apply), _, indices :: Nil) =>
                // table(indices)(implicits)
                mkUpdate(table, indices, Some(extra))
              case _  => UnexpectedTreeAssignmentConversionError(qual)
            }

          case Apply(fn, indices) =>
            fn match {
              case treeInfo.Applied(Select(table, nme.apply), _, Nil) =>
                mkUpdate(table, indices, None)
              case _  => UnexpectedTreeAssignmentConversionError(qual)
            }

          case x => throw new MatchError(x)
        }
        assignment
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
            ps = site.parents filter (_.typeSymbol.name == mix)
          if (ps.isEmpty) {
            debuglog("Fatal: couldn't find site " + site + " in " + site.parents.map(_.typeSymbol.name))
            if (phase.erasedTypes && context.enclClass.owner.isTrait) {
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

        val owntype =
          if (!mix.isEmpty) findMixinSuper(clazz.tpe)
          else if (context.inSuperInit) clazz.info.firstParent
          else intersectionType(clazz.info.parents)

        treeCopy.Super(tree, qual1, mix) setType SuperType(clazz.thisType, owntype)
      }

      def typedThis(tree: This) =
        tree.symbol orElse qualifyingClass(tree, tree.qual, packageOK = false, immediate = true) match {
          case NoSymbol => tree
          case clazz    =>
            tree setSymbol clazz setType clazz.thisType.underlying
            if (isStableContext(tree, mode, pt)) tree setType clazz.thisType else tree
        }

      // For Java, instance and static members are in the same scope, but we put the static ones in the companion object
      // so, when we can't find a member in the class scope, check the companion
      def inCompanionForJavaStatic(cls: Symbol, name: Name): Symbol =
        if (!(context.unit.isJava && cls.isClass)) NoSymbol
        else context.javaFindMember(cls.typeOfThis, name, s => s.isStaticMember || s.isStaticModule)._2
      // For Java, a selection p.q requires checking if p is a type with member q; otherwise it is a package.
      // If a non-package term was found, look for a class; otherwise just look for a package.
      def repairJavaSelection(qual: Tree, name: Name): Symbol =
        if (!context.unit.isJava || !qual.hasAttachment[RootSelection.type] || qual.symbol.hasPackageFlag) NoSymbol
        else qual match {
          case Ident(qname) =>
            val found =
              if (qual.symbol.isTerm) {
                val lookup = context.lookupSymbol(qname.toTypeName, s => qualifies(s) && s.isClass)
                if (lookup.isSuccess) inCompanionForJavaStatic(lookup.symbol, name) else NoSymbol
              }
              else NoSymbol
            found.orElse {
              context.lookupSymbol(qname, s => qualifies(s) && s.hasPackageFlag) match {
                case LookupSucceeded(_, pkg) => member(pkg.info, name)
                case _ => NoSymbol
              }
            }
          case _ => NoSymbol
        }

      // If they try C.tupled, make it (C.apply _).tupled
      def fixUpCaseTupled(tree: Tree, qual: Tree, name: Name, mode: Mode): Tree =
        if (!isPastTyper && qual.symbol != null && qual.symbol.isModule && qual.symbol.companion.isCase &&
            context.undetparams.isEmpty && fixableFunctionMembers.contains(name)) {
          val t2 = {
            val t = atPos(tree.pos)(Select(qual, nme.apply))
            val t1 = typedSelect(t, qual, nme.apply)
            typed(atPos(tree.pos)(Select(etaExpand(t1, context.owner), name)), mode, pt)
          }
          if (!t2.isErroneous) {
            val msg = s"The method `apply` is inserted. The auto insertion will be deprecated, please write `($qual.apply _).$name` explicitly."
            context.deprecationWarning(tree.pos, qual.symbol, msg, "2.13.13")
            t2
          }
          else EmptyTree
        }
        else EmptyTree

      /* Attribute a selection where `tree` is `qual.name`.
       * `qual` is already attributed.
       */
      def typedSelect(tree: Tree, qual: Tree, name: Name): Tree = {
        // note: on error, we discard the work we did in type checking tree.qualifier into qual
        // (tree is either Select or SelectFromTypeTree, and qual may be different from tree.qualifier because it has been type checked)
        val qualTp = qual.tpe
        if ((qualTp eq null) || qualTp.isError) setError(tree)
        else if (name.isTypeName && qualTp.isVolatile)  // TODO: use same error message for volatileType#T and volatilePath.T?
          if (tree.isInstanceOf[SelectFromTypeTree]) TypeSelectionFromVolatileTypeError(tree, qual)
          else UnstableTreeError(qual)
        else {
          def asDynamicCall = mkInvoke(context, tree, qual, name) map { t =>
            wrapErrors(t, _.typed1(t, mode, pt))
          }
          def checkDubiousUnitSelection(result: Tree): Unit =
            if (!isPastTyper && isUniversalMember(result.symbol))
              context.warning(tree.pos, s"dubious usage of ${result.symbol} with unit value", WarningCategory.LintUniversalMethods)

          val sym = tree.symbol
            .orElse(member(qualTp, name))
            .orElse(inCompanionForJavaStatic(qualTp.typeSymbol, name))
            .orElse(repairJavaSelection(qual, name))
          if ((sym eq NoSymbol) && name != nme.CONSTRUCTOR && mode.inAny(EXPRmode | PATTERNmode)) {
            // symbol not found? --> try to convert implicitly to a type that does have the required
            // member.  Added `| PATTERNmode` to allow enrichment in patterns (so we can add e.g., an
            // xml member to StringContext, which in turn has an unapply[Seq] method)

              def checkDubiousAdaptation(sel: Tree): Unit = if (!isPastTyper && settings.lintNumericMethods) {
                val dubious = ScalaIntegralValueClasses(qualTp.typeSymbol) && sel.symbol != null && (
                  sel.symbol.owner.eq(BoxedFloatClass) || sel.symbol.owner.eq(RichFloatClass))
                if (dubious)
                  context.warning(tree.pos, s"dubious usage of ${sel.symbol} with integer value", WarningCategory.LintNumericMethods)
              }
              val qual1 = adaptToMemberWithArgs(tree, qual, name, mode)
              val fixed =
                if ((qual1 ne qual) && !qual1.isErrorTyped)
                  typed(treeCopy.Select(tree, qual1, name), mode, pt).tap(checkDubiousAdaptation)
                else
                  fixUpCaseTupled(tree, qual, name, mode)
              if (!fixed.isEmpty)
                return fixed
          }

          // This special-case complements the logic in `adaptMember` in erasure, it handles selections
          // from `Super`. In `adaptMember`, if the erased type of a qualifier doesn't conform to the
          // owner of the selected member, a cast is inserted, e.g., (foo: Option[String]).get.trim).
          // Similarly, for `super.m`, typing `super` during erasure assigns the superclass. If `m`
          // is defined in a trait, this is incorrect, we need to assign a type to `super` that conforms
          // to the owner of `m`. Adding a cast (as in `adaptMember`) would not work, `super.asInstanceOf`
          // is not a valid tree.
          if (phase.erasedTypes && qual.isInstanceOf[Super]) {
            //  See the comment in `preErase` why we use the attachment (scala/bug#7936)
            val qualSym = tree.getAndRemoveAttachment[QualTypeSymAttachment] match {
              case Some(a) => a.sym
              case None => sym.owner
            }
            qual.setType(qualSym.tpe)
          }

          if (!reallyExists(sym)) {
            def handleMissing: Tree = {
              def errorTree = missingSelectErrorTree(tree, qual, name)
              def asTypeSelection =
                if (context.unit.isJava && name.isTypeName) {
                  // scala/bug#3120 Java uses the same syntax, A.B, to express selection from the
                  // value A and from the type A. We have to try both.
                  atPos(tree.pos)(gen.convertToSelectFromType(qual, name)) match {
                    case EmptyTree => None
                    case tree1     => Some(typed1(tree1, mode, pt))
                  }
                } else None

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
            if ((sym ne NoSymbol) && !qual.tpe.isStable && argsDependOnPrefix(sym)) {
              // Rewrites "qual.name ..." to "{ val lhs = qual ; lhs.name ... }" in cases where
              // qual is not stable and name has a method type which depends on its prefix. If
              // this is the case then hoisting qual out as a stable val means that members of
              // implicit scopes which are accessible via lhs can be candidates for satisfying
              // implicit (conversions to) arguments of name.

              val vsym = context.owner.newValue(freshTermName(nme.STABILIZER_PREFIX), qual.pos.focus, SYNTHETIC | ARTIFACT | STABLE)
              vsym.setInfo(uncheckedBounds(qual.tpe))
              val vdef = atPos(vsym.pos)(ValDef(vsym, focusInPlace(qual)) setType NoType)
              context.pendingStabilizers ::= vdef
              qual.changeOwner(context.owner -> vsym)
              val newQual = Ident(vsym) setType singleType(NoPrefix, vsym) setPos qual.pos.focus
              return typedSelect(tree, newQual, name).modifyType(_.map {
                // very specific fix for scala/bug#12987 (details in the ticket)
                case t: AliasTypeRef if t.pre.termSymbol == vsym && context.undetparams.contains(t.normalize.typeSymbol) =>
                  t.normalize
                case t => t
              })
            }

            val tree1 = tree match {
              case Select(_, _)             => treeCopy.Select(tree, qual, name)
              case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
              case x                        => throw new MatchError(x)
            }
            val pre = qual.tpe
            var accessibleError: AccessTypeError = null
            val result = silent(_.makeAccessible(tree1, sym, pre, qual)) match {
              case SilentTypeError(err) => err match {
                case err: AccessTypeError =>
                  accessibleError = err
                  tree1
                case _ =>
                  SelectWithUnderlyingError(tree, err)
                  return tree
              }
              case SilentResultValue(result: Tree)               => stabilize(result, pre, mode, pt)
              case SilentResultValue((result: Tree, pre1: Type)) => stabilize(result, pre1, mode, pt)
              case x => throw new MatchError(x)
            }

            result match {
              case _ if accessibleError != null =>
                // don't adapt constructor, scala/bug#6074
                val qual1 = if (name == nme.CONSTRUCTOR) qual
                            else adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = false, saveErrors = false)
                if (!qual1.isErrorTyped && (qual1 ne qual))
                  typed(Select(qual1, name) setPos tree.pos, mode, pt)
                else
                // before failing due to access, try a dynamic call.
                  asDynamicCall getOrElse {
                    context.issue(accessibleError)
                    setError(tree)
                  }
              // could checkAccessible (called by makeAccessible) potentially have skipped checking a type application in qual?
              case SelectFromTypeTree(qualifier@TypeTree(), name) if qualifier.tpe.typeArgs.nonEmpty => // TODO: somehow the new qual is not checked in refchecks
                treeCopy.SelectFromTypeTree(
                  result,
                  TypeTreeWithDeferredRefCheck(qualifier) { () => val tp = qualifier.tpe; val sym = tp.typeSymbolDirect
                    // will execute during refchecks -- TODO: make private checkTypeRef in refchecks public and call that one?
                    checkBounds(qualifier, tp.prefix, sym.owner, sym.typeParams, tp.typeArgs, "")
                    qualifier // you only get to see the wrapped tree after running this check :-p
                  }.setType(qualifier.tpe).setPos(qual.pos),
                  name
                )
              case _ =>
                if (settings.lintUniversalMethods && qualTp.widen.eq(UnitTpe)) checkDubiousUnitSelection(result)
                if (isConstantType(qualTp) && constfold.foldableUnaryOps(name)) constfold(result, context.owner)
                else result
            }
          }
        }
      }

      def typedTypeSelectionQualifier(tree: Tree, pt: Type) =
        context.withImplicitsDisabled {
          val mode1 = MonoQualifierModes | mode.onlyTypePat
          typed(checkRootOfQualifier(tree, mode1), mode1, pt)
        }

      def typedSelectOrSuperCall(tree: Select) = tree match {
        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          // the qualifier type of a supercall constructor is its first parent class
          typedSelect(tree, typedSelectOrSuperQualifier(qual), nme.CONSTRUCTOR)
        case Select(qual, name) =>
          if (name.isTypeName) {
            val qualTyped = typedTypeSelectionQualifier(tree.qualifier, WildcardType)
            val qualStableOrError =
              if (qualTyped.isErrorTyped || unit.isJava || treeInfo.admitsTypeSelection(qualTyped)) qualTyped
              else UnstableTreeError(qualTyped)
            typedSelect(tree, qualStableOrError, name)
          } else {
            if (settings.areStatisticsEnabled) statistics.incCounter(typedSelectCount)
            val qualTyped = checkDead(context, typedQualifier(qual, mode))
            val tree1 = typedSelect(tree, qualTyped, name)

            if (tree.hasAttachment[PostfixAttachment.type])
              checkFeature(tree.pos, currentRun.runDefinitions.PostfixOpsFeature, name.decode)
            val sym = tree1.symbol
            if (sym != null && sym.isOnlyRefinementMember && !sym.isMacro)
              checkFeature(tree1.pos, currentRun.runDefinitions.ReflectiveCallsFeature, sym.toString)

            qualTyped.symbol match {
              case s: Symbol if s.isRootPackage => treeCopy.Ident(tree1, name)
              case _ => tree1
            }
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
        def inEmptyPackage = if (settings.exposeEmptyPackage.value) lookupInEmpty(name) else NoSymbol

        def issue(err: AbsTypeError) = {
          // Avoiding some spurious error messages: see scala/bug#2388.
          val suppress = reporter.hasErrors && (name startsWith tpnme.ANON_CLASS_NAME)
          if (!suppress)
            ErrorUtils.issueTypeError(err)

          setError(tree)
        }
        // ignore current variable scope in patterns to enforce linearity
        val startContext = if (mode.typingPatternOrTypePat) context.outer else context

        def asTypeName = if (mode.inAll(MonoQualifierModes) && unit.isJava && name.isTermName) {
          startContext.lookupSymbol(name.toTypeName, qualifies).symbol
        } else NoSymbol

        // in Java, only pick a package p if it is rooted (no relative packaging)
        def termQualifies(sym: Symbol) = qualifies(sym) && (
            !startContext.unit.isJava  || !sym.hasPackageFlag
          || sym.owner.isEffectiveRoot || sym.owner.isRootPackage || sym.isRootPackage
        )
        val nameLookup  = tree.symbol match {
          case NoSymbol => startContext.lookupSymbol(name, termQualifies)
          case oksymbol => LookupSucceeded(EmptyTree, oksymbol)
        }
        import InferErrorGen._
        nameLookup match {
          case LookupAmbiguous(msg)            => issue(AmbiguousIdentError(tree, name, msg))
          case LookupInaccessible(symbol, msg) => issue(AccessError(tree, symbol, context, msg))
          case LookupNotFound =>
            asTypeName orElse inEmptyPackage orElse lookupInRoot(name) match {
              case NoSymbol =>
                def hasOutput = settings.outputDirs.getSingleOutput.isDefined || settings.outputDirs.outputs.nonEmpty
                val hidden = !unit.isJava && hasOutput && {
                  startContext.lookupSymbol(name.companionName, _ => true) match {
                    case LookupSucceeded(qualifier, symbol) if symbol.owner.hasPackageFlag && symbol.sourceFile != null =>
                      symbol.owner.info.decls.lookupAll(name).toList match {
                        case other :: Nil if other.sourceFile == null =>
                          val nameString = name.toString
                          classPath.classes(symbol.owner.fullNameString).find(_.name == nameString).exists { repr =>
                            settings.outputDirs.getSingleOutput match {
                              // is the class file not in the output directory, so it's a dependency not a stale symbol
                              case Some(out) => repr.binary.exists(!_.path.startsWith(out.path))
                              // is the class file not in any output directory
                              case _ => repr.binary.exists { bin =>
                                !settings.outputDirs.outputs.exists { case (_, out) =>
                                  bin.path.startsWith(out.path)
                                }
                              }
                            }
                          }
                        case _ => false
                      }
                    case _ => false
                  }
                }
                issue(SymbolNotFoundError(tree, name, context.owner, inPattern = mode.in(all = PATTERNmode, none = APPSELmode | TYPEPATmode), hidden = hidden))
              case oksymbol => typed1(tree.setSymbol(oksymbol), mode, pt)
            }
          case LookupSucceeded(qual, symbol) =>
            symbol.getAndRemoveAttachment[LookupAmbiguityWarning].foreach(w => {
              val cat = if (currentRun.isScala3) Scala3Migration else WarningCategory.Other
              val fix = runReporting.codeAction("make reference explicit", tree.pos.focusStart, w.fix, w.msg)
              runReporting.warning(tree.pos, w.msg, cat, context.owner, fix)
            })
            if (currentRun.isScala3)
              tree.getAndRemoveAttachment[VirtualStringContext.type].foreach(_ =>
                if (symbol != definitions.StringContextModule)
                  runReporting.warning(
                    tree.pos,
                    s"In Scala 3 (or with -Xsource-features:string-context-scope), String interpolations always use scala.StringContext (${symbol.fullNameString} is used here)",
                    Scala3Migration,
                    context.owner)
              )
            val onSuccess =
              if (symbol.isThisSym)
                typed1(This(symbol.owner).setPos(tree.pos), mode, pt) // this -> Foo.this
              else if (symbol.rawname == nme.classOf && currentRun.runDefinitions.isPredefClassOf(symbol) && pt.typeSymbol == ClassClass && pt.typeArgs.nonEmpty) {
                // Inferring classOf type parameter from expected type.  Otherwise an
                // actual call to the stubbed classOf method is generated, returning null.
                typedClassOf(tree, TypeTree(pt.typeArgs.head).setPos(tree.pos.focus))
              }
              else {
                val pre1  = if (symbol.isTopLevel) symbol.owner.thisType else if (qual == EmptyTree) NoPrefix else qual.tpe
                if (settings.lintUniversalMethods && !pre1.isInstanceOf[ThisType] && isUniversalMember(symbol))
                  context.warning(tree.pos, s"${symbol.nameString} not selected from this instance", WarningCategory.LintUniversalMethods)
                val tree1 = if (qual == EmptyTree) tree else {
                  val pos = tree.pos
                  Select(atPos(pos.focusStart)(qual), name).setPos(pos)
                }
                var tree2: Tree = null
                var pre2: Type = pre1
                makeAccessible(tree1, symbol, pre1, qual) match {
                  case (t: Tree, tp: Type) =>
                    tree2 = t
                    pre2 = tp
                  case t: Tree =>
                    tree2 = t
                  case x => throw new MatchError(x)
                }
                // scala/bug#5967 Important to replace param type A* with Seq[A] when seen from from a reference,
                // to avoid inference errors in pattern matching.
                stabilize(tree2, pre2, mode, pt).modifyType(dropIllegalStarTypes)
              }
            onSuccess.setAttachments(tree.attachments)
        }
      }

      def typedIdentOrWildcard(tree: Ident) = {
        val name = tree.name
        if (settings.areStatisticsEnabled) statistics.incCounter(typedIdentCount)
        if (!tree.isBackquoted &&
            ((name == nme.WILDCARD && mode.typingPatternNotConstructor) ||
             (name == tpnme.WILDCARD && mode.inTypeMode)))
          tree setType makeFullyDefined(pt)
        else
          typedIdent(tree, name)
      }

      def typedCompoundTypeTree(tree: CompoundTypeTree) = {
        val templ = tree.templ
        val parents1 = templ.parents mapConserve (typedType(_, mode))

        // This is also checked later in typedStats, but that is too late for scala/bug#5361, so
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
          tree setType (if (templ.exists(_.isErroneous)) ErrorType else self) // Being conservative to avoid scala/bug#5361
        }
      }

      def typedAppliedTypeTree(tree: AppliedTypeTree) = {
        val tpt = tree.tpt
        val args = tree.args
        val tpt1 = typed1(tpt, mode | FUNmode | TAPPmode, WildcardType)

        if (tpt1.isErrorTyped) {
          tpt1
        } else if (!tpt1.hasSymbolField) {
          AppliedTypeNoParametersError(tree, tpt1.tpe)
        } else {
          val tparams = tpt1.symbol.typeParams
          val isComplete = tpt1.symbol.rawInfo.isComplete

          if (sameLength(tparams, args)) {
            // @M: kind-arity checking is done here and in adapt, full kind-checking is in checkKindBounds (in Infer)
            val args1 = map2Conserve(args, tparams) { (arg, tparam) =>
              def ptParams = Kind.FromParams(tparam.typeParams)

              // if symbol hasn't been fully loaded, can't check kind-arity except when we're in a pattern,
              // where we can (we can't take part in F-Bounds) and must (scala/bug#8023)
              val pt = if (mode.typingPatternOrTypePat) {
                tparam.initialize; ptParams
              }
              else if (isComplete) ptParams
              else Kind.Wildcard

              typedHigherKindedType(arg, mode, pt)
            }
            val argtypes = mapList(args1)(_.tpe)

            foreach2(args, tparams) { (arg, tparam) =>
              // note: can't use args1 in selector, because Binds got replaced
              val asym = arg.symbol
              def abounds = asym.info.bounds
              def tbounds = tparam.info.bounds
              // TODO investigate whether this should be merged with the near duplicate in Inferencer
              // and whether or not we should avoid using setInfo here as well to avoid potentially
              // trampling on type history.
              def enhanceBounds(): Unit = {
                val TypeBounds(lo0, hi0) = abounds: @unchecked
                val TypeBounds(lo1, hi1) = tbounds.subst(tparams, argtypes): @unchecked
                val lo = lub(List(lo0, lo1))
                val hi = glb(List(hi0, hi1))
                if (!(lo =:= lo0 && hi =:= hi0))
                  asym setInfo logResult(s"Updating bounds of ${asym.fullLocationString} in $tree from '$abounds' to")(TypeBounds(lo, hi))
              }
              if (asym != null && asym.isAbstractType) {
                arg match {
                  // I removed the Ident() case that partially fixed scala/bug#1786,
                  // because the stricter bounds being inferred broke e.g., slick
                  // worse, the fix was compilation order-dependent
                  // sharpenQuantifierBounds (used in skolemizeExistential) has an alternative fix (scala/bug#6169) that's less invasive
                  case Bind(_, _) => enhanceBounds()
                  case _          =>
                }
              }
            }
            val original = treeCopy.AppliedTypeTree(tree, tpt1, args1)
            val result = TypeTree(appliedType(tpt1.tpe, argtypes)) setOriginal original
            val isPoly = tpt1.tpe.isInstanceOf[PolyType]
            if (isPoly) // did the type application (performed by appliedType) involve an unchecked beta-reduction?
              TypeTreeWithDeferredRefCheck(result) { () =>
                // wrap the tree and include the bounds check -- refchecks will perform this check (that the beta reduction was indeed allowed) and unwrap
                // we can't simply use original in refchecks because it does not contains types
                // (and the only typed trees we have been mangled so they're not quite the original tree anymore)
                checkBounds(result, tpt1.tpe.prefix, tpt1.symbol.owner, tpt1.symbol.typeParams, argtypes, "")
                result // you only get to see the wrapped tree after running this check :-p
              }.setType(result.tpe).setPos(result.pos)
            else result
          } else if (tparams.isEmpty) {
            AppliedTypeNoParametersError(tree, tpt1.tpe)
          } else {
            //Console.println("\{tpt1}:\{tpt1.symbol}:\{tpt1.symbol.info}")
            if (settings.isDebug) Console.println(s"$tpt1:${tpt1.symbol}:${tpt1.symbol.info}")//debug
            AppliedTypeWrongNumberOfArgsError(tree, tpt1, tparams)
          }
        }
      }

      val sym: Symbol = tree.symbol
      if ((sym ne null) && (sym ne NoSymbol)) sym.initialize

      def typedPackageDef(pdef0: PackageDef) = {
        val pdef = treeCopy.PackageDef(pdef0, pdef0.pid, pluginsEnterStats(this, namer.expandMacroAnnotations(pdef0.stats)))
        val pid1 = context.withMode(ContextMode.InPackageClauseName)(typedPackageQualifier(pdef.pid).asInstanceOf[RefTree])
        assert(sym.moduleClass ne NoSymbol, sym)
        if (pid1.symbol.ne(NoSymbol) && !(pid1.symbol.hasPackageFlag || pid1.symbol.isModule))
          reporter.error(pdef.pos, s"There is name conflict between the ${pid1.symbol.fullName} and the package ${sym.fullName}.")
        val stats1 = newTyper(context.make(tree, sym.moduleClass, sym.info.decls))
                  .typedStats(pdef.stats, NoSymbol)
        treeCopy.PackageDef(tree, pid1, stats1) setType NoType
      }

      /*
       * The typer with the correct context for a method definition. If the method is a default getter for
       * a constructor default, the resulting typer has a constructor context (fixes scala/bug#5543).
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

      def typedTry(tree: Try) = {
        def warn(pos: Position, name: Name) = {
          val msg = s"This catches all Throwables. If this is really intended, use `case ${name.decoded} : Throwable` to clear this warning."
          context.warning(pos, msg, WarningCategory.Other)
        }
        def issueTryWarnings(tree: Try): Try = {
          def checkForCatchAll(cdef: CaseDef): Unit = {
            def unbound(t: Tree) = t.symbol == null || t.symbol == NoSymbol
            if (cdef.guard.isEmpty) cdef.pat match {
              case Bind(name, i @ Ident(_)) if unbound(i) => warn(cdef.pat.pos, name)
              case i @ Ident(name) if unbound(i)          => warn(cdef.pat.pos, name)
              case _                                      =>
            }
          }
          if (!isPastTyper) tree match {
            case Try(_, Nil, fin) =>
              if (fin eq EmptyTree)
                context.warning(tree.pos, "A try without a catch or finally is equivalent to putting its body in a block; no exceptions are handled.", WarningCategory.Other)
            case Try(_, catches, _) =>
              catches foreach checkForCatchAll
          }
          tree
        }

        val Try(block, catches, finalizer) = tree
        val block1   = typed(block, pt)
        val cases    = catches match {
          case CaseDef(EmptyTree, EmptyTree, catchExpr) :: Nil =>
            val e = typed(catchExpr, functionType(List(ThrowableTpe), pt))
            val catcher =
              if (isPartialFunctionType(e.tpe)) treeBuilder.makeCatchFromExpr(e)
              else {
                warn(e.pos, nme.WILDCARD)
                treeBuilder.makeCatchFromFunc(e)
              }
            catcher :: Nil
          case _ => catches
        }
        val catches1 = typedCases(cases, ThrowableTpe, pt)
        val fin1     = if (finalizer.isEmpty) finalizer else typed(finalizer, UnitTpe)

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
          // because `expr` might contain nested macro calls (see scala/bug#6673).
          // Otherwise, (check for dead code, and) eta-expand.
          case MethodValue(expr) =>
            // Need to type in FUNmode so that we accept a method type (which also means we can't use our pt),
            // this does mean no overloading is performed. The main reason to ignore pt and move to FUNmode is that
            // the `m` in `m _` could involve an implicit conversion, which will go through adapt after converting,
            // which will run afoul of the restriction that a method-typed tree is only allowed when a function type is expected.
            // We peeled off the `_` marker for the typed1 call, so we don't know that the user has requested eta-expansion.
            // See scala/bug#8299.
            val funTyped = typed1(suppressMacroExpansion(expr), mode | FUNmode, WildcardType)
            if (funTyped.tpe.isInstanceOf[OverloadedType]) inferExprAlternative(funTyped, pt)
            funTyped match {
              case macroDef if treeInfo.isMacroApplication(macroDef) => MacroEtaError(macroDef)
              case methodValue                                       => typedEta(checkDead(context, methodValue))
            }
          case Typed(expr, tpt) =>
            val tpt1  = typedType(tpt, mode)                           // type the ascribed type first
            val exprWithAttachment =
              if (definitions.isUnitType(tpt1.tpe)) expr.updateAttachment(TypedExpectingUnitAttachment)
              else expr
            val expr1 = typed(exprWithAttachment, mode.onlySticky, tpt1.tpe.deconst) // then type the expression with tpt1 as the expected type
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
        assert(phase.erasedTypes, "typedApplyDynamic called before erasure")
        val qual1 = typed(tree.qual, AnyRefTpe)
        val args1 = tree.args mapConserve (arg => typed(arg, AnyRefTpe))
        treeCopy.ApplyDynamic(tree, qual1, args1) setType AnyRefTpe
      }

      def typedReferenceToBoxed(tree: ReferenceToBoxed) = {
        val id = tree.ident
        val id1 = typed1(id, mode, pt).asInstanceOf[Ident]
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
          //context.tree.exists(t => t.pos.includes(lit.pos) && hasMacroExpansionAttachment(t))
          // testing pos works and may suffice
          //openMacros.exists(_.macroApplication.pos.includes(lit.pos))
          // tests whether the lit belongs to the expandee of an open macro
          openMacros.exists(_.macroApplication.attachments.get[MacroExpansionAttachment] match {
            case Some(MacroExpansionAttachment(_, t: Tree)) => t.exists(_ eq lit)
            case _                                          => false
          })
        }
        val checkMacroExpansion = settings.warnMacros.value match {
          case "both" | "after" => true
          case _ => !isMacroExpansion
        }
        // An interpolation desugars to `StringContext(parts).m(args)`, so obviously not missing.
        // `implicitNotFound` annotations have strings with `${A}`, so never warn for that.
        // Also don't warn for macro expansion unless they ask for it.
        def mightBeMissingInterpolation: Boolean = context.enclosingApply.tree match {
          case Apply(Select(Apply(RefTree(_, nme.StringContextName), _), _), _) => false
          case Apply(Select(New(RefTree(_, tpnme.implicitNotFound)), _), _)     => false
          case _                                                                => checkMacroExpansion
        }
        def maybeWarn(s: String): Unit = {
          def warn(message: String) = context.warning(lit.pos, s"possible missing interpolator: $message", WarningCategory.LintMissingInterpolator)
          def isPlausible(id: String): Boolean = {
            def requiresNoArgs(tp: Type): Boolean = tp match {
              case PolyType(_, restpe)     => requiresNoArgs(restpe)
              case MethodType(Nil, restpe) => requiresNoArgs(restpe)  // may be a curried method - can't tell yet
              case MethodType(p :: _, _)   => p.isImplicit            // implicit method requires no args
              case _                       => true                    // catches all others including NullaryMethodType
            }
            def isNullaryTerm: Boolean = {
              val idName = TermName(id)
              val maybe = context.lookupSymbol(idName, _ => true).symbol
              maybe != NoSymbol && !maybe.hasPackageFlag && !maybe.isModule &&
              !context.owner.ownersIterator.exists(_.name.dropLocal == idName) && // avoid forcing an owner
              maybe.alternatives.exists(x => requiresNoArgs(x.info))
            }
            id == "this" || isNullaryTerm
          }
          val suspiciousExprs  = InterpolatorCodeRegex.findAllMatchIn(s)
          def suspiciousIdents = InterpolatorIdentRegex.findAllIn(s)

          if (suspiciousExprs.hasNext) {
            def isCheapIdent(expr: String) = Character.isJavaIdentifierStart(expr.charAt(0)) && expr.tail.forall(Character.isJavaIdentifierPart)
            def warnableExpr(expr: String) = !expr.isEmpty && (!isCheapIdent(expr) || isPlausible(expr))
            val exprs = suspiciousExprs.map(_ group 1).toList
            // short-circuit on leading ${}
            if (!exprs.head.isEmpty && exprs.exists(warnableExpr))
              warn("detected an interpolated expression") // "${...}"
          } else suspiciousIdents.toList match {
            case Nil =>
            case id :: Nil => if (isPlausible(id.substring(1))) warn(s"detected interpolated identifier `$id`")
            case all => if (all.forall(id => isPlausible(id.substring(1)))) warn(all.mkString("detected interpolated identifiers `", "`, `", "`"))
          }
        }
        lit match {
          case Literal(Constant(s: String)) if mightBeMissingInterpolation => maybeWarn(s)
          case _                                                           =>
        }
      }

      def typedLiteral(tree: Literal) = {
        if (settings.warnMissingInterpolator) warnMissingInterpolator(tree)

        tree.setType(if (tree.value.tag == UnitTag) UnitTpe else ConstantType(tree.value))
      }

      def typedSingletonTypeTree(tree: SingletonTypeTree) = {
        val refTyped = typedTypeSelectionQualifier(tree.ref, WildcardType)

        if (refTyped.isErrorTyped) setError(tree)
        else {
            // .resultType unwraps NullaryMethodType (accessor of a path)
          // .deconst unwraps the ConstantType to a LiteralType (for literal-based singleton types)
          if (!treeInfo.admitsTypeSelection(refTyped)) UnstableTreeError(tree)
          else treeCopy.SingletonTypeTree(tree, refTyped).setType(refTyped.tpe.resultType.deconst)
        }
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

      @inline def typedTypTree(tree: TypTree): Tree = tree match {
        case tree: TypeTree                     => typedTypeTree(tree)
        case tree: AppliedTypeTree              => typedAppliedTypeTree(tree)
        case tree: TypeBoundsTree               => typedTypeBoundsTree(tree)
        case tree: SingletonTypeTree            => typedSingletonTypeTree(tree)
        case tree: SelectFromTypeTree           => typedSelect(tree, typedType(tree.qualifier, mode), tree.name)
        case tree: CompoundTypeTree             => typedCompoundTypeTree(tree)
        case tree: ExistentialTypeTree          => typedExistentialTypeTree(tree)
        case tree: TypeTreeWithDeferredRefCheck => tree // TODO: retype the wrapped tree?
        case _                                  => abort(s"unexpected type-representing tree: ${tree.getClass}\n$tree")
      }

      @inline def typedMemberDef(tree: MemberDef): Tree = tree match {
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
        case tree: Block            =>
          val blockContext = context.makeNewScope(tree, context.owner)
          try typerWithLocalContext(blockContext)(_.typedBlock(tree, mode, pt))
          finally context.undetparams ++= blockContext.undetparams
        case tree: If               => typedIf(tree)
        case tree: TypeApply        => typedTypeApply(tree)
        case tree: Function         => typedFunction(tree)
        case tree: Match            => typedVirtualizedMatch(tree)
        case tree: New              => typedNew(tree)
        case tree: Assign           => typedAssign(tree.lhs, tree.rhs)
        case tree: NamedArg         => typedAssign(tree.lhs, tree.rhs)
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
      @inline def typedInAnyMode(tree: Tree): Tree = tree match {
        case tree: Ident   => typedIdentOrWildcard(tree)
        case tree: Bind    => typedBind(tree)
        case tree: Apply   => typedApply(tree)
        case tree: Select  => typedSelectOrSuperCall(tree)
        case tree: Literal => typedLiteral(tree)
        case tree: Typed   => typedTyped(tree)
        case tree: This    => typedThis(tree)  // scala/bug#6104
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
      val startByType = if (settings.areHotStatisticsEnabled) statistics.pushTimer(byTypeStack, byTypeNanos(tree.getClass)) else null
      if (settings.areHotStatisticsEnabled) statistics.incCounter(visitsByType, tree.getClass)
      val shouldPrintTyping = printTypings && !phase.erasedTypes && !noPrintTyping(tree)
      val shouldPopTypingStack = shouldPrintTyping && typingStack.beforeNextTyped(tree, mode, pt, context)

      def shouldInsertStabilizersImpl = tree match {
        case _ if phase.erasedTypes || mode.in(APPSELmode) || isMacroImplRef(tree) => false
        case _: Select | _: Apply | _: TypeApply                                   => true
        case _                                                                     => false
      }

      val shouldInsertStabilizers = shouldInsertStabilizersImpl
      val mode1: Mode = if (shouldInsertStabilizers) mode | APPSELmode else mode
      val savedPendingStabilizer = context.pendingStabilizers
      if (shouldInsertStabilizers) context.pendingStabilizers = Nil

      try {
        val ptPlugins = pluginsPt(pt, this, tree, mode1)
        def retypingOk = (
          context.retyping
            && (tree.tpe ne null)
            && (tree.tpe.isErroneous || !(tree.tpe <:< ptPlugins))
          )
        if (retypingOk) {
          tree.setType(null)
          if (tree.hasSymbolField) tree.symbol = NoSymbol
        }
        val alreadyTyped = tree.tpe ne null
        val shouldPrint = !alreadyTyped && !phase.erasedTypes
        val ptWild = if (mode1.inPatternMode)
          ptPlugins // scala/bug#5022 don't widen pt for patterns as types flow from it to the case body.
        else
          dropExistential(ptPlugins) // FIXME: document why this is done.
        val tree1: Tree = if (alreadyTyped) tree else typed1(tree, mode1, ptWild)

        if (shouldPrint)
          typingStack.showTyped(tree1)

        // Can happen during erroneous compilation - error(s) have been
        // reported, but we need to avoid causing an NPE with this tree
        if (tree1.tpe eq null)
          return setError(tree)

        tree1 setType pluginsTyped(tree1.tpe, this, tree1, mode1, ptPlugins)

        val adapted =
          if (tree1.isEmpty) tree1
          else {
            val result = adapt(tree1, mode1, ptPlugins, tree)
            if (typerShouldExpandDeferredMacros) {
              macroExpandAll(this, result)
            } else result
          }

        val result =
          if (shouldInsertStabilizers) addStabilizers(context.pendingStabilizers, adapted)
          else adapted

        if (shouldPrint)
          typingStack.showAdapt(tree1, result, ptPlugins, context)

        if (!isPastTyper)
          signalDone(context.asInstanceOf[analyzer.Context], tree, result)

        if (mode1.inPatternMode && !mode1.inPolyMode && result.isType)
          PatternMustBeValue(result, pt)

        if (shouldPopTypingStack) typingStack.showPop(result)

        result
      } catch {
        case ex: CyclicReference if global.propagateCyclicReferences =>
          throw ex
        case ex: TypeError =>
          tree.clearType()
          // The only problematic case are (recoverable) cyclic reference errors which can pop up almost anywhere.
          typingStack.printTyping(tree, s"caught $ex: while typing $tree")
          reportTypeError(context, tree.pos, ex)
          setError(tree)
        case ex: Exception =>
          // @M causes cyclic reference error
          devWarning(s"exception when typing $tree, pt=$pt")
          if (context != null && context.unit.exists && tree != null)
            logError("AT: " + tree.pos, ex)
          throw ex
      } finally {
        if (shouldPopTypingStack) typingStack.pop(tree)
        if (settings.areHotStatisticsEnabled) statistics.popTimer(byTypeStack, startByType)
        if (shouldInsertStabilizers) context.pendingStabilizers = savedPendingStabilizer
      }
    }

    private def addStabilizers(newStabilizers: List[Tree], expr: Tree): Tree = {
      if (newStabilizers.isEmpty) expr else {
        devWarningIf(newStabilizers.forall(_.symbol.owner == context.owner))(s"${context.owner} - ${(newStabilizers.map(vd => (vd.symbol, vd.symbol.owner.fullNameString)), context.owner)}")
        // Insert stabilizing ValDefs (if any) which might have been introduced during the typing of the original expression.
        Block(newStabilizers.reverse, expr).setPos(expr.pos).setType(expr.tpe)
      }
    }

    def atOwner(owner: Symbol): Typer =
      newTyper(context.make(owner = owner))

    def atOwner(tree: Tree, owner: Symbol): Typer =
      newTyper(context.make(tree, owner))

    /** Types expression or definition `tree`.
     */
    @inline final def typed(tree: Tree): Tree =
      typed(tree, context.defaultModeForTyped, WildcardType)

    @inline final def typedByValueExpr(tree: Tree, pt: Type = WildcardType): Tree = typed(tree, EXPRmode | BYVALmode, pt)

    def typedPos(pos: Position, mode: Mode, pt: Type)(tree: Tree) = typed(atPos(pos)(tree), mode, pt)
    def typedPos(pos: Position)(tree: Tree) = typed(atPos(pos)(tree))
    // TODO: see if this formulation would impose any penalty, since
    // it makes for a lot less casting.
    // def typedPos[T <: Tree](pos: Position)(tree: T): T = typed(atPos(pos)(tree)).asInstanceOf[T]

    /** Types expression `tree` with given prototype `pt`.
     */
    @inline final def typed(tree: Tree, pt: Type): Tree =
      typed(tree, context.defaultModeForTyped, pt)

    @inline final def typed(tree: Tree, mode: Mode): Tree =
      typed(tree, mode, WildcardType)

    /** Types qualifier `tree` of a select node.
     *  E.g. is tree occurs in a context like `tree.m`.
     */
    @inline final def typedQualifier(tree: Tree, mode: Mode, pt: Type): Tree =
      typed(checkRootOfQualifier(tree, mode), PolyQualifierModes | mode.onlyTypePat, pt) // TR: don't set BYVALmode, since qualifier might end up as by-name param to an implicit

    /** Types qualifier `tree` of a select node.
     *  E.g. is tree occurs in a context like `tree.m`.
     */
    @inline final def typedQualifier(tree: Tree, mode: Mode): Tree =
      typedQualifier(tree, mode, WildcardType)

    @inline final def typedQualifier(tree: Tree): Tree = typedQualifier(tree, NOmode, WildcardType)

    // if a package id is a selection from _root_ in scope, warn about semantics and set symbol for typedQualifier
    @inline final def typedPackageQualifier(tree: Tree): Tree = typedQualifier(checkRootOfPackageQualifier(tree))

    def checkRootOfPackageQualifier(q: Tree): Tree = {
      q match {
        case Select(id @ Ident(nme.ROOTPKG), _) if !id.hasExistingSymbol && id.hasAttachment[RootSelection.type] =>
          context.lookupSymbol(nme.ROOTPKG, p => p.hasPackageFlag && !p.isRootPackage) match {
            case LookupSucceeded(_, sym) =>
              runReporting.warning(
                id.pos,
                s"${nme.ROOTPKG} in root position in package definition does not refer to the root package, but to ${sym.fullLocationString}, which is in scope",
                WarningCategory.Other,
                context.owner)
              id.removeAttachment[RootSelection.type]
              id.setSymbol(sym)
            case _ =>
          }
        case _ =>
      }
      q
    }

    /** If import from path starting with _root_, warn if there is a _root_ value in scope,
     *  and ensure _root_ can only be the root package in that position.
     */
    def checkRootOfQualifier(q: Tree, mode: Mode): Tree = {
      q match {
        case Ident(nme.ROOTPKG) if !q.hasExistingSymbol && q.hasAttachment[RootSelection.type] =>
          val startContext = if (mode.typingPatternOrTypePat) context.outer else context
          startContext.lookupSymbol(nme.ROOTPKG, !_.isRootPackage) match {
            case LookupSucceeded(_, sym) =>
              runReporting.warning(
                q.pos,
                s"${nme.ROOTPKG} in root position of qualifier refers to the root package, not ${sym.fullLocationString}, which is in scope",
                WarningCategory.Other,
                context.owner)
            case _ =>
          }
          q.setSymbol(rootMirror.RootPackage)
        case _ =>
      }
      q
    }

    /** Types function part of an application */
    @inline final def typedOperator(tree: Tree): Tree = typed(tree, OperatorModes)

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
      typingInPattern(context.withImplicitsDisabledAllowEnrichment(typed(tree, PATTERNmode, pt)))
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

    def typedHigherKindedType(tree: Tree, @unused mode: Mode): Tree =
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

    // called from `methodSig`. Macro defs must have an explicit type.
    // The supplied expected type is ignored.
    def computeMacroDefType(ddef: DefDef, @unused pt: Type): Type = {
      assert(context.owner.isMacro, context.owner)
      assert(ddef.symbol.isMacro, ddef.symbol)

      val rhs1 =
        if (transformed contains ddef.rhs)
          transformed(ddef.rhs)
        else {
          val rhs1 = typedMacroBody(this, ddef)
          transformed(ddef.rhs) = rhs1
          rhs1
        }
      val isMacroBodyOkay = !ddef.symbol.isErroneous && !rhs1.exists(_.isErroneous) && rhs1 != EmptyTree
      val shouldInheritMacroImplReturnType = ddef.tpt.isEmpty
      if (isMacroBodyOkay && shouldInheritMacroImplReturnType) {
        def reportFailure() = {
          val commonMessage = "macro defs must have explicitly specified return types"
          ddef.symbol.setFlag(IS_ERROR)
          context.error(ddef.pos, commonMessage)
        }
        reportFailure()
      }
      AnyTpe
    }

    @inline final def transformedOr(tree: Tree, op: => Tree): Tree = lookupTransformed(tree) match {
      case Some(tree1) => tree1
      case _           => op
    }

    @inline
    final def transformedOrTyped(tree: Tree, mode: Mode, pt: Type): Tree = {
      lookupTransformed(tree) match {
        case Some(tree1) => tree1
        case _           => if (canSkipRhs(tree)) EmptyTree else typed(tree, mode, pt)
      }
    }
    final def lookupTransformed(tree: Tree): Option[Tree] =
      if (phase.erasedTypes) None // OPT save the hashmap lookup in erasure type and beyond
      else transformed remove tree

    private final def canSkipRhs(tree: Tree) = settings.Youtline.value && !tree.exists {
      case Super(_, mix) =>
        // conservative approximation of method bodies that may give rise to super accessors which must be
        // stored in pickle.
        context.owner.enclClass.isTrait || mix != tpnme.EMPTY
      case _ => false
    }
  }

  /** Finish computation of param aliases after typechecking is completed */
  final def finishComputeParamAlias(): Unit = {
    val classes = superConstructorCalls.keys.toArray
    // superclasses before subclasses to avoid a data race between `superAcc.alias` and `acc.setAlias` below.
    scala.util.Sorting.quickSort(classes)(Ordering.fromLessThan((a, b) => b.isLess(a)))

    for (sym <- classes) {
      for ((ownAcc, superAcc) <- superConstructorCalls.getOrElse(sym, Nil)) {
        // We have a corresponding parameter in the super class.
        val superClazz = sym.superClass
        val alias = (
          superAcc.initialize.alias // Is the param accessor is an alias for a field further up  the class hierarchy?
            orElse (superAcc getterIn superAcc.owner) // otherwise, lookup the accessor for the super
            filter (alias => superClazz.info.nonPrivateMember(alias.name) == alias) // the accessor must be public
          )
        if (alias.exists && !alias.accessed.isVariable && !isRepeatedParamType(alias.accessed.info)) {
          ownAcc match {
            case acc: TermSymbol if !acc.isVariable && !isByNameParamType(acc.info) =>
              debuglog(s"$acc has alias ${alias.fullLocationString}")
              acc setAlias alias
            case _ =>
          }
        }
      }
    }
    superConstructorCalls.clear()
  }

}

trait TypersStats {
  self: TypesStats with Statistics =>
  val typedIdentCount     = newCounter("#typechecked identifiers")
  val typedSelectCount    = newCounter("#typechecked selections")
  val typedApplyCount     = newCounter("#typechecked applications")
  val subtypeFailed       = newSubCounter("  of which in failed", subtypeCount)
  val findMemberFailed    = newSubCounter("  of which in failed", findMemberCount)
  val failedSilentNanos   = newSubTimer("time spent in failed", typerNanos)
  val failedApplyNanos    = newSubTimer("  failed apply", typerNanos)
  val failedOpEqNanos     = newSubTimer("  failed op=", typerNanos)
  val isReferencedNanos   = newSubTimer("time spent ref scanning", typerNanos)
  val visitsByType        = newByClass("#visits by tree node", "typer")(newCounter(""))
  val byTypeNanos         = newByClass("time spent by tree node", "typer")(newStackableTimer("", typerNanos))
  val byTypeStack         = newTimerStack()
}
