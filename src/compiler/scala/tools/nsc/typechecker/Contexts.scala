/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ immutable, mutable }
import scala.annotation.tailrec
import scala.reflect.internal.util.shortClassOfInstance

/**
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Contexts { self: Analyzer =>
  import global._
  import definitions.{ JavaLangPackage, ScalaPackage, PredefModule }
  import ContextMode._

  object NoContext
    extends Context(EmptyTree, NoSymbol, EmptyScope, NoCompilationUnit,
                    null) { // We can't pass the uninitialized `this`. Instead, we treat null specially in `Context#outer`
    enclClass  = this
    enclMethod = this

    override val depth = 0
    override def nextEnclosing(p: Context => Boolean): Context = this
    override def enclosingContextChain: List[Context] = Nil
    override def implicitss: List[List[ImplicitInfo]] = Nil
    override def imports: List[ImportInfo] = Nil
    override def firstImport: Option[ImportInfo] = None
    override def toString = "NoContext"
  }
  private object RootImports {
    // Possible lists of root imports
    val javaList         = JavaLangPackage :: Nil
    val javaAndScalaList = JavaLangPackage :: ScalaPackage :: Nil
    val completeList     = JavaLangPackage :: ScalaPackage :: PredefModule :: Nil
  }

  def ambiguousImports(imp1: ImportInfo, imp2: ImportInfo) =
    LookupAmbiguous(s"it is imported twice in the same scope by\n$imp1\nand $imp2")
  def ambiguousDefnAndImport(owner: Symbol, imp: ImportInfo) =
    LookupAmbiguous(s"it is both defined in $owner and imported subsequently by \n$imp")

  private lazy val startContext = {
    NoContext.make(
    Template(List(), emptyValDef, List()) setSymbol global.NoSymbol setType global.NoType,
    rootMirror.RootClass,
    rootMirror.RootClass.info.decls)
  }

  private lazy val allUsedSelectors =
    mutable.Map[ImportInfo, Set[ImportSelector]]() withDefaultValue Set()
  private lazy val allImportInfos =
    mutable.Map[CompilationUnit, List[ImportInfo]]() withDefaultValue Nil

  def warnUnusedImports(unit: CompilationUnit) = {
    for (imps <- allImportInfos.remove(unit)) {
      for (imp <- imps.reverse.distinct) {
        val used = allUsedSelectors(imp)
        def isMask(s: ImportSelector) = s.name != nme.WILDCARD && s.rename == nme.WILDCARD

        imp.tree.selectors filterNot (s => isMask(s) || used(s)) foreach { sel =>
          unit.warning(imp posOf sel, "Unused import")
        }
      }
      allUsedSelectors --= imps
    }
  }

  var lastAccessCheckDetails: String = ""

  /** List of symbols to import from in a root context.  Typically that
   *  is `java.lang`, `scala`, and [[scala.Predef]], in that order.  Exceptions:
   *
   *  - if option `-Yno-imports` is given, nothing is imported
   *  - if the unit is java defined, only `java.lang` is imported
   *  - if option `-Yno-predef` is given, if the unit body has an import of Predef
   *    among its leading imports, or if the tree is [[scala.Predef]], `Predef` is not imported.
   */
  protected def rootImports(unit: CompilationUnit): List[Symbol] = {
    assert(definitions.isDefinitionsInitialized, "definitions uninitialized")

    if (settings.noimports) Nil
    else if (unit.isJava) RootImports.javaList
    else if (settings.nopredef || treeInfo.noPredefImportForUnit(unit.body)) {
      debuglog("Omitted import of Predef._ for " + unit)
      RootImports.javaAndScalaList
    }
    else RootImports.completeList
  }

  def rootContext(unit: CompilationUnit, tree: Tree = EmptyTree, erasedTypes: Boolean = false): Context = {
    val rootImportsContext = (startContext /: rootImports(unit))((c, sym) => c.make(gen.mkWildcardImport(sym)))
    val c = rootImportsContext.make(tree, unit = unit)
    if (erasedTypes) c.setThrowErrors() else c.setReportErrors()
    c(EnrichmentEnabled | ImplicitsEnabled) = !erasedTypes
    c
  }

  def resetContexts() {
    startContext.enclosingContextChain foreach { context =>
      context.tree match {
        case Import(qual, _) => qual setType singleType(qual.symbol.owner.thisType, qual.symbol)
        case _               =>
      }
      context.reportBuffer.clearAll()
    }
  }

  /**
   * A motley collection of the state and loosely associated behaviour of the type checker.
   * Each `Typer` has an associated context, and as it descends into the tree new `(Typer, Context)`
   * pairs are spawned.
   *
   * Meet the crew; first the state:
   *
   *   - A tree, symbol, and scope representing the focus of the typechecker
   *   - An enclosing context, `outer`.
   *   - The current compilation unit.
   *   - A variety of bits that track the current error reporting policy (more on this later);
   *     whether or not implicits/macros are enabled, whether we are in a self or super call or
   *     in a constructor suffix. These are represented as bits in the mask `contextMode`.
   *   - Some odds and ends: undetermined type pararameters of the current line of type inference;
   *     contextual augmentation for error messages, tracking of the nesting depth.
   *
   * And behaviour:
   *
   *   - The central point for issuing errors and warnings from the typechecker, with a means
   *     to buffer these for use in 'silent' type checking, when some recovery might be possible.
   *  -  `Context` is something of a Zipper for the tree were are typechecking: it `enclosingContextChain`
   *     is the path back to the root. This is exactly what we need to resolve names (`lookupSymbol`)
   *     and to collect in-scope implicit defintions (`implicitss`)
   *     Supporting these are `imports`, which represents all `Import` trees in in the enclosing context chain.
   *  -  In a similar vein, we can assess accessiblity (`isAccessible`.)
   *
   * More on error buffering:
   *     When are type errors recoverable? In quite a few places, it turns out. Some examples:
   *     trying to type an application with/without the expected type, or with/without implicit views
   *     enabled. This is usually mediated by `Typer.silent`, `Inferencer#tryTwice`.
   *
   *     Intially, starting from the `typer` phase, the contexts either buffer or report errors;
   *     afterwards errors are thrown. This is configured in `rootContext`. Additionally, more
   *     fine grained control is needed based on the kind of error; ambiguity errors are often
   *     suppressed during exploraratory typing, such as determining whether `a == b` in an argument
   *     position is an assignment or a named argument, when `Infererencer#isApplicableSafe` type checks
   *     applications with and without an expected type, or whtn `Typer#tryTypedApply` tries to fit arguments to
   *     a function type with/without implicit views.
   *
   *     When the error policies entails error/warning buffering, the mutable [[ReportBuffer]] records
   *     everything that is issued. It is important to note, that child Contexts created with `make`
   *     "inherit" the very same `ReportBuffer` instance, whereas children spawned through `makeSilent`
   *     receive an separate, fresh buffer.
   *
   * @param tree  Tree associated with this context
   * @param owner The current owner
   * @param scope The current scope
   * @param _outer The next outer context.
   */
  class Context private[typechecker](val tree: Tree, val owner: Symbol, val scope: Scope,
                                     val unit: CompilationUnit, _outer: Context) {
    private def outerIsNoContext = _outer eq null
    final def outer: Context = if (outerIsNoContext) NoContext else _outer

    /** The next outer context whose tree is a template or package definition */
    var enclClass: Context = _

    @inline private def savingEnclClass[A](c: Context)(a: => A): A = {
      val saved = enclClass
      enclClass = c
      try a finally enclClass = saved
    }

    /** A bitmask containing all the boolean flags in a context, e.g. are implicit views enabled */
    var contextMode: ContextMode = ContextMode.DefaultMode

    /** Update all modes in `mask` to `value` */
    def update(mask: ContextMode, value: Boolean) {
      contextMode = contextMode.set(value, mask)
    }

    /** Set all modes in the mask `enable` to true, and all in `disable` to false. */
    def set(enable: ContextMode = NOmode, disable: ContextMode = NOmode): this.type = {
      contextMode = contextMode.set(true, enable).set(false, disable)
      this
    }

    /** Is this context in all modes in the given `mask`? */
    def apply(mask: ContextMode): Boolean = contextMode.inAll(mask)

    /** The next outer context whose tree is a method */
    var enclMethod: Context = _

    /** Variance relative to enclosing class */
    var variance: Variance = Variance.Invariant

    private var _undetparams: List[Symbol] = List()

    protected def outerDepth = if (outerIsNoContext) 0 else outer.depth

    val depth: Int = {
      val increasesDepth = isRootImport || outerIsNoContext || (outer.scope != scope)
      ( if (increasesDepth) 1 else 0 ) + outerDepth
    }

    /** The currently visible imports */
    def imports: List[ImportInfo] = outer.imports
    /** Equivalent to `imports.headOption`, but more efficient */
    def firstImport: Option[ImportInfo] = outer.firstImport
    def isRootImport: Boolean = false

    /** Types for which implicit arguments are currently searched */
    var openImplicits: List[OpenImplicit] = List()

    /* For a named application block (`Tree`) the corresponding `NamedApplyInfo`. */
    var namedApplyBlockInfo: Option[(Tree, NamedApplyInfo)] = None
    var prefix: Type = NoPrefix

    def inSuperInit_=(value: Boolean)         = this(SuperInit) = value
    def inSuperInit                           = this(SuperInit)
    def inConstructorSuffix_=(value: Boolean) = this(ConstructorSuffix) = value
    def inConstructorSuffix                   = this(ConstructorSuffix)
    def inPatAlternative_=(value: Boolean)    = this(PatternAlternative) = value
    def inPatAlternative                      = this(PatternAlternative)
    def starPatterns_=(value: Boolean)        = this(StarPatterns) = value
    def starPatterns                          = this(StarPatterns)
    def returnsSeen_=(value: Boolean)         = this(ReturnsSeen) = value
    def returnsSeen                           = this(ReturnsSeen)
    def inSelfSuperCall_=(value: Boolean)     = this(SelfSuperCall) = value
    def inSelfSuperCall                       = this(SelfSuperCall)
    def implicitsEnabled_=(value: Boolean)    = this(ImplicitsEnabled) = value
    def implicitsEnabled                      = this(ImplicitsEnabled)
    def macrosEnabled_=(value: Boolean)       = this(MacrosEnabled) = value
    def macrosEnabled                         = this(MacrosEnabled)
    def enrichmentEnabled_=(value: Boolean)   = this(EnrichmentEnabled) = value
    def enrichmentEnabled                     = this(EnrichmentEnabled)
    def checking_=(value: Boolean)            = this(Checking) = value
    def checking                              = this(Checking)
    def retyping_=(value: Boolean)            = this(ReTyping) = value
    def retyping                              = this(ReTyping)
    def inSecondTry                           = this(SecondTry)
    def inSecondTry_=(value: Boolean)         = this(SecondTry) = value
    def inReturnExpr                          = this(ReturnExpr)
    def inTypeConstructorAllowed              = this(TypeConstructorAllowed)

    def defaultModeForTyped: Mode = if (inTypeConstructorAllowed) Mode.NOmode else Mode.EXPRmode

    /** These messages are printed when issuing an error */
    var diagnostic: List[String] = Nil

    /** Saved type bounds for type parameters which are narrowed in a GADT. */
    var savedTypeBounds: List[(Symbol, Type)] = List()

    /** Indentation level, in columns, for output under -Ytyper-debug */
    var typingIndentLevel: Int = 0
    def typingIndent = "  " * typingIndentLevel

    /** The next enclosing context (potentially `this`) that is owned by a class or method */
    def enclClassOrMethod: Context =
      if ((owner eq NoSymbol) || (owner.isClass) || (owner.isMethod)) this
      else outer.enclClassOrMethod

    /** The next enclosing context (potentially `this`) that has a `CaseDef` as a tree */
    def enclosingCaseDef = nextEnclosing(_.tree.isInstanceOf[CaseDef])

    /** ...or an Apply. */
    def enclosingApply = nextEnclosing(_.tree.isInstanceOf[Apply])

    //
    // Tracking undetermined type parameters for type argument inference.
    //
    def undetparamsString =
      if (undetparams.isEmpty) ""
      else undetparams.mkString("undetparams=", ", ", "")
    /** Undetermined type parameters. See `Infer#{inferExprInstance, adjustTypeArgs}`. Not inherited to child contexts */
    def undetparams: List[Symbol] = _undetparams
    def undetparams_=(ps: List[Symbol]) = { _undetparams = ps }

    /** Return and clear the undetermined type parameters */
    def extractUndetparams(): List[Symbol] = {
      val tparams = undetparams
      undetparams = List()
      tparams
    }

    /** Run `body` with this context with no undetermined type parameters, restore the original
     *  the original list afterwards.
     *  @param reportAmbiguous Should ambiguous errors be reported during evaluation of `body`?
     */
    def savingUndeterminedTypeParams[A](reportAmbiguous: Boolean = ambiguousErrors)(body: => A): A = {
      withMode() {
        this(AmbiguousErrors) = reportAmbiguous
        val saved = extractUndetparams()
        try body
        finally undetparams = saved
      }
    }

    //
    // Error reporting policies and buffer.
    //

    private var _reportBuffer: ReportBuffer = new ReportBuffer
    /** A buffer for errors and warnings, used with `this.bufferErrors == true` */
    def reportBuffer = _reportBuffer
    /** Discard the current report buffer, and replace with an empty one */
    def useFreshReportBuffer() = _reportBuffer = new ReportBuffer
    /** Discard the current report buffer, and replace with `other` */
    def restoreReportBuffer(other: ReportBuffer) = _reportBuffer = other

    /** The first error, if any, in the report buffer */
    def firstError: Option[AbsTypeError] = reportBuffer.firstError
    /** Does the report buffer contain any errors? */
    def hasErrors = reportBuffer.hasErrors

    def reportErrors    = this(ReportErrors)
    def bufferErrors    = this(BufferErrors)
    def ambiguousErrors = this(AmbiguousErrors)
    def throwErrors     = contextMode.inNone(ReportErrors | BufferErrors)

    def setReportErrors(): Unit                   = set(enable = ReportErrors | AmbiguousErrors, disable = BufferErrors)
    def setBufferErrors(): Unit                   = set(enable = BufferErrors, disable = ReportErrors | AmbiguousErrors)
    def setThrowErrors(): Unit                    = this(ReportErrors | AmbiguousErrors | BufferErrors) = false
    def setAmbiguousErrors(report: Boolean): Unit = this(AmbiguousErrors) = report

    /** Append the given errors to the report buffer */
    def updateBuffer(errors: Traversable[AbsTypeError]) = reportBuffer ++= errors
    /** Clear all errors from the report buffer */
    def flushBuffer() { reportBuffer.clearAllErrors() }
    /** Return and clear all errors from the report buffer */
    def flushAndReturnBuffer(): immutable.Seq[AbsTypeError] = {
      val current = reportBuffer.errors
      reportBuffer.clearAllErrors()
      current
    }

    /** Issue and clear all warnings from the report buffer */
    def flushAndIssueWarnings() {
      reportBuffer.warnings foreach {
        case (pos, msg) => unit.warning(pos, msg)
      }
      reportBuffer.clearAllWarnings()
    }

    //
    // Temporary mode adjustment
    //

    @inline def withMode[T](enabled: ContextMode = NOmode, disabled: ContextMode = NOmode)(op: => T): T = {
      val saved = contextMode
      set(enabled, disabled)
      try op
      finally contextMode = saved
    }

    @inline final def withImplicitsEnabled[T](op: => T): T                 = withMode(enabled = ImplicitsEnabled)(op)
    @inline final def withImplicitsDisabled[T](op: => T): T                = withMode(disabled = ImplicitsEnabled | EnrichmentEnabled)(op)
    @inline final def withImplicitsDisabledAllowEnrichment[T](op: => T): T = withMode(enabled = EnrichmentEnabled, disabled = ImplicitsEnabled)(op)
    @inline final def withMacrosEnabled[T](op: => T): T                    = withMode(enabled = MacrosEnabled)(op)
    @inline final def withMacrosDisabled[T](op: => T): T                   = withMode(disabled = MacrosEnabled)(op)
    @inline final def withinStarPatterns[T](op: => T): T                   = withMode(enabled = StarPatterns)(op)
    @inline final def withinSuperInit[T](op: => T): T                      = withMode(enabled = SuperInit)(op)
    @inline final def withinSecondTry[T](op: => T): T                      = withMode(enabled = SecondTry)(op)
    @inline final def withinPatAlternative[T](op: => T): T                 = withMode(enabled = PatternAlternative)(op)

    /** TypeConstructorAllowed is enabled when we are typing a higher-kinded type.
     *  adapt should then check kind-arity based on the prototypical type's kind
     *  arity. Type arguments should not be inferred.
     */
    @inline final def withinTypeConstructorAllowed[T](op: => T): T = withMode(enabled = TypeConstructorAllowed)(op)

    /* TODO - consolidate returnsSeen (which seems only to be used by checkDead)
     * and ReturnExpr.
     */
    @inline final def withinReturnExpr[T](op: => T): T = {
      enclMethod.returnsSeen = true
      withMode(enabled = ReturnExpr)(op)
    }

    // See comment on FormerNonStickyModes.
    @inline final def withOnlyStickyModes[T](op: => T): T = withMode(disabled = FormerNonStickyModes)(op)

    /** @return true if the `expr` evaluates to true within a silent Context that incurs no errors */
    @inline final def inSilentMode(expr: => Boolean): Boolean = {
      withMode() { // withMode with no arguments to restore the mode mutated by `setBufferErrors`.
        setBufferErrors()
        try expr && !hasErrors
        finally reportBuffer.clearAll()
      }
    }

    //
    // Child Context Creation
    //

    /**
     * Construct a child context. The parent and child will share the report buffer.
     * Compare with `makeSilent`, in which the child has a fresh report buffer.
     *
     * If `tree` is an `Import`, that import will be avaiable at the head of
     * `Context#imports`.
     */
    def make(tree: Tree = tree, owner: Symbol = owner,
             scope: Scope = scope, unit: CompilationUnit = unit): Context = {
      val isTemplateOrPackage = tree match {
        case _: Template | _: PackageDef => true
        case _                           => false
      }
      val isDefDef = tree match {
        case _: DefDef => true
        case _         => false
      }
      val isImport = tree match {
        case _: Import => true
        case _         => false
      }
      val sameOwner = owner == this.owner
      val prefixInChild =
        if (isTemplateOrPackage) owner.thisType
        else if (!sameOwner && owner.isTerm) NoPrefix
        else prefix

      // The blank canvas
      val c = if (isImport)
        new Context(tree, owner, scope, unit, this) with ImportContext
      else
        new Context(tree, owner, scope, unit, this)

      // Fields that are directly propagated
      c.variance           = variance
      c.diagnostic         = diagnostic
      c.typingIndentLevel  = typingIndentLevel
      c.openImplicits      = openImplicits
      c.contextMode        = contextMode // note: ConstructorSuffix, a bit within `mode`, is conditionally overwritten below.
      c._reportBuffer      = reportBuffer

      // Fields that may take on a different value in the child
      c.prefix             = prefixInChild
      c.enclClass          = if (isTemplateOrPackage) c else enclClass
      c(ConstructorSuffix) = !isTemplateOrPackage && c(ConstructorSuffix)
      c.enclMethod         = if (isDefDef) c else enclMethod

      registerContext(c.asInstanceOf[analyzer.Context])
      debuglog("[context] ++ " + c.unit + " / " + tree.summaryString)
      c
    }

    def make(tree: Tree, owner: Symbol, scope: Scope): Context =
      // TODO SI-7345 Moving this optimization into the main overload of `make` causes all tests to fail.
      //              even if it is extened to check that `unit == this.unit`. Why is this?
      if (tree == this.tree && owner == this.owner && scope == this.scope) this
      else make(tree, owner, scope, unit)

    /** Make a child context that represents a new nested scope */
    def makeNewScope(tree: Tree, owner: Symbol): Context =
      make(tree, owner, newNestedScope(scope))

    /** Make a child context that buffers errors and warnings into a fresh report buffer. */
    def makeSilent(reportAmbiguousErrors: Boolean = ambiguousErrors, newtree: Tree = tree): Context = {
      val c = make(newtree)
      c.setBufferErrors()
      c.setAmbiguousErrors(reportAmbiguousErrors)
      c._reportBuffer = new ReportBuffer // A fresh buffer so as not to leak errors/warnings into `this`.
      c
    }

    /** Make a silent child context does not allow implicits. Used to prevent chaining of implicit views. */
    def makeImplicit(reportAmbiguousErrors: Boolean) = {
      val c = makeSilent(reportAmbiguousErrors)
      c(ImplicitsEnabled | EnrichmentEnabled) = false
      c
    }

    /**
     * A context for typing constructor parameter ValDefs, super or self invocation arguments and default getters
     * of constructors. These expressions need to be type checked in a scope outside the class, cf. spec 5.3.1.
     *
     * This method is called by namer / typer where `this` is the context for the constructor DefDef. The
     * owner of the resulting (new) context is the outer context for the Template, i.e. the context for the
     * ClassDef. This means that class type parameters will be in scope. The value parameters of the current
     * constructor are also entered into the new constructor scope. Members of the class however will not be
     * accessible.
     */
    def makeConstructorContext = {
      val baseContext = enclClass.outer.nextEnclosing(!_.tree.isInstanceOf[Template])
      val argContext = baseContext.makeNewScope(tree, owner)
      argContext.contextMode = contextMode
      argContext.inSelfSuperCall = true
      def enterElems(c: Context) {
        def enterLocalElems(e: ScopeEntry) {
          if (e != null && e.owner == c.scope) {
            enterLocalElems(e.next)
            argContext.scope enter e.sym
          }
        }
        if (c.isLocal && !c.owner.isLocalDummy) {
          enterElems(c.outer)
          enterLocalElems(c.scope.elems)
        }
      }
      // Enter the scope elements of this (the scope for the constructor DefDef) into the new constructor scope.
      // Concretely, this will enter the value parameters of constructor.
      enterElems(this)
      argContext
    }

    //
    // Error and warning issuance
    //

    private def addDiagString(msg: String) = {
      val ds =
        if (diagnostic.isEmpty) ""
        else diagnostic.mkString("\n","\n", "")
      if (msg endsWith ds) msg else msg + ds
    }

    private def unitError(pos: Position, msg: String) =
      unit.error(pos, if (checking) "\n**** ERROR DURING INTERNAL CHECKING ****\n" + msg else msg)

    @inline private def issueCommon(err: AbsTypeError)(pf: PartialFunction[AbsTypeError, Unit]) {
      if (settings.Yissuedebug) {
        log("issue error: " + err.errMsg)
        (new Exception).printStackTrace()
      }
      if (pf isDefinedAt err) pf(err)
      else if (bufferErrors) { reportBuffer += err }
      else throw new TypeError(err.errPos, err.errMsg)
    }

    /** Issue/buffer/throw the given type error according to the current mode for error reporting. */
    def issue(err: AbsTypeError) {
      issueCommon(err) { case _ if reportErrors =>
        unitError(err.errPos, addDiagString(err.errMsg))
      }
    }

    /** Issue/buffer/throw the given implicit ambiguity error according to the current mode for error reporting. */
    def issueAmbiguousError(pre: Type, sym1: Symbol, sym2: Symbol, err: AbsTypeError) {
      issueCommon(err) { case _ if ambiguousErrors =>
        if (!pre.isErroneous && !sym1.isErroneous && !sym2.isErroneous)
          unitError(err.errPos, err.errMsg)
      }
    }

    /** Issue/buffer/throw the given implicit ambiguity error according to the current mode for error reporting. */
    def issueAmbiguousError(err: AbsTypeError) {
      issueCommon(err) { case _ if ambiguousErrors => unitError(err.errPos, addDiagString(err.errMsg)) }
    }

    /** Issue/throw the given `err` according to the current mode for error reporting. */
    def error(pos: Position, err: Throwable) =
      if (reportErrors) unitError(pos, addDiagString(err.getMessage()))
      else throw err

    /** Issue/throw the given error message according to the current mode for error reporting. */
    def error(pos: Position, msg: String) = {
      val msg1 = addDiagString(msg)
      if (reportErrors) unitError(pos, msg1)
      else throw new TypeError(pos, msg1)
    }

    /** Issue/throw the given error message according to the current mode for error reporting. */
    def warning(pos: Position, msg: String, force: Boolean = false) {
      if (reportErrors || force) unit.warning(pos, msg)
      else if (bufferErrors) reportBuffer += (pos -> msg)
    }

    /** Is the owning symbol of this context a term? */
    final def isLocal: Boolean = owner.isTerm

    // nextOuter determines which context is searched next for implicits
    // (after `this`, which contributes `newImplicits` below.) In
    // most cases, it is simply the outer context: if we're owned by
    // a constructor, the actual current context and the conceptual
    // context are different when it comes to scoping. The current
    // conceptual scope is the context enclosing the blocks which
    // represent the constructor body (TODO: why is there more than one
    // such block in the outer chain?)
    private def nextOuter = {
      // Drop the constructor body blocks, which come in varying numbers.
      // -- If the first statement is in the constructor, scopingCtx == (constructor definition)
      // -- Otherwise, scopingCtx == (the class which contains the constructor)
      val scopingCtx =
        if (owner.isConstructor) nextEnclosing(c => !c.tree.isInstanceOf[Block])
        else this

      scopingCtx.outer
    }

    def nextEnclosing(p: Context => Boolean): Context =
      if (p(this)) this else outer.nextEnclosing(p)

    def enclosingContextChain: List[Context] = this :: outer.enclosingContextChain

    private def treeTruncated       = tree.toString.replaceAll("\\s+", " ").lines.mkString("\\n").take(70)
    private def treeIdString        = if (settings.uniqid.value) "#" + System.identityHashCode(tree).toString.takeRight(3) else ""
    private def treeString          = tree match {
      case x: Import => "" + x
      case Template(parents, `emptyValDef`, body) =>
        val pstr = if ((parents eq null) || parents.isEmpty) "Nil" else parents mkString " "
        val bstr = if (body eq null) "" else body.length + " stats"
        s"""Template($pstr, _, $bstr)"""
      case x => s"${tree.shortClass}${treeIdString}:${treeTruncated}"
    }

    override def toString =
      sm"""|Context($unit) {
           |   owner       = $owner
           |   tree        = $treeString
           |   scope       = ${scope.size} decls
           |   contextMode = $contextMode
           |   outer.owner = ${outer.owner}
           |}"""

    //
    // Accessibility checking
    //

    /** Is `sub` a subclass of `base` or a companion object of such a subclass? */
    private def isSubClassOrCompanion(sub: Symbol, base: Symbol) =
      sub.isNonBottomSubClass(base) ||
    sub.isModuleClass && sub.linkedClassOfClass.isNonBottomSubClass(base)

    /** Return the closest enclosing context that defines a subclass of `clazz`
     *  or a companion object thereof, or `NoContext` if no such context exists.
     */
    def enclosingSubClassContext(clazz: Symbol): Context = {
      var c = this.enclClass
      while (c != NoContext && !isSubClassOrCompanion(c.owner, clazz))
        c = c.outer.enclClass
      c
    }

    /** Is `sym` accessible as a member of `pre` in current context? */
    def isAccessible(sym: Symbol, pre: Type, superAccess: Boolean = false): Boolean = {
      lastAccessCheckDetails = ""
      // Console.println("isAccessible(%s, %s, %s)".format(sym, pre, superAccess))

      def accessWithinLinked(ab: Symbol) = {
        val linked = ab.linkedClassOfClass
        // don't have access if there is no linked class
        // (before adding the `ne NoSymbol` check, this was a no-op when linked eq NoSymbol,
        //  since `accessWithin(NoSymbol) == true` whatever the symbol)
        (linked ne NoSymbol) && accessWithin(linked)
      }

      /* Are we inside definition of `ab`? */
      def accessWithin(ab: Symbol) = {
        // #3663: we must disregard package nesting if sym isJavaDefined
        if (sym.isJavaDefined) {
          // is `o` or one of its transitive owners equal to `ab`?
          // stops at first package, since further owners can only be surrounding packages
          @tailrec def abEnclosesStopAtPkg(o: Symbol): Boolean =
            (o eq ab) || (!o.isPackageClass && (o ne NoSymbol) && abEnclosesStopAtPkg(o.owner))
          abEnclosesStopAtPkg(owner)
        } else (owner hasTransOwner ab)
      }

      def isSubThisType(pre: Type, clazz: Symbol): Boolean = pre match {
        case ThisType(pclazz) => pclazz isNonBottomSubClass clazz
        case _ => false
      }

      /* Is protected access to target symbol permitted */
      def isProtectedAccessOK(target: Symbol) = {
        val c = enclosingSubClassContext(sym.owner)
        if (c == NoContext)
          lastAccessCheckDetails =
            "\n Access to protected "+target+" not permitted because"+
            "\n "+"enclosing "+this.enclClass.owner+
            this.enclClass.owner.locationString+" is not a subclass of "+
            "\n "+sym.owner+sym.owner.locationString+" where target is defined"
        c != NoContext &&
        {
          target.isType || { // allow accesses to types from arbitrary subclasses fixes #4737
            val res =
              isSubClassOrCompanion(pre.widen.typeSymbol, c.owner) ||
              c.owner.isModuleClass &&
              isSubClassOrCompanion(pre.widen.typeSymbol, c.owner.linkedClassOfClass)
            if (!res)
              lastAccessCheckDetails =
                "\n Access to protected "+target+" not permitted because"+
                "\n prefix type "+pre.widen+" does not conform to"+
                "\n "+c.owner+c.owner.locationString+" where the access take place"
              res
          }
        }
      }

      (pre == NoPrefix) || {
        val ab = sym.accessBoundary(sym.owner)

        (  (ab.isTerm || ab == rootMirror.RootClass)
        || (accessWithin(ab) || accessWithinLinked(ab)) &&
             (  !sym.hasLocalFlag
             || sym.owner.isImplClass // allow private local accesses to impl classes
             || sym.isProtected && isSubThisType(pre, sym.owner)
             || pre =:= sym.owner.thisType
             )
        || sym.isProtected &&
             (  superAccess
             || pre.isInstanceOf[ThisType]
             || phase.erasedTypes
             || (sym.overrideChain exists isProtectedAccessOK)
                // that last condition makes protected access via self types work.
             )
        )
        // note: phase.erasedTypes disables last test, because after addinterfaces
        // implementation classes are not in the superclass chain. If we enable the
        // test, bug780 fails.
      }
    }

    //
    // Type bound management
    //

    def pushTypeBounds(sym: Symbol) {
      sym.info match {
        case tb: TypeBounds => if (!tb.isEmptyBounds) log(s"Saving $sym info=$tb")
        case info           => devWarning(s"Something other than a TypeBounds seen in pushTypeBounds: $info is a ${shortClassOfInstance(info)}")
      }
      savedTypeBounds ::= ((sym, sym.info))
    }

    def restoreTypeBounds(tp: Type): Type = {
      def restore(): Type = savedTypeBounds.foldLeft(tp) { case (current, (sym, savedInfo)) =>
        def bounds_s(tb: TypeBounds) = if (tb.isEmptyBounds) "<empty bounds>" else s"TypeBounds(lo=${tb.lo}, hi=${tb.hi})"
        //@M TODO: when higher-kinded types are inferred, probably need a case PolyType(_, TypeBounds(...)) if ... =>
        val TypeBounds(lo, hi) = sym.info.bounds
        val isUnique           = lo <:< hi && hi <:< lo
        val isPresent          = current contains sym
        def saved_s            = bounds_s(savedInfo.bounds)
        def current_s          = bounds_s(sym.info.bounds)

        if (isUnique && isPresent)
          devWarningResult(s"Preserving inference: ${sym.nameString}=$hi in $current (based on $current_s) before restoring $sym to saved $saved_s")(
            current.instantiateTypeParams(List(sym), List(hi))
          )
        else if (isPresent)
          devWarningResult(s"Discarding inferred $current_s because it does not uniquely determine $sym in")(current)
        else
          logResult(s"Discarding inferred $current_s because $sym does not appear in")(current)
      }
      try restore()
      finally {
        for ((sym, savedInfo) <- savedTypeBounds)
          sym setInfo debuglogResult(s"Discarding inferred $sym=${sym.info}, restoring saved info")(savedInfo)

        savedTypeBounds = Nil
      }
    }

    //
    // Implicit collection
    //

    private var implicitsCache: List[List[ImplicitInfo]] = null
    private var implicitsRunId = NoRunId

    def resetCache() {
      implicitsRunId = NoRunId
      implicitsCache = null
      if (outer != null && outer != this) outer.resetCache()
    }

    /** A symbol `sym` qualifies as an implicit if it has the IMPLICIT flag set,
     *  it is accessible, and if it is imported there is not already a local symbol
     *  with the same names. Local symbols override imported ones. This fixes #2866.
     */
    private def isQualifyingImplicit(name: Name, sym: Symbol, pre: Type, imported: Boolean) =
      sym.isImplicit &&
      isAccessible(sym, pre) &&
      !(imported && {
        val e = scope.lookupEntry(name)
        (e ne null) && (e.owner == scope)
      })

    private def collectImplicits(syms: Scope, pre: Type, imported: Boolean = false): List[ImplicitInfo] =
      for (sym <- syms.toList if isQualifyingImplicit(sym.name, sym, pre, imported)) yield
        new ImplicitInfo(sym.name, pre, sym)

    private def collectImplicitImports(imp: ImportInfo): List[ImplicitInfo] = {
      val qual = imp.qual

      val pre =
        if (qual.tpe.typeSymbol.isPackageClass)
          // SI-6225 important if the imported symbol is inherited by the the package object.
          singleType(qual.tpe, qual.tpe member nme.PACKAGE)
        else
          qual.tpe
      def collect(sels: List[ImportSelector]): List[ImplicitInfo] = sels match {
        case List() =>
          List()
        case List(ImportSelector(nme.WILDCARD, _, _, _)) =>
          collectImplicits(pre.implicitMembers, pre, imported = true)
        case ImportSelector(from, _, to, _) :: sels1 =>
          var impls = collect(sels1) filter (info => info.name != from)
          if (to != nme.WILDCARD) {
            for (sym <- importedAccessibleSymbol(imp, to).alternatives)
              if (isQualifyingImplicit(to, sym, pre, imported = true))
                impls = new ImplicitInfo(to, pre, sym) :: impls
          }
          impls
      }
      //debuglog("collect implicit imports " + imp + "=" + collect(imp.tree.selectors))//DEBUG
      collect(imp.tree.selectors)
    }

    /* SI-5892 / SI-4270: `implicitss` can return results which are not accessible at the
     * point where implicit search is triggered. Example: implicits in (annotations of)
     * class type parameters (SI-5892). The `context.owner` is the class symbol, therefore
     * `implicitss` will return implicit conversions defined inside the class. These are
     * filtered out later by `eligibleInfos` (SI-4270 / 9129cfe9), as they don't type-check.
     */
    def implicitss: List[List[ImplicitInfo]] = {
      val imports = this.imports
      val nextOuter = this.nextOuter
      if (implicitsRunId != currentRunId) {
        implicitsRunId = currentRunId
        implicitsCache = List()
        val newImplicits: List[ImplicitInfo] =
          if (owner != nextOuter.owner && owner.isClass && !owner.isPackageClass && !inSelfSuperCall) {
            if (!owner.isInitialized) return nextOuter.implicitss
            // debuglog("collect member implicits " + owner + ", implicit members = " + owner.thisType.implicitMembers)//DEBUG
            savingEnclClass(this) {
              // !!! In the body of `class C(implicit a: A) { }`, `implicitss` returns `List(List(a), List(a), List(<predef..)))`
              //     it handled correctly by implicit search, which considers the second `a` to be shadowed, but should be
              //     remedied nonetheless.
              collectImplicits(owner.thisType.implicitMembers, owner.thisType)
            }
          } else if (scope != nextOuter.scope && !owner.isPackageClass) {
            debuglog("collect local implicits " + scope.toList)//DEBUG
            collectImplicits(scope, NoPrefix)
          } else if (firstImport != nextOuter.firstImport) {
            assert(imports.tail.headOption == nextOuter.firstImport, (imports, nextOuter.imports))
            collectImplicitImports(imports.head)
          } else if (owner.isPackageClass) {
            // the corresponding package object may contain implicit members.
            collectImplicits(owner.tpe.implicitMembers, owner.tpe)
          } else List()
        implicitsCache = if (newImplicits.isEmpty) nextOuter.implicitss
                         else newImplicits :: nextOuter.implicitss
      }
      implicitsCache
    }

    //
    // Imports and symbol lookup
    //

    /** It's possible that seemingly conflicting identifiers are
     *  identifiably the same after type normalization.  In such cases,
     *  allow compilation to proceed.  A typical example is:
     *    package object foo { type InputStream = java.io.InputStream }
     *    import foo._, java.io._
     */
    private def resolveAmbiguousImport(name: Name, imp1: ImportInfo, imp2: ImportInfo): Option[ImportInfo] = {
      val imp1Explicit = imp1 isExplicitImport name
      val imp2Explicit = imp2 isExplicitImport name
      val ambiguous    = if (imp1.depth == imp2.depth) imp1Explicit == imp2Explicit else !imp1Explicit && imp2Explicit
      val imp1Symbol   = (imp1 importedSymbol name).initialize filter (s => isAccessible(s, imp1.qual.tpe, superAccess = false))
      val imp2Symbol   = (imp2 importedSymbol name).initialize filter (s => isAccessible(s, imp2.qual.tpe, superAccess = false))

      // The types of the qualifiers from which the ambiguous imports come.
      // If the ambiguous name is a value, these must be the same.
      def t1 = imp1.qual.tpe
      def t2 = imp2.qual.tpe
      // The types of the ambiguous symbols, seen as members of their qualifiers.
      // If the ambiguous name is a monomorphic type, we can relax this far.
      def mt1 = t1 memberType imp1Symbol
      def mt2 = t2 memberType imp2Symbol

      def characterize = List(
        s"types:  $t1 =:= $t2  ${t1 =:= t2}  members: ${mt1 =:= mt2}",
        s"member type 1: $mt1",
        s"member type 2: $mt2"
      ).mkString("\n  ")

      if (!ambiguous || !imp2Symbol.exists) Some(imp1)
      else if (!imp1Symbol.exists) Some(imp2)
      else (
        // The symbol names are checked rather than the symbols themselves because
        // each time an overloaded member is looked up it receives a new symbol.
        // So foo.member("x") != foo.member("x") if x is overloaded.  This seems
        // likely to be the cause of other bugs too...
        if (t1 =:= t2 && imp1Symbol.name == imp2Symbol.name) {
          log(s"Suppressing ambiguous import: $t1 =:= $t2 && $imp1Symbol == $imp2Symbol")
          Some(imp1)
        }
        // Monomorphism restriction on types is in part because type aliases could have the
        // same target type but attach different variance to the parameters. Maybe it can be
        // relaxed, but doesn't seem worth it at present.
        else if (mt1 =:= mt2 && name.isTypeName && imp1Symbol.isMonomorphicType && imp2Symbol.isMonomorphicType) {
          log(s"Suppressing ambiguous import: $mt1 =:= $mt2 && $imp1Symbol and $imp2Symbol are equivalent")
          Some(imp1)
        }
        else {
          log(s"Import is genuinely ambiguous:\n  " + characterize)
          None
        }
      )
    }

    /** The symbol with name `name` imported via the import in `imp`,
     *  if any such symbol is accessible from this context.
     */
    def importedAccessibleSymbol(imp: ImportInfo, name: Name): Symbol =
      importedAccessibleSymbol(imp, name, requireExplicit = false)

    private def importedAccessibleSymbol(imp: ImportInfo, name: Name, requireExplicit: Boolean): Symbol =
      imp.importedSymbol(name, requireExplicit) filter (s => isAccessible(s, imp.qual.tpe, superAccess = false))

    /** Is `sym` defined in package object of package `pkg`?
     *  Since sym may be defined in some parent of the package object,
     *  we cannot inspect its owner only; we have to go through the
     *  info of the package object.  However to avoid cycles we'll check
     *  what other ways we can before pushing that way.
     */
    def isInPackageObject(sym: Symbol, pkg: Symbol): Boolean = {
      def uninitialized(what: String) = {
        log(s"Cannot look for $sym in package object of $pkg; $what is not initialized.")
        false
      }
      def pkgClass = if (pkg.isTerm) pkg.moduleClass else pkg
      def matchesInfo = (
        // need to be careful here to not get a cyclic reference during bootstrap
        if (pkg.isInitialized) {
          val module = pkg.info member nme.PACKAGEkw
          if (module.isInitialized)
            module.info.member(sym.name).alternatives contains sym
          else
            uninitialized("" + module)
        }
        else uninitialized("" + pkg)
      )
      def inPackageObject(sym: Symbol) = (
        // To be in the package object, one of these must be true:
        //   1) sym.owner is a package object class, and sym.owner.owner is the package class for `pkg`
        //   2) sym.owner is inherited by the correct package object class
        // We try to establish 1) by inspecting the owners directly, and then we try
        // to rule out 2), and only if both those fail do we resort to looking in the info.
        !sym.isPackage && (sym.owner ne NoSymbol) && (
          if (sym.owner.isPackageObjectClass)
            sym.owner.owner == pkgClass
          else
            !sym.owner.isPackageClass && matchesInfo
        )
      )

      // An overloaded symbol might not have the expected owner!
      // The alternatives must be inspected directly.
      pkgClass.isPackageClass && (
        if (sym.isOverloaded)
          sym.alternatives forall (isInPackageObject(_, pkg))
        else
          inPackageObject(sym)
      )
    }

    def isNameInScope(name: Name) = lookupSymbol(name, _ => true).isSuccess

    /** Find the symbol of a simple name starting from this context.
     *  All names are filtered through the "qualifies" predicate,
     *  the search continuing as long as no qualifying name is found.
     */
    def lookupSymbol(name: Name, qualifies: Symbol => Boolean): NameLookup = {
      var lookupError: NameLookup  = null       // set to non-null if a definite error is encountered
      var inaccessible: NameLookup = null       // records inaccessible symbol for error reporting in case none is found
      var defSym: Symbol           = NoSymbol   // the directly found symbol
      var pre: Type                = NoPrefix   // the prefix type of defSym, if a class member
      var cx: Context              = this       // the context under consideration
      var symbolDepth: Int         = -1         // the depth of the directly found symbol

      def finish(qual: Tree, sym: Symbol): NameLookup = (
        if (lookupError ne null) lookupError
        else sym match {
          case NoSymbol if inaccessible ne null => inaccessible
          case NoSymbol                         => LookupNotFound
          case _                                => LookupSucceeded(qual, sym)
        }
      )
      def finishDefSym(sym: Symbol, pre0: Type): NameLookup =
        if (requiresQualifier(sym))
          finish(gen.mkAttributedQualifier(pre0), sym)
        else
          finish(EmptyTree, sym)

      def isPackageOwnedInDifferentUnit(s: Symbol) = (
        s.isDefinedInPackage && (
             !currentRun.compiles(s)
          || unit.exists && s.sourceFile != unit.source.file
        )
      )
      def requiresQualifier(s: Symbol) = (
           s.owner.isClass
        && !s.owner.isPackageClass
        && !s.isTypeParameterOrSkolem
      )
      def lookupInPrefix(name: Name)    = pre member name filter qualifies
      def accessibleInPrefix(s: Symbol) = isAccessible(s, pre, superAccess = false)

      def searchPrefix = {
        cx = cx.enclClass
        val found0 = lookupInPrefix(name)
        val found1 = found0 filter accessibleInPrefix
        if (found0.exists && !found1.exists && inaccessible == null)
          inaccessible = LookupInaccessible(found0, analyzer.lastAccessCheckDetails)

        found1
      }

      def lookupInScope(scope: Scope) =
        (scope lookupUnshadowedEntries name filter (e => qualifies(e.sym))).toList

      def newOverloaded(owner: Symbol, pre: Type, entries: List[ScopeEntry]) =
        logResult(s"!!! lookup overloaded")(owner.newOverloaded(pre, entries map (_.sym)))

      // Constructor lookup should only look in the decls of the enclosing class
      // not in the self-type, nor in the enclosing context, nor in imports (SI-4460, SI-6745)
      if (name == nme.CONSTRUCTOR) return {
        val enclClassSym = cx.enclClass.owner
        val scope = cx.enclClass.prefix.baseType(enclClassSym).decls
        val constructorSym = lookupInScope(scope) match {
          case Nil       => NoSymbol
          case hd :: Nil => hd.sym
          case entries   => newOverloaded(enclClassSym, cx.enclClass.prefix, entries)
        }
        finishDefSym(constructorSym, cx.enclClass.prefix)
      }

      // cx.scope eq null arises during FixInvalidSyms in Duplicators
      while (defSym == NoSymbol && (cx ne NoContext) && (cx.scope ne null)) {
        pre    = cx.enclClass.prefix
        defSym = lookupInScope(cx.scope) match {
          case Nil                  => searchPrefix
          case entries @ (hd :: tl) =>
            // we have a winner: record the symbol depth
            symbolDepth = (cx.depth - cx.scope.nestingLevel) + hd.depth
            if (tl.isEmpty) hd.sym
            else newOverloaded(cx.owner, pre, entries)
        }
        if (!defSym.exists)
          cx = cx.outer // push further outward
      }
      if (symbolDepth < 0)
        symbolDepth = cx.depth

      var impSym: Symbol = NoSymbol
      var imports        = Context.this.imports
      def imp1           = imports.head
      def imp2           = imports.tail.head
      def sameDepth      = imp1.depth == imp2.depth
      def imp1Explicit   = imp1 isExplicitImport name
      def imp2Explicit   = imp2 isExplicitImport name

      def lookupImport(imp: ImportInfo, requireExplicit: Boolean) =
        importedAccessibleSymbol(imp, name, requireExplicit) filter qualifies

      // Java: A single-type-import declaration d in a compilation unit c of package p
      // that imports a type named n shadows, throughout c, the declarations of:
      //
      //  1) any top level type named n declared in another compilation unit of p
      //
      // A type-import-on-demand declaration never causes any other declaration to be shadowed.
      //
      // Scala: Bindings of different kinds have a precedence deﬁned on them:
      //
      //  1) Deﬁnitions and declarations that are local, inherited, or made available by a
      //     package clause in the same compilation unit where the deﬁnition occurs have
      //     highest precedence.
      //  2) Explicit imports have next highest precedence.
      def depthOk(imp: ImportInfo) = (
           imp.depth > symbolDepth
        || (unit.isJava && imp.isExplicitImport(name) && imp.depth == symbolDepth)
      )

      while (!impSym.exists && imports.nonEmpty && depthOk(imports.head)) {
        impSym = lookupImport(imp1, requireExplicit = false)
        if (!impSym.exists)
          imports = imports.tail
      }

      if (defSym.exists && impSym.exists) {
        // imported symbols take precedence over package-owned symbols in different compilation units.
        if (isPackageOwnedInDifferentUnit(defSym))
          defSym = NoSymbol
        // Defined symbols take precedence over erroneous imports.
        else if (impSym.isError || impSym.name == nme.CONSTRUCTOR)
          impSym = NoSymbol
        // Otherwise they are irreconcilably ambiguous
        else
          return ambiguousDefnAndImport(defSym.owner, imp1)
      }

      // At this point only one or the other of defSym and impSym might be set.
      if (defSym.exists)
        finishDefSym(defSym, pre)
      else if (impSym.exists) {
        // We continue walking down the imports as long as the tail is non-empty, which gives us:
        //   imports  ==  imp1 :: imp2 :: _
        // And at least one of the following is true:
        //   - imp1 and imp2 are at the same depth
        //   - imp1 is a wildcard import, so all explicit imports from outer scopes must be checked
        def keepLooking = (
             lookupError == null
          && imports.tail.nonEmpty
          && (sameDepth || !imp1Explicit)
        )
        // If we find a competitor imp2 which imports the same name, possible outcomes are:
        //
        //  - same depth, imp1 wild, imp2 explicit:        imp2 wins, drop imp1
        //  - same depth, imp1 wild, imp2 wild:            ambiguity check
        //  - same depth, imp1 explicit, imp2 explicit:    ambiguity check
        //  - differing depth, imp1 wild, imp2 explicit:   ambiguity check
        //  - all others:                                  imp1 wins, drop imp2
        //
        // The ambiguity check is: if we can verify that both imports refer to the same
        // symbol (e.g. import foo.X followed by import foo._) then we discard imp2
        // and proceed. If we cannot, issue an ambiguity error.
        while (keepLooking) {
          // If not at the same depth, limit the lookup to explicit imports.
          // This is desirable from a performance standpoint (compare to
          // filtering after the fact) but also necessary to keep the unused
          // import check from being misled by symbol lookups which are not
          // actually used.
          val other = lookupImport(imp2, requireExplicit = !sameDepth)
          def imp1wins() = { imports = imp1 :: imports.tail.tail }
          def imp2wins() = { impSym = other ; imports = imports.tail }

          if (!other.exists) // imp1 wins; drop imp2 and continue.
            imp1wins()
          else if (sameDepth && !imp1Explicit && imp2Explicit) // imp2 wins; drop imp1 and continue.
            imp2wins()
          else resolveAmbiguousImport(name, imp1, imp2) match {
            case Some(imp) => if (imp eq imp1) imp1wins() else imp2wins()
            case _         => lookupError = ambiguousImports(imp1, imp2)
          }
        }
        // optimization: don't write out package prefixes
        finish(resetPos(imp1.qual.duplicate), impSym)
      }
      else finish(EmptyTree, NoSymbol)
    }

    /**
     * Find a symbol in this context or one of its outers.
     *
     * Used to find symbols are owned by methods (or fields), they can't be
     * found in some scope.
     *
     * Examples: companion module of classes owned by a method, default getter
     * methods of nested methods. See NamesDefaults.scala
     */
    def lookup(name: Name, expectedOwner: Symbol) = {
      var res: Symbol = NoSymbol
      var ctx = this
      while (res == NoSymbol && ctx.outer != ctx) {
        val s = ctx.scope lookup name
        if (s != NoSymbol && s.owner == expectedOwner)
          res = s
        else
          ctx = ctx.outer
      }
      res
    }
  } //class Context

  /** A `Context` focussed on an `Import` tree */
  trait ImportContext extends Context {
    private val impInfo: ImportInfo = {
      val info = new ImportInfo(tree.asInstanceOf[Import], outerDepth)
      if (settings.lint && !isRootImport) // excludes java.lang/scala/Predef imports
        allImportInfos(unit) ::= info
      info
    }
    override final def imports      = impInfo :: super.imports
    override final def firstImport  = Some(impInfo)
    override final def isRootImport = !tree.pos.isDefined
    override final def toString     = s"ImportContext { $impInfo; outer.owner = ${outer.owner} }"
  }

  /** A buffer for warnings and errors that are accumulated during speculative type checking. */
  final class ReportBuffer {
    type Error = AbsTypeError
    type Warning = (Position, String)

    private def newBuffer[A] = mutable.LinkedHashSet.empty[A] // Important to use LinkedHS for stable results.

    // [JZ] Contexts, pre- the SI-7345 refactor, avoided allocating the buffers until needed. This
    // is replicated here out of conservatism.
    private var _errorBuffer: mutable.LinkedHashSet[Error] = _
    private def errorBuffer = {if (_errorBuffer == null) _errorBuffer = newBuffer; _errorBuffer}
    def errors: immutable.Seq[Error] = errorBuffer.toVector

    private var _warningBuffer: mutable.LinkedHashSet[Warning] = _
    private def warningBuffer = {if (_warningBuffer == null) _warningBuffer = newBuffer; _warningBuffer}
    def warnings: immutable.Seq[Warning] = warningBuffer.toVector

    def +=(error: AbsTypeError): this.type = {
      errorBuffer += error
      this
    }
    def ++=(errors: Traversable[AbsTypeError]): this.type = {
      errorBuffer ++= errors
      this
    }
    def +=(warning: Warning): this.type = {
      warningBuffer += warning
      this
    }

    def clearAll(): this.type = {
      clearAllErrors(); clearAllWarnings();
    }

    def clearAllErrors(): this.type = {
      errorBuffer.clear()
      this
    }
    def clearErrors(removeF: PartialFunction[AbsTypeError, Boolean]): this.type = {
      errorBuffer.retain(!PartialFunction.cond(_)(removeF))
      this
    }
    def retainErrors(leaveF: PartialFunction[AbsTypeError, Boolean]): this.type = {
      errorBuffer.retain(PartialFunction.cond(_)(leaveF))
      this
    }
    def clearAllWarnings(): this.type = {
      warningBuffer.clear()
      this
    }

    def hasErrors     = errorBuffer.nonEmpty
    def firstError    = errorBuffer.headOption
  }

  class ImportInfo(val tree: Import, val depth: Int) {
    def pos = tree.pos
    def posOf(sel: ImportSelector) = tree.pos withPoint sel.namePos

    /** The prefix expression */
    def qual: Tree = tree.symbol.info match {
      case ImportType(expr) => expr
      case ErrorType        => tree setType NoType // fix for #2870
      case _                => throw new FatalError("symbol " + tree.symbol + " has bad type: " + tree.symbol.info) //debug
    }

    /** Is name imported explicitly, not via wildcard? */
    def isExplicitImport(name: Name): Boolean =
      tree.selectors exists (_.rename == name.toTermName)

    /** The symbol with name `name` imported from import clause `tree`.
     */
    def importedSymbol(name: Name): Symbol = importedSymbol(name, requireExplicit = false)

    private def recordUsage(sel: ImportSelector, result: Symbol) {
      def posstr = pos.source.file.name + ":" + posOf(sel).safeLine
      def resstr = if (tree.symbol.hasCompleteInfo) s"(qual=$qual, $result)" else s"(expr=${tree.expr}, ${result.fullLocationString})"
      debuglog(s"In $this at $posstr, selector '${selectorString(sel)}' resolved to $resstr")
      allUsedSelectors(this) += sel
    }

    /** If requireExplicit is true, wildcard imports are not considered. */
    def importedSymbol(name: Name, requireExplicit: Boolean): Symbol = {
      var result: Symbol = NoSymbol
      var renamed = false
      var selectors = tree.selectors
      def current = selectors.head
      while (selectors.nonEmpty && result == NoSymbol) {
        if (current.rename == name.toTermName)
          result = qual.tpe.nonLocalMember( // new to address #2733: consider only non-local members for imports
            if (name.isTypeName) current.name.toTypeName else current.name)
        else if (current.name == name.toTermName)
          renamed = true
        else if (current.name == nme.WILDCARD && !renamed && !requireExplicit)
          result = qual.tpe.nonLocalMember(name)

        if (result == NoSymbol)
          selectors = selectors.tail
      }
      if (settings.lint && selectors.nonEmpty && result != NoSymbol && pos != NoPosition)
        recordUsage(current, result)

      // Harden against the fallout from bugs like SI-6745
      //
      // [JZ] I considered issuing a devWarning and moving the
      //      check inside the above loop, as I believe that
      //      this always represents a mistake on the part of
      //      the caller.
      if (definitions isImportable result) result
      else NoSymbol
    }
    private def selectorString(s: ImportSelector): String = {
      if (s.name == nme.WILDCARD && s.rename == null) "_"
      else if (s.name == s.rename) "" + s.name
      else s.name + " => " + s.rename
    }

    def allImportedSymbols: Iterable[Symbol] =
      importableMembers(qual.tpe) flatMap (transformImport(tree.selectors, _))

    private def transformImport(selectors: List[ImportSelector], sym: Symbol): List[Symbol] = selectors match {
      case List() => List()
      case List(ImportSelector(nme.WILDCARD, _, _, _)) => List(sym)
      case ImportSelector(from, _, to, _) :: _ if from == sym.name =>
        if (to == nme.WILDCARD) List()
        else List(sym.cloneSymbol(sym.owner, sym.rawflags, to))
      case _ :: rest => transformImport(rest, sym)
    }

    override def hashCode = tree.##
    override def equals(other: Any) = other match {
      case that: ImportInfo => (tree == that.tree)
      case _                => false
    }
    override def toString = tree.toString
  }

  case class ImportType(expr: Tree) extends Type {
    override def safeToString = "ImportType("+expr+")"
  }
}

object ContextMode {
  private implicit def liftIntBitsToContextState(bits: Int): ContextMode = apply(bits)
  def apply(bits: Int): ContextMode = new ContextMode(bits)
  final val NOmode: ContextMode                   = 0

  final val ReportErrors: ContextMode             = 1 << 0
  final val BufferErrors: ContextMode             = 1 << 1
  final val AmbiguousErrors: ContextMode          = 1 << 2

  /** Are we in a secondary constructor after the this constructor call? */
  final val ConstructorSuffix: ContextMode        = 1 << 3

  /** For method context: were returns encountered? */
  final val ReturnsSeen: ContextMode              = 1 << 4

  /** Is this context (enclosed in) a constructor call?
    * (the call to the super or self constructor in the first line of a constructor.)
    * In such a context, the object's fields should not be in scope
    */
  final val SelfSuperCall: ContextMode            = 1 << 5

  // TODO harvest documentation for this
  final val ImplicitsEnabled: ContextMode         = 1 << 6

  final val MacrosEnabled: ContextMode            = 1 << 7

  /** To selectively allow enrichment in patterns, where other kinds of implicit conversions are not allowed */
  final val EnrichmentEnabled: ContextMode        = 1 << 8

  /** Are we in a run of [[scala.tools.nsc.typechecker.TreeCheckers]]? */
  final val Checking: ContextMode                 = 1 << 9

  /** Are we retypechecking arguments independently from the function applied to them? See `Typer.tryTypedApply`
   *  TODO - iron out distinction/overlap with SecondTry.
   */
  final val ReTyping: ContextMode                 = 1 << 10

  /** Are we typechecking pattern alternatives. Formerly ALTmode. */
  final val PatternAlternative: ContextMode       = 1 << 11

  /** Are star patterns allowed. Formerly STARmode. */
  final val StarPatterns: ContextMode             = 1 << 12

  /** Are we typing the "super" in a superclass constructor call super.<init>. Formerly SUPERCONSTRmode. */
  final val SuperInit: ContextMode                = 1 << 13

  /*  Is this the second attempt to type this tree? In that case functions
   *  may no longer be coerced with implicit views. Formerly SNDTRYmode.
   */
  final val SecondTry: ContextMode                = 1 << 14

  /** Are we in return position? Formerly RETmode. */
  final val ReturnExpr: ContextMode               = 1 << 15

  /** Are unapplied type constructors allowed here? Formerly HKmode. */
  final val TypeConstructorAllowed: ContextMode   = 1 << 16

  /** TODO: The "sticky modes" are EXPRmode, PATTERNmode, TYPEmode.
   *  To mimick the sticky mode behavior, when captain stickyfingers
   *  comes around we need to propagate those modes but forget the other
   *  context modes which were once mode bits; those being so far the
   *  ones listed here.
   */
  final val FormerNonStickyModes: ContextMode = (
    PatternAlternative | StarPatterns | SuperInit | SecondTry | ReturnExpr | TypeConstructorAllowed
  )

  final val DefaultMode: ContextMode      = MacrosEnabled

  private val contextModeNameMap = Map(
    ReportErrors           -> "ReportErrors",
    BufferErrors           -> "BufferErrors",
    AmbiguousErrors        -> "AmbiguousErrors",
    ConstructorSuffix      -> "ConstructorSuffix",
    SelfSuperCall          -> "SelfSuperCall",
    ImplicitsEnabled       -> "ImplicitsEnabled",
    MacrosEnabled          -> "MacrosEnabled",
    Checking               -> "Checking",
    ReTyping               -> "ReTyping",
    PatternAlternative     -> "PatternAlternative",
    StarPatterns           -> "StarPatterns",
    SuperInit              -> "SuperInit",
    SecondTry              -> "SecondTry",
    TypeConstructorAllowed -> "TypeConstructorAllowed"
  )
}

/**
 * A value class to carry the boolean flags of a context, such as whether errors should
 * be buffered or reported.
 */
final class ContextMode private (val bits: Int) extends AnyVal {
  import ContextMode._

  def &(other: ContextMode): ContextMode  = new ContextMode(bits & other.bits)
  def |(other: ContextMode): ContextMode  = new ContextMode(bits | other.bits)
  def &~(other: ContextMode): ContextMode = new ContextMode(bits & ~(other.bits))
  def set(value: Boolean, mask: ContextMode) = if (value) |(mask) else &~(mask)

  def inAll(required: ContextMode)        = (this & required) == required
  def inAny(required: ContextMode)        = (this & required) != NOmode
  def inNone(prohibited: ContextMode)     = (this & prohibited) == NOmode

  override def toString =
    if (bits == 0) "NOmode"
    else (contextModeNameMap filterKeys inAll).values.toList.sorted mkString " "
}
