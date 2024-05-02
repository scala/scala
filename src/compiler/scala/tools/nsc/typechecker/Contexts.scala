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

package scala.tools.nsc
package typechecker

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal.util.{CodeAction, ReusableInstance, shortClassOfInstance, ListOfNil, SomeOfNil}
import scala.tools.nsc.Reporting.WarningCategory
import scala.util.chaining._

/**
 *  @author  Martin Odersky
 */
trait Contexts { self: Analyzer =>
  import global._
  import definitions.{JavaLangPackage, ScalaPackage, PredefModule, ScalaXmlTopScope, ScalaXmlPackage}
  import ContextMode._
  import scala.reflect.internal.Flags._


  protected def onTreeCheckerError(pos: Position, msg: String): Unit = ()

  object NoContext
    extends Context(EmptyTree, NoSymbol, EmptyScope, NoCompilationUnit,
      // We can't pass the uninitialized `this`. Instead, we treat null specially in `Context#outer`
                    null, 0) {
    enclClass  = this
    enclMethod = this

    override def enclosingContextChain: List[Context] = Nil
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
  private lazy val NoJavaMemberFound = (NoType, NoSymbol)

  private lazy val startContext = NoContext.make(
    Template(List(), noSelfType, List()) setSymbol global.NoSymbol setType global.NoType,
    rootMirror.RootClass,
    rootMirror.RootClass.info.decls
  )

  private lazy val allUsedSelectors =
    mutable.Map.empty[ImportInfo, Set[ImportSelector]].withDefaultValue(Set.empty)
  private lazy val allImportInfos =
    mutable.Map.empty[CompilationUnit, List[(ImportInfo, Symbol)]].withDefaultValue(Nil)

  def warnUnusedImports(unit: CompilationUnit) = if (!unit.isJava) {
    def msg(sym: Symbol) = sym.deprecationMessage.map(": " + _).getOrElse("")
    def checkDeprecatedElementInPath(selector: ImportSelector, info: ImportInfo): String = {
      def badName(name: Name) =
        info.qual.tpe.member(name) match {
          case m if m.isDeprecated => Some(s" of $m${msg(m)}")
          case _ => None
        }
      val badSelected =
        if (!selector.isMask && selector.isSpecific) badName(selector.name).orElse(badName(selector.name.toTypeName))
        else None
      def badFrom = {
        val sym = info.qual.symbol
        if (sym.isDeprecated) Some(s" from $sym${msg(sym)}") else None
      }
      badSelected.orElse(badFrom).getOrElse("")
    }
    def warnUnusedSelections(infos0: List[(ImportInfo, Symbol)]): Unit = {
      type Culled = (ImportSelector, ImportInfo, Symbol)
      var unused = List.empty[Culled]
      @tailrec def loop(infos: List[(ImportInfo, Symbol)]): Unit =
        infos match {
          case (info, owner) :: infos =>
            val used = allUsedSelectors.remove(info).getOrElse(Set.empty)
            def checkSelectors(selectors: List[ImportSelector]): Unit =
              selectors match {
                case selector :: selectors =>
                  checkSelectors(selectors)
                  if (!selector.isMask && !used(selector))
                    unused ::= ((selector, info, owner))
                case _ =>
              }
            checkSelectors(info.tree.selectors)
            loop(infos)
          case _ =>
        }
      loop(infos0)
      unused.foreach {
        case (selector, info, owner) =>
          val pos = info.posOf(selector)
          val origin = info.fullSelectorString(selector)
          val addendum = checkDeprecatedElementInPath(selector, info)
          runReporting.warning(pos, s"Unused import$addendum", WarningCategory.UnusedImports, owner, origin)
      }
    }
    allImportInfos.remove(unit).foreach(warnUnusedSelections)
  }

  var lastAccessCheckDetails: String = ""

  val rootImportsCached = perRunCaches.newMap[CompilationUnit, List[Symbol]]()

  val excludedRootImportsCached = perRunCaches.newMap[CompilationUnit, List[Symbol]]()

  // register an import for the narrow purpose of excluding root imports of predef modules
  def registerImport(ctx: Context, imp: Import): Unit = {
    val sym = imp.expr.symbol
    if (sym != null && !sym.hasPackageFlag && ctx.enclosingNonImportContext.owner.hasPackageFlag && rootImports(ctx.unit).contains(sym)) {
      var current = excludedRootImportsCached.get(ctx.unit).getOrElse(Nil)
      current = sym :: current
      excludedRootImportsCached += ctx.unit -> current
    }
  }

  /** List of symbols to import from in a root context.  By default, that
   *  is `java.lang`, `scala`, and [[scala.Predef]], in that order.
   *
   *  - if option `-Yimports` is supplied, then that specifies the preamble imports
   *  - if the unit body has an import of Predef
   *    among its leading imports, or if the tree is [[scala.Predef]], `Predef` is not imported.
   *    Similarly for any module among the preamble imports.
   *  - if the unit is java defined, only `java.lang` is imported
   *
   *  The root imports for a unit are cached.
   */
  protected def rootImports(unit: CompilationUnit): List[Symbol] = {
    assert(definitions.isDefinitionsInitialized, "definitions uninitialized")

    if (unit.isJava) RootImports.javaList
    else rootImportsCached.get(unit).getOrElse {
      val calculated = defaultRootImports
      rootImportsCached += unit -> calculated
      calculated
    }
  }

  private def defaultRootImports: List[Symbol] =
    if (settings.imports.isSetByUser)
      settings.imports.value.map {
        case "java.lang"    => JavaLangPackage
        case "scala"        => ScalaPackage
        case "scala.Predef" => PredefModule
        case name           =>
          import rootMirror.{getModuleIfDefined, getPackageObjectIfDefined, getPackageIfDefined}
          getModuleIfDefined(name) orElse
          getPackageObjectIfDefined(name) orElse
          getPackageIfDefined(name) orElse {
            // force package objects in prefixes
            def force(pkg: String, next: String): String = {
              val full = if (pkg.isEmpty) next else s"$pkg.$next"
              val sym = getPackageIfDefined(full)
              if (sym != NoSymbol) openPackageModule(sym, force = true)
              full
            }
            name.split('.').toList.init.foldLeft("")(force)
            getModuleIfDefined(name)
          } orElse NoSymbol.tap(_ => globalError(s"bad preamble import $name"))
      }
    else RootImports.completeList

  def rootContext(unit: CompilationUnit, tree: Tree = EmptyTree, throwing: Boolean = false, checking: Boolean = false): Context = {
    val rootImportsContext = rootImports(unit).foldLeft(startContext)((c, sym) =>
      c.make(gen.mkWildcardImport(sym), unit = unit)
    )

    // there must be a scala.xml package when xml literals were parsed in this unit
    if (unit.hasXml && ScalaXmlPackage == NoSymbol)
      reporter.error(unit.firstXmlPos, "To compile XML syntax, the scala.xml package must be on the classpath.\nPlease see https://github.com/scala/scala-xml for details.")

    // scala-xml needs `scala.xml.TopScope` to be in scope globally as `$scope`
    // We detect `scala-xml` by looking for `scala.xml.TopScope` and
    // inject the equivalent of `import scala.xml.{TopScope => $scope}`
    val contextWithXML =
      if (!unit.hasXml || ScalaXmlTopScope == NoSymbol) rootImportsContext
      else rootImportsContext.make(gen.mkImport(ScalaXmlPackage, nme.TopScope, nme.dollarScope))

    contextWithXML.make(tree, unit = unit).tap(_.initRootContext(throwing, checking))
  }

  def rootContextPostTyper(unit: CompilationUnit, tree: Tree = EmptyTree): Context =
    rootContext(unit, tree, throwing = true)

  def resetContexts(): Unit = {
    startContext.enclosingContextChain foreach { context =>
      context.tree match {
        case Import(qual, _) => qual setType singleType(qual.symbol.owner.thisType, qual.symbol)
        case _               =>
      }
      context.reporter.clearAll()
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
   *   - Some odds and ends: undetermined type parameters of the current line of type inference;
   *     contextual augmentation for error messages, tracking of the nesting depth.
   *
   * And behaviour:
   *
   *   - The central point for issuing errors and warnings from the typechecker, with a means
   *     to buffer these for use in 'silent' type checking, when some recovery might be possible.
   *  -  `Context` is something of a Zipper for the tree were are typechecking: it `enclosingContextChain`
   *     is the path back to the root. This is exactly what we need to resolve names (`lookupSymbol`)
   *     and to collect in-scope implicit definitions (`implicitss`)
   *     Supporting these are `imports`, which represents all `Import` trees in in the enclosing context chain.
   *  -  In a similar vein, we can assess accessibility (`isAccessible`.)
   *
   * More on error buffering:
   *     When are type errors recoverable? In quite a few places, it turns out. Some examples:
   *     trying to type an application with/without the expected type, or with/without implicit views
   *     enabled. This is usually mediated by `Typer.silent`, `Inferencer#tryTwice`.
   *
   *     Initially, starting from the `typer` phase, the contexts either buffer or report errors;
   *     afterwards errors are thrown. This is configured in `rootContext`. Additionally, more
   *     fine grained control is needed based on the kind of error; ambiguity errors are often
   *     suppressed during exploratory typing, such as determining whether `a == b` in an argument
   *     position is an assignment or a named argument, when `Inferencer#isApplicableSafe` type checks
   *     applications with and without an expected type, or when `Typer#tryTypedApply` tries to fit arguments to
   *     a function type with/without implicit views.
   *
   *     When the error policies entail error/warning buffering, the mutable [[ContextReporter]] records
   *     everything that is issued. It is important to note, that child Contexts created with `make`
   *     "inherit" the very same `ContextReporter` instance, whereas children spawned through `makeSilent`
   *     receive a separate, fresh buffer.
   *
   * @param tree  Tree associated with this context
   * @param owner The current owner
   * @param scope The current scope
   * @param _outer The next outer context.
   */
  class Context private[typechecker](val tree: Tree, val owner: Symbol, val scope: Scope,
                                     val unit: CompilationUnit, _outer: Context, val depth: Int,
                                     private[this] var _reporter: ContextReporter = new ThrowingReporter) {
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
    def update(mask: ContextMode, value: Boolean): Unit = {
      contextMode = contextMode.set(value, mask)
    }

    /** Set all modes in the mask `enable` to true, and all in `disable` to false. */
    def set(enable: ContextMode = NOmode, disable: ContextMode = NOmode): this.type = {
      contextMode = (contextMode | enable) &~ disable
      this
    }

    /** Is this context in all modes in the given `mask`? */
    def apply(mask: ContextMode): Boolean = contextMode.inAll(mask)

    /** The next (logical) outer context whose tree is a method.
      *
      * NOTE: this is the "logical" enclosing method, which may not be the actual enclosing method when we
      * synthesize a nested method, such as for lazy val getters (scala/bug#8245) or the methods that
      * implement a PartialFunction literal (scala/bug#10291).
      */
    var enclMethod: Context = _

    private[this] var _undetparams: List[Symbol] = List()

    protected def outerDepth = if (outerIsNoContext) 0 else outer.depth

    /** The currently visible imports, from innermost to outermost. */
    def imports: List[ImportInfo] = outer.imports
    /** Equivalent to `imports.headOption`, but more efficient */
    def firstImport: Option[ImportInfo] = outer.firstImport
    protected[Contexts] def importOrNull: ImportInfo = null
    /** A root import is never unused and always bumps context depth. (e.g scala._ / Predef._ and magic REPL imports) */
    def isRootImport: Boolean = false

    var pendingStabilizers: List[Tree] = Nil

    /** Types for which implicit arguments are currently searched */
    var openImplicits: List[OpenImplicit] = List()
    final def isSearchingForImplicitParam: Boolean = {
      openImplicits.nonEmpty && openImplicits.exists(x => !x.isView)
    }

    private type ImplicitDict = List[(Type, (Symbol, Tree))]
    private var implicitDictionary: ImplicitDict = null

    @tailrec final def implicitRootContext: Context = {
      if(implicitDictionary != null) this
      else if(outerIsNoContext || outer.openImplicits.isEmpty) {
        implicitDictionary = Nil
        this
      } else outer.implicitRootContext
    }

    private def linkImpl(tpe: Type): Tree = {
      val sym =
        implicitDictionary.find(_._1 =:= tpe) match {
          case Some((_, (sym, _))) => sym
          case None =>
            val fresh = freshNameCreatorFor(this)
            val vname = newTermName(fresh.newName("rec$"))
            val vsym = owner.newValue(vname, newFlags = FINAL | SYNTHETIC) setInfo tpe
            implicitDictionary +:= ((tpe, (vsym, EmptyTree)))
            vsym
        }
      gen.mkAttributedRef(sym) setType tpe
    }

    final def linkByNameImplicit(tpe: Type): Tree = implicitRootContext.linkImpl(tpe)

    private def refImpl(tpe: Type): Tree =
      implicitDictionary.find(_._1 =:= tpe) match {
        case Some((_, (sym, _))) =>
          gen.mkAttributedRef(sym) setType tpe
        case None =>
          EmptyTree
      }

    final def refByNameImplicit(tpe: Type): Tree = implicitRootContext.refImpl(tpe)

    private def defineImpl(tpe: Type, result: SearchResult): SearchResult = {
      @tailrec
      def patch(d: ImplicitDict, acc: ImplicitDict): (ImplicitDict, SearchResult) = d match {
        case Nil => (implicitDictionary, result)
        case (tp, (sym, EmptyTree)) :: tl if tp =:= tpe =>
          val ref = gen.mkAttributedRef(sym) setType tpe
          val res = new SearchResult(ref, result.subst, result.undetparams)
          (acc reverse_::: ((tpe, (sym, result.tree)) :: tl), res)
        case hd :: tl =>
          patch(tl, hd :: acc)
      }

      val (d, res) = patch(implicitDictionary, Nil)
      implicitDictionary = d
      res
    }

    def defineByNameImplicit(tpe: Type, result: SearchResult): SearchResult = implicitRootContext.defineImpl(tpe, result)

    def emitImplicitDictionary(result: SearchResult): SearchResult =
      if(implicitDictionary == null || implicitDictionary.isEmpty || result.tree == EmptyTree) result
      else {
        val typer = newTyper(this)

        @tailrec
        def prune(trees: List[Tree], pending: List[(Symbol, Tree)], acc: List[(Symbol, Tree)]): List[(Symbol, Tree)] = pending match {
          case Nil => acc
          case ps =>
            val (in, out) = ps.partition { case (vsym, rhs) => trees.exists(_.exists(_.symbol == vsym)) }
            if(in.isEmpty) acc
            else prune(in.map(_._2) ++ trees, out, in ++ acc)
        }

        val pruned = prune(List(result.tree), implicitDictionary.map(_._2), Nil)
        if (pruned.isEmpty) result
        else if (pruned.exists(_._2 == EmptyTree)) SearchFailure
        else {
          val pos = result.tree.pos
          val (dictClassSym, dictClass0) = {
            val cname = newTypeName(typer.fresh.newName("LazyDefns$"))
            val parents = addSerializable(definitions.AnyRefTpe)
            val csym = owner.newClass(cname, pos, FINAL | SYNTHETIC)
            csym.setInfo(ClassInfoType(parents, newScope, csym))

            val vdefs = pruned.map { case (vsym, rhs) =>
              changeNonLocalOwners(rhs, vsym)
              // We want the normal mechanism for generating accessors during
              // typechecking to be applied, so we don't create symbols for
              // these ValDefs ourselves.
              atPos(pos)(ValDef(Modifiers(FINAL | SYNTHETIC), vsym.name.toTermName, TypeTree(rhs.tpe), rhs))
            }

            val cdef = {
              val cdef0 = ClassDef(csym, NoMods, ListOfNil, vdefs, pos)
              typer.namer.enterSym(cdef0)
              typer.typed(cdef0)
            }

            (csym, cdef)
          }

          val dictTpe = dictClassSym.tpe_*

          val preSyms = pruned.map(_._1)
          val postSyms = preSyms.map(vsym => dictTpe.decl(vsym.name))

          val symMap = (preSyms zip postSyms).toMap

          val dictClass = {
            class DictionarySubstituter extends TreeSymSubstituter(preSyms, postSyms) {
              override def transform(tree: Tree): Tree = {
                if (tree.hasExistingSymbol) {
                  val sym = tree.symbol
                  symMap.get(sym.owner).foreach(sym.owner = _)
                }
                super.transform(tree)
              }
            }
            (new DictionarySubstituter)(dictClass0)
          }

          val dictSym = {
            val vname = newTermName(typer.fresh.newName("lazyDefns$"))
            owner.newValue(vname, pos, FINAL | SYNTHETIC).setInfo(dictTpe)
          }

          val dict = {
            val rhs = atPos(pos)(Apply(Select(New(Ident(dictClassSym)), nme.CONSTRUCTOR), List()))
            val vdef0 = ValDef(dictSym, rhs)
            typer.namer.enterSym(vdef0)
            typer.typed(vdef0)
          }

          val resultTree = {
            class ReferenceSubstituter extends TreeSymSubstituter(preSyms, postSyms) {
              override def transform(tree: Tree): Tree = tree match {
                case i: Ident if symMap.contains(i.symbol) =>
                  super.transform(atPos(i.pos)(treeCopy.Select(i, gen.mkAttributedRef(dictSym), i.name)))

                case _ =>
                  super.transform(tree)
              }
            }
            (new ReferenceSubstituter)(result.tree)
          }

          val resultBlock = atPos(pos.focus)(Block(dictClass, dict, resultTree).setType(resultTree.tpe))
          new SearchResult(resultBlock, result.subst, result.undetparams)
        }
      }

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
    def retyping_=(value: Boolean)            = this(ReTyping) = value
    def retyping                              = this(ReTyping)
    def inSecondTry                           = this(SecondTry)
    def inSecondTry_=(value: Boolean)         = this(SecondTry) = value
    def inReturnExpr                          = this(ReturnExpr)
    def inTypeConstructorAllowed              = this(TypeConstructorAllowed)
    def inAnnotation                          = this(TypingAnnotation)

    def defaultModeForTyped: Mode = if (inTypeConstructorAllowed) Mode.NOmode else Mode.EXPRmode

    /** Saved type bounds for type parameters which are narrowed in a GADT. */
    var savedTypeBounds: List[(Symbol, Type)] = List()

    /** The next enclosing context (potentially `this`) that is owned by a class or method */
    @tailrec
    final def enclClassOrMethod: Context =
      if (!owner.exists || owner.isClass || owner.isMethod) this
      else outer.enclClassOrMethod

    /** The next enclosing context (potentially `this`) that has a `CaseDef` as a tree */
    def enclosingCaseDef = nextEnclosing(_.tree.isInstanceOf[CaseDef])

    /** ...or an Apply. */
    def enclosingApply = nextEnclosing(_.tree.isInstanceOf[Apply])

    @tailrec
    final def enclosingImport: Context = this match {
      case _: ImportContext => this
      case NoContext => this
      case _ => outer.enclosingImport
    }

    def siteString = {
      def what_s  = if (owner.isConstructor) "" else owner.kindString
      def where_s = if (owner.isClass) "" else "in " + enclClass.owner.decodedName
      List(what_s, owner.decodedName, where_s) filterNot (_ == "") mkString " "
    }
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
        setAmbiguousErrors(reportAmbiguous)
        val saved = extractUndetparams()
        try body
        finally undetparams = saved
      }
    }

    //
    // Error reporting policies and buffer.
    //

    // the reporter for this context
    def reporter: ContextReporter = _reporter

    // if set, errors will not be reporter/thrown
    def bufferErrors = reporter.isBuffering
    def reportErrors = !(bufferErrors || reporter.isThrowing)

    // whether to *report* (which is separate from buffering/throwing) ambiguity errors
    def ambiguousErrors = this(AmbiguousErrors)

    private def setAmbiguousErrors(report: Boolean): Unit = this(AmbiguousErrors) = report

    /**
     * Try inference twice: once without views and once with views,
     *  unless views are already disabled.
     */
    abstract class TryTwice {
      def tryOnce(isLastTry: Boolean): Unit

      final def apply(): Unit = {
        val doLastTry =
          // do first try if implicits are enabled
          if (implicitsEnabled) {
            // We create a new BufferingReporter to
            // distinguish errors that occurred before entering tryTwice
            // and our first attempt in 'withImplicitsDisabled'. If the
            // first attempt fails, we try with implicits on
            // and the original reporter.
            // immediate reporting of ambiguous errors is suppressed, so that they are buffered
            inSilentMode {
              try {
                set(disable = ImplicitsEnabled | EnrichmentEnabled) // restored by inSilentMode
                tryOnce(isLastTry = false)
                reporter.hasErrors
              } catch {
                case ex: CyclicReference => throw ex
                case ex: TypeError => true // recoverable cyclic references?
              }
            }
          } else true

        // do last try if try with implicits enabled failed
        // (or if it was not attempted because they were disabled)
        if (doLastTry)
          tryOnce(isLastTry = true)
      }
    }

    //
    // Temporary mode adjustment
    //

    @inline final def withMode[T](enabled: ContextMode = NOmode, disabled: ContextMode = NOmode)(op: => T): T = {
      val saved = contextMode
      set(enabled, disabled)
      try op
      finally contextMode = saved
    }

    @inline final def withImplicitsEnabled[T](op: => T): T                 = withMode(enabled = ImplicitsEnabled)(op)
    @inline final def withImplicitsDisabled[T](op: => T): T                = withMode(disabled = ImplicitsEnabled | EnrichmentEnabled)(op)
    @inline final def withImplicitsDisabledAllowEnrichment[T](op: => T): T = withMode(enabled = EnrichmentEnabled, disabled = ImplicitsEnabled)(op)
    @inline final def withImplicits[T](enabled: Boolean)(op: => T): T      = if (enabled) withImplicitsEnabled(op) else withImplicitsDisabled(op)
    @inline final def withMacrosEnabled[T](op: => T): T                    = withMode(enabled = MacrosEnabled)(op)
    @inline final def withMacrosDisabled[T](op: => T): T                   = withMode(disabled = MacrosEnabled)(op)
    @inline final def withMacros[T](enabled: Boolean)(op: => T): T         = if (enabled) withMacrosEnabled(op) else withMacrosDisabled(op)
    @inline final def withinStarPatterns[T](op: => T): T                   = withMode(enabled = StarPatterns)(op)
    @inline final def withinSuperInit[T](op: => T): T                      = withMode(enabled = SuperInit)(op)
    @inline final def withinSecondTry[T](op: => T): T                      = withMode(enabled = SecondTry)(op)
    @inline final def withinPatAlternative[T](op: => T): T                 = withMode(enabled = PatternAlternative)(op)
    @inline final def withinAnnotation[T](op: => T): T                     = withMode(enabled = TypingAnnotation)(op)

    @inline final def withSuppressDeadArgWarning[T](suppress: Boolean)(op: => T): T =
      if (suppress) withMode(enabled = SuppressDeadArgWarning)(op) else withMode(disabled = SuppressDeadArgWarning)(op)

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

    // inliner note: this has to be a simple method for inlining to work -- moved the `&& !reporter.hasErrors` out
    @inline final def inSilentMode(expr: => Boolean): Boolean = {
      val savedContextMode = contextMode
      val savedReporter    = reporter

      setAmbiguousErrors(report = false)
      _reporter = new BufferingReporter

      try expr
      finally {
        contextMode = savedContextMode
        _reporter   = savedReporter
      }
    }

    //
    // Child Context Creation
    //

    /**
     * Construct a child context. The parent and child will share the report buffer.
     * Compare with `makeSilent`, in which the child has a fresh report buffer.
     *
     * If `tree` is an `Import`, that import will be available at the head of
     * `Context#imports`.
     */
    def make(tree: Tree = tree, owner: Symbol = owner,
             scope: Scope = scope, unit: CompilationUnit = unit,
             reporter: ContextReporter = this.reporter): Context = {
      val isTemplateOrPackage = tree match {
        case _: Template | _: PackageDef => true
        case _                           => false
      }
      val isDefDef = tree match {
        case _: DefDef => true
        case _         => false
      }
      val isImport = tree match {
        // The guard is for scala/bug#8403. It prevents adding imports again in the context created by
        // `Namer#createInnerNamer`
        case _: Import if tree != this.tree => true
        case _                              => false
      }
      val sameOwner = owner == this.owner
      val prefixInChild =
        if (isTemplateOrPackage) owner.thisType
        else if (!sameOwner && owner.isTerm) NoPrefix
        else prefix

      def innerDepth(isRootImport: Boolean) = {
        val increasesDepth = isRootImport || (this == NoContext) || (this.scope != scope)
        depth + (if (increasesDepth) 1 else 0)
      }

      // The blank canvas
      val c = if (isImport) {
        val isRootImport = !tree.pos.isDefined || isReplImportWrapperImport(tree)
        new ImportContext(tree, owner, scope, unit, this, isRootImport, innerDepth(isRootImport), reporter)
      } else
        new Context(tree, owner, scope, unit, this, innerDepth(isRootImport = false), reporter)

      // Fields that are directly propagated
      c.openImplicits      = openImplicits
      c.contextMode        = contextMode // note: ConstructorSuffix, a bit within `mode`, is conditionally overwritten below.

      // Fields that may take on a different value in the child
      c.prefix             = prefixInChild
      c.enclClass          = if (isTemplateOrPackage) c else enclClass
      c(ConstructorSuffix) = !isTemplateOrPackage && c(ConstructorSuffix)

      // scala/bug#8245 `isLazy` need to skip lazy getters to ensure `return` binds to the right place
      c.enclMethod         = if (isDefDef && !owner.isLazy) c else enclMethod

      if (tree != outer.tree)
        c(TypeConstructorAllowed) = false

      registerContext(c.asInstanceOf[analyzer.Context])
      debuglog(s"[context] ++ ${c.unit} / ${if (tree == null) "" else tree.summaryString}")
      c
    }

    def makeImportContext(tree: Import): Context =
      make(tree).tap { ctx =>
        if (settings.warnUnusedImport && openMacros.isEmpty && !ctx.isRootImport)
          allImportInfos(ctx.unit) ::= ctx.importOrNull -> ctx.owner
      }

    /** Use reporter (possibly buffered) for errors/warnings and enable implicit conversion **/
    def initRootContext(throwing: Boolean = false, checking: Boolean = false): Unit = {
      _reporter =
        if (checking) new CheckingReporter
        else if (throwing) new ThrowingReporter
        else new ImmediateReporter

      setAmbiguousErrors(!throwing)
      this(EnrichmentEnabled | ImplicitsEnabled) = !throwing
    }

    def make(tree: Tree, owner: Symbol, scope: Scope): Context =
      // TODO scala/bug#7345 Moving this optimization into the main overload of `make` causes all tests to fail.
      //              even if it is extended to check that `unit == this.unit`. Why is this?
      if (tree == this.tree && owner == this.owner && scope == this.scope) this
      else make(tree, owner, scope, unit)

    /** Make a child context that represents a new nested scope */
    def makeNewScope(tree: Tree, owner: Symbol, reporter: ContextReporter = this.reporter): Context =
      make(tree, owner, newNestedScope(scope), reporter = reporter)

    /** Make a child context that buffers errors and warnings into a fresh report buffer. */
    def makeSilent(reportAmbiguousErrors: Boolean = ambiguousErrors, newtree: Tree = tree): Context = {
      // A fresh buffer so as not to leak errors/warnings into `this`.
      val c = make(newtree, reporter = new BufferingReporter)
      c.setAmbiguousErrors(reportAmbiguousErrors)
      c
    }

    def makeNonSilent(newtree: Tree): Context = {
      val c = make(newtree, reporter = reporter.makeImmediate)
      c.setAmbiguousErrors(report = true)
      c
    }

    /** Make a silent child context does not allow implicits. Used to prevent chaining of implicit views. */
    def makeImplicit(reportAmbiguousErrors: Boolean) = {
      val c = makeSilent(reportAmbiguousErrors)
      c(ImplicitsEnabled | EnrichmentEnabled) = false
      c(InImplicitSearch) = true
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
      // must propagate reporter!
      // (caught by neg/t3649 when refactoring reporting to be specified only by this.reporter and not also by this.contextMode)
      val argContext = baseContext.makeNewScope(tree, owner, reporter = this.reporter)
      argContext.contextMode = contextMode
      argContext.inSelfSuperCall = true
      def enterElems(c: Context): Unit = {
        def enterLocalElems(e: ScopeEntry): Unit = {
          if (e != null && e.owner == c.scope) {
            enterLocalElems(e.next)
            argContext.scope enter e.sym
          }
        }
        if (c.owner.isTerm && !c.owner.isLocalDummy) {
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

    /** Issue/buffer/throw the given type error according to the current mode for error reporting. */
    private[typechecker] def issue(err: AbsTypeError) = reporter.issue(err)(this)

    /** Issue/buffer/throw the given implicit ambiguity error according to the current mode for error reporting. */
    private[typechecker] def issueAmbiguousError(err: AbsAmbiguousTypeError) = reporter.issueAmbiguousError(err)(this)

    /** Issue/throw the given error message according to the current mode for error reporting. */
    def error(pos: Position, msg: String, actions: List[CodeAction] = Nil) =
      reporter.errorAndDumpIfDebug(fixPosition(pos), msg, actions)

    /** Issue/throw the given error message according to the current mode for error reporting. */
    def warning(pos: Position, msg: String, category: WarningCategory, actions: List[CodeAction] = Nil): Unit =
      reporter.warning(fixPosition(pos), msg, category, owner, actions)
    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol, actions: List[CodeAction]): Unit =
      reporter.warning(fixPosition(pos), msg, category, site, actions)
    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol): Unit =
      warning(pos, msg, category, site, Nil)

    def echo(pos: Position, msg: String) = reporter.echo(fixPosition(pos), msg)

    def fixPosition(pos: Position): Position = pos match {
      case NoPosition => nextEnclosing(_.tree.pos != NoPosition).tree.pos
      case _ => pos
    }


    // TODO: buffer deprecations under silent (route through ContextReporter, store in BufferingReporter)
    def deprecationWarning(pos: Position, sym: Symbol, msg: String, since: String, actions: List[CodeAction] = Nil): Unit =
      runReporting.deprecationWarning(fixPosition(pos), sym, owner, msg, since, actions)
    def deprecationWarning(pos: Position, sym: Symbol): Unit =
      runReporting.deprecationWarning(fixPosition(pos), sym, owner)

    def featureWarning(pos: Position, featureName: String, featureDesc: String, featureTrait: Symbol, construct: => String = "", required: Boolean): Unit =
      runReporting.featureWarning(fixPosition(pos), featureName, featureDesc, featureTrait, construct, required, owner)


    @tailrec
    final def nextEnclosing(p: Context => Boolean): Context =
      if (this eq NoContext) this else if (p(this)) this else outer.nextEnclosing(p)

    final def outermostContextAtCurrentPos: Context = {
      var pos = tree.pos
      var encl = this
      while (pos == NoPosition && encl != NoContext) {
        encl = encl.outer
        pos = encl.tree.pos
      }
      while (encl.outer.tree.pos == pos && encl != NoContext)
        encl = encl.outer
      encl
    }

    def enclosingContextChain: List[Context] = this :: outer.enclosingContextChain

    private def treeTruncated       = tree.toString.replaceAll("\\s+", " ").linesIterator.mkString("\\n").take(70)
    private def treeIdString        = if (settings.uniqid.value) "#" + System.identityHashCode(tree).toString.takeRight(3) else ""
    private def treeString          = tree match {
      case x: Import => "" + x
      case Template(parents, `noSelfType`, body) =>
        val pstr = if ((parents eq null) || parents.isEmpty) "Nil" else parents mkString " "
        val bstr = if (body eq null) "" else "" + body.length + " stats"
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

    /** True iff...
      * - `sub` is a subclass of `base`
      * - `sub` is the module class of a companion of a subclass of `base`
      * - `base` is a Java-defined module class (containing static members),
      *   and `sub` is a subclass of its companion class. (see scala/bug#6394)
      */
    private def isSubClassOrCompanion(sub: Symbol, base: Symbol) =
      sub.isNonBottomSubClass(base) ||
        (sub.isModuleClass && sub.linkedClassOfClass.isNonBottomSubClass(base)) ||
        (base.isJavaDefined && base.isModuleClass && (
          sub.isNonBottomSubClass(base.linkedClassOfClass) ||
            sub.isModuleClass && sub.linkedClassOfClass.isNonBottomSubClass(base.linkedClassOfClass)))

    /** Return the closest enclosing context that defines a subclass of `clazz`
     *  or a companion object thereof, or `NoContext` if no such context exists.
     */
    def enclosingSubClassContext(clazz: Symbol): Context = {
      var c = this.enclClass
      while (c != NoContext && !isSubClassOrCompanion(c.owner, clazz))
        c = c.outer.enclClass
      c
    }

    def enclosingNonImportContext: Context = {
      var c = this
      while (c != NoContext && c.tree.isInstanceOf[Import])
        c = c.outer
      c
    }

    /** Is `sym` accessible as a member of `pre` in current context? */
    def isAccessible(sym: Symbol, pre: Type, superAccess: Boolean = false): Boolean = {
      lastAccessCheckDetails = ""
      // Console.println("isAccessible(%s, %s, %s)".format(sym, pre, superAccess))

      // don't have access if there is no linked class (so exclude linkedClass=NoSymbol)
      def accessWithinLinked(ab: Symbol) = {
        val linked = linkedClassOfClassOf(ab, this)
        linked.fold(false)(accessWithin)
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
        val preSym = pre.widen.typeSymbol
        if (c == NoContext)
          lastAccessCheckDetails =
            sm"""
                | Access to protected $target not permitted because
                | enclosing ${enclClass.owner.fullLocationString} is not a subclass of
                | ${sym.owner.fullLocationString} where target is defined"""
        c != NoContext &&
        {
          target.isType || { // allow accesses to types from arbitrary subclasses fixes scala/bug#4737
            val res =
              isSubClassOrCompanion(preSym, c.owner) ||
                (c.owner.isModuleClass
                  && isSubClassOrCompanion(preSym, c.owner.linkedClassOfClass)) ||
                (preSym.isJava
                  && preSym.isModuleClass) // java static members don't care about prefix for accessibility
            if (!res)
              lastAccessCheckDetails =
                sm"""
                    | Access to protected $target not permitted because
                    | prefix type ${pre.widen} does not conform to
                    | ${c.owner.fullLocationString} where the access takes place"""
              res
          }
        }
      }

      (pre == NoPrefix) || {
        val ab = sym.accessBoundary(sym.owner)

        (  (ab.isTerm || ab == rootMirror.RootClass)
        || (accessWithin(ab) || accessWithinLinked(ab)) &&
             (  !sym.isLocalToThis
             || sym.isProtected && isSubThisType(pre, sym.owner)
             || pre =:= sym.owner.thisType
             )
        || sym.isProtected &&
             (  superAccess
             || pre.isInstanceOf[ThisType]
             || phase.erasedTypes // (*)
             || (sym.overrideChain exists isProtectedAccessOK)
                // that last condition makes protected access via self types work.
             )
        )
        // (*) in t780.scala: class B extends A { protected val x }; trait A { self: B => x }
        // Before erasure, the `pre` is a `ThisType`, so the access is allowed. Erasure introduces
        // a cast to access `x` (this.$asInstanceOf[B].x), then `pre` is no longer a `ThisType`
        // but a `TypeRef` to `B`.
        // Note that `isProtectedAccessOK` is false, it checks if access is OK in the current
        // context's owner (trait `A`), not in the `pre` type.
        // This implementation makes `isAccessible` return false positives. Maybe the idea is to
        // represent VM-level information, as we don't emit protected? If so, it's wrong for
        // Java-defined symbols, which can be protected in bytecode. History:
        //   - Phase check added in 8243b2dd2d
        //   - Removed in 1536b1c67e, but moved to `accessBoundary`
        //   - Re-added in 42744ffda0 (and left in `accessBoundary`)
      }
    }

    //
    // Type bound management
    //

    def pushTypeBounds(sym: Symbol): Unit = {
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

    private var implicitsCache: List[ImplicitInfo] = null
    private var implicitsRunId = NoRunId

    @tailrec
    final def resetCache(): Unit = {
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
        (e ne null) && (e.owner == scope) && e.sym.exists
      })

    /** Do something with the symbols with name `name` imported via the import in `imp`,
     *  if any such symbol is accessible from this context and is a qualifying implicit.
     */
    private def withQualifyingImplicitAlternatives(imp: ImportInfo, name: Name, pre: Type)(f: Symbol => Unit) = {
      val imported = importedAccessibleSymbol(imp, imp.importedSymbol(name))
      if (imported.isOverloaded) {
        for (sym <- imported.alternatives)
          if (isQualifyingImplicit(name, sym, pre, imported = true))
            f(sym)
      }
      else if (isQualifyingImplicit(name, imported, pre, imported = true))
        f(imported)
    }

    private def collectImplicits(syms: Scope, pre: Type): List[ImplicitInfo] =
      for (sym <- syms.toList if isQualifyingImplicit(sym.name, sym, pre, imported = false))
      yield new ImplicitInfo(sym.name, pre, sym, inPackagePrefix = false)

    private def collectImplicitImports(imp: ImportInfo): List[ImplicitInfo] = if (isExcludedRootImport(imp)) List() else {
      val qual = imp.qual

      val pre = qual.tpe
      def collect(sels: List[ImportSelector]): List[ImplicitInfo] = sels match {
        case List() =>
          List()
        case sel :: _ if sel.isWildcard =>
          // Using pre.implicitMembers seems to exposes a problem with out-dated symbols in the IDE,
          // see the example in https://www.assembla.com/spaces/scala-ide/tickets/1002552#/activity/ticket
          // I haven't been able to boil that down the an automated test yet.
          // Looking up implicit members in the package, rather than package object, here is at least
          // consistent with what is done just below for named imports.
          for (sym <- qual.tpe.implicitMembers.toList if isQualifyingImplicit(sym.name, sym, pre, imported = true))
          yield new ImplicitInfo(sym.name, pre, sym, importInfo = imp, importSelector = sel)
        case (sel @ ImportSelector(from, _, to, _)) :: sels1 =>
          var impls = collect(sels1).filter(_.name != from)
          if (!sel.isMask)
            withQualifyingImplicitAlternatives(imp, to, pre) { sym =>
              impls = new ImplicitInfo(to, pre, sym, importInfo = imp, importSelector = sel) :: impls
            }
          impls
      }
      //debuglog("collect implicit imports " + imp + "=" + collect(imp.tree.selectors))//DEBUG
      collect(imp.tree.selectors)
    }

    /* scala/bug#5892 / scala/bug#4270: `implicitss` can return results which are not accessible at the
     * point where implicit search is triggered. Example: implicits in (annotations of)
     * class type parameters (scala/bug#5892). The `context.owner` is the class symbol, therefore
     * `implicitss` will return implicit conversions defined inside the class. These are
     * filtered out later by `eligibleInfos` (scala/bug#4270 / 9129cfe9), as they don't type-check.
     */
    final def implicitss: List[List[ImplicitInfo]] = implicitssImpl(NoSymbol)

    private def implicitssImpl(skipClass: Symbol): List[List[ImplicitInfo]] = {
      if (this == NoContext) Nil
      else if (owner == skipClass) outer.implicitssImpl(NoSymbol)
      else {
        def withOuter(is: List[ImplicitInfo]): List[List[ImplicitInfo]] = {
          // In a constructor super call, the members of the constructed class are not in scope. We
          // need to skip over the context of that class when searching for implicits. See PR #8441.
          val nextSkipClass = if (owner.isPrimaryConstructor && inSelfSuperCall) owner.owner else skipClass
          is match {
            case Nil => outer.implicitssImpl(nextSkipClass)
            case _ => is :: outer.implicitssImpl(nextSkipClass)
          }
        }

        val CycleMarker = NoRunId - 1
        if (implicitsRunId == CycleMarker) {
          debuglog(s"cycle while collecting implicits at owner ${owner}, probably due to an implicit without an explicit return type. Continuing with implicits from enclosing contexts.")
          withOuter(Nil)
        } else if (implicitsRunId != currentRunId) {
          implicitsRunId = CycleMarker
          implicits match {
            case None =>
              implicitsRunId = NoRunId
              withOuter(Nil)
            case Some(is) =>
              implicitsRunId = currentRunId
              implicitsCache = is
              withOuter(is)
          }
        }
        else withOuter(implicitsCache)
      }
    }

    /** @return None if a cycle is detected, or Some(infos) containing the in-scope implicits at this context */
    private def implicits: Option[List[ImplicitInfo]] = {
      val firstImport = this.firstImport
      if (unit.isJava) SomeOfNil
      else if (owner != outer.owner && owner.isClass && !owner.isPackageClass) {
        if (!owner.isInitialized) None
        else savingEnclClass(this) {
          // !!! In the body of `class C(implicit a: A) { }`, `implicitss` returns `List(List(a), List(a), List(<predef..)))`
          //     it handled correctly by implicit search, which considers the second `a` to be shadowed, but should be
          //     remedied nonetheless.
          Some(collectImplicits(owner.thisType.implicitMembers, owner.thisType))
        }
      } else if (scope != outer.scope && !owner.isPackageClass) {
        debuglog("collect local implicits " + scope.toList)//DEBUG
        Some(collectImplicits(scope, NoPrefix))
      } else if (firstImport != outer.firstImport) {
        if (isDeveloper)
          assert(imports.tail.headOption == outer.firstImport, (imports, outer.imports))
        Some(collectImplicitImports(firstImport.get))
      } else if (owner.isPackageClass) {
        // the corresponding package object may contain implicit members.
        val pre = owner.packageObject.typeOfThis
        Some(collectImplicits(pre.implicitMembers, pre))
      } else SomeOfNil
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
    private[Contexts] def resolveAmbiguousImport(name: Name, imp1: ImportInfo, imp2: ImportInfo): Option[ImportInfo] = {
      val imp1Explicit = imp1 isExplicitImport name
      val imp2Explicit = imp2 isExplicitImport name
      val ambiguous    = if (imp1.depth == imp2.depth) imp1Explicit == imp2Explicit else !imp1Explicit && imp2Explicit
      val imp1Symbol   = imp1.importedSymbol(name).initialize.filter(isAccessible(_, imp1.qual.tpe, superAccess = false))
      val imp2Symbol   = imp2.importedSymbol(name).initialize.filter(isAccessible(_, imp2.qual.tpe, superAccess = false))

      // The types of the qualifiers from which the ambiguous imports come.
      // If the ambiguous name is a value, these must be the same.
      def t1 = imp1.qual.tpe
      def t2 = imp2.qual.tpe
      // The types of the ambiguous symbols, seen as members of their qualifiers.
      // If the ambiguous name is a monomorphic type, we can relax this far.
      def mt1 = t1 memberType imp1Symbol
      def mt2 = t2 memberType imp2Symbol

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
          log(s"""Import is genuinely ambiguous:
                 |  types:  $t1 =:= $t2  ${t1 =:= t2}  members: ${mt1 =:= mt2}
                 |  member type 1: $mt1
                 |  member type 2: $mt2""".stripMargin)
          None
        }
      )
    }

    def isPackageOwnedInDifferentUnit(s: Symbol): Boolean =
      if (s.isOverloaded) s.alternatives.exists(isPackageOwnedInDifferentUnit)
      else (s.isDefinedInPackage && (
           !currentRun.compiles(s)
        || unit.exists && s.sourceFile != unit.source.file)
      )

    /** If the given import is permitted, fetch the symbol and filter for accessibility.
     *  Tests `exists` to complete SymbolLoaders, which sets the symbol's access flags (scala/bug#12736)
     */
    private[Contexts] def importedAccessibleSymbol(imp: ImportInfo, sym: => Symbol): Symbol =
      if (isExcludedRootImport(imp)) NoSymbol
      else sym.filter(s => s.exists && isAccessible(s, imp.qual.tpe, superAccess = false))

    private def isExcludedRootImport(imp: ImportInfo): Boolean =
      imp.isRootImport && excludedRootImportsCached.get(unit).exists(_.contains(imp.qual.symbol))

    private[Contexts] def requiresQualifier(s: Symbol): Boolean = (
          s.owner.isClass
      && !s.owner.isPackageClass
      && !s.isTypeParameterOrSkolem
      && !s.isExistentiallyBound
    )

    /** Must `sym` defined in package object of package `pkg`, if
     *  it selected from a prefix with `pkg` as its type symbol?
     */
    def isInPackageObject(sym: Symbol, pkg: Symbol): Boolean = {
      if (sym.isOverloaded) sym.alternatives.exists(alt => isInPackageObject(alt, pkg))
      else pkg.hasPackageFlag && sym.owner != pkg && requiresQualifier(sym)
    }

    def isNameInScope(name: Name) = lookupSymbol(name, _ => true).isSuccess

    def lookupSymbol(name: Name, qualifies: Symbol => Boolean): NameLookup =
      symbolLookupCache.using(_(this, name)(qualifies))

    final def lookupCompanionInIncompleteOwner(original: Symbol): Symbol = {
      // Must have both a class and module symbol, so that `{ class C; def C }` or `{ type T; object T }` are not companions.
      def isCompanion(sym: Symbol): Boolean =
        (original.isModule && sym.isClass || sym.isModule && original.isClass) && sym.isCoDefinedWith(original)
      lookupSibling(original, original.name.companionName).filter(isCompanion)
    }

    final def lookupSibling(original: Symbol, name: Name): Symbol = {
      /* Search scopes in current and enclosing contexts for the definition of `symbol` */
      def lookupScopeEntry(symbol: Symbol): ScopeEntry = {
        var res: ScopeEntry = null
        var ctx = this
        while (res == null && ctx.outer != ctx) {
          val s = ctx.scope lookupSymbolEntry symbol
          if (s != null)
            res = s
          else
            ctx = ctx.outer
        }
        res
      }

      // Must be owned by the same Scope, to ensure that in
      // `{ class C; { ...; object C } }`, the class is not seen as a companion of the object.
      lookupScopeEntry(original) match {
        case null => NoSymbol
        case entry =>
          entry.owner.lookupNameInSameScopeAs(original, name)
      }
    }

    final def javaFindMember(pre: Type, name: Name, qualifies: Symbol => Boolean): (Type, Symbol) = {
      val preSym = pre.typeSymbol
      val sym = {
        def asModule =
          if (name.isTypeName && nme.isModuleName(name))
            pre.member(name.dropModule.toTermName) match {
              case nope @ NoSymbol => nope
              case member          => member.filter(qualifies).moduleClass
            }
          else NoSymbol
        pre.member(name) match {
          case NoSymbol => asModule
          case member   => member.filter(qualifies)
        }
      }
      if (sym.exists || preSym.isPackageClass || !preSym.isClass) (pre, sym)
      else {
        // In Java code, static inner classes, which we model as members of the companion object,
        // can be referenced from an ident in a subclass or by a selection prefixed by the subclass.
        val toSearch = if (preSym.isModuleClass) companionSymbolOf(pre.typeSymbol.sourceModule, this).baseClasses else preSym.baseClasses
        toSearch.iterator.map { bc =>
          val pre1 = bc.typeOfThis
          val found = pre1.decl(name)
          found.filter(qualifies) match {
            case NoSymbol =>
              val companionModule = companionSymbolOf(pre1.typeSymbol, this)
              val pre2 = companionModule.typeOfThis
              val found = pre2.decl(name).filter(qualifies)
              found match {
                case NoSymbol => NoJavaMemberFound
                case sym => (pre2, sym)
              }
            case sym => (pre1, sym)
          }
        }.find(_._2 ne NoSymbol).getOrElse(NoJavaMemberFound)
      }
    }

    private def isReplImportWrapperImport(tree: Tree): Boolean = {
      tree match {
        case Import(expr, selector :: Nil) =>
          // Just a syntactic check to avoid forcing typechecking of imports
          selector.name.string_==(nme.INTERPRETER_IMPORT_LEVEL_UP) && owner.enclosingTopLevelClass.isInterpreterWrapper
        case _ => false
      }
    }

  } //class Context

  /** Find the symbol of a simple name starting from this context.
   *  All names are filtered through the "qualifies" predicate,
   *  the search continuing as long as no qualifying name is found.
   */
  // OPT: moved this into a (cached) object to avoid costly and non-eliminated {Object,Int}Ref allocations
  private[Contexts] final val symbolLookupCache = ReusableInstance[SymbolLookup](new SymbolLookup, enabled = isCompilerUniverse)
  private[Contexts] final class SymbolLookup {
    private[this] var lookupError: NameLookup  = _ // set to non-null if a definite error is encountered
    private[this] var inaccessible: NameLookup = _ // records inaccessible symbol for error reporting in case none is found
    private[this] var defSym: Symbol           = _ // the directly found symbol
    private[this] var pre: Type                = _ // the prefix type of defSym, if a class member
    private[this] var cx: Context              = _ // the context under consideration
    private[this] var symbolDepth: Int         = _ // the depth of the directly found symbol
    private[this] var foundInPrefix: Boolean   = _ // the symbol was found in pre
    private[this] var foundInSuper: Boolean    = _ // the symbol was found super of context class (inherited)

    def ambiguousImports(imp1: ImportInfo, imp2: ImportInfo) =
      LookupAmbiguous(s"it is imported twice in the same scope by\n$imp1\nand $imp2")
    def ambiguousDefnAndImport(owner: Symbol, imp: ImportInfo) =
      LookupAmbiguous(s"it is both defined in $owner and imported subsequently by \n$imp")
    def ambiguousDefinitions(outer: Symbol, inherited: Symbol, foundInSuper: Boolean, classOfInherited: Symbol, currentClass: Symbol) =
      if (foundInSuper) {
        if (inherited.isImplicit) None
        else {
          val outer1 = outer.alternatives.head
          val inherited1 = inherited.alternatives.head
          val classDesc = if (classOfInherited.isAnonymousClass) "anonymous class" else classOfInherited.toString
          val parent = classOfInherited.parentSymbols.find(_.isNonBottomSubClass(inherited1.owner)).getOrElse(NoSymbol)
          val inherit = if (parent.exists && parent != inherited1.owner) s", inherited through parent $parent" else ""
          val fix = if (classOfInherited != currentClass) s"${classOfInherited.name}.this." else "this."
          val message =
            sm"""|it is both defined in the enclosing ${outer1.owner} and inherited in the enclosing $classDesc as $inherited1 (defined in ${inherited1.ownsString}$inherit)
                 |In Scala 2, symbols inherited from a superclass shadow symbols defined in an outer scope.
                 |Such references are ambiguous in Scala 3. To continue using the inherited symbol, write `${fix}${outer1.name}`."""
          inherited.updateAttachment(LookupAmbiguityWarning(
            sm"""|reference to ${outer1.name} is ambiguous;
                 |$message
                 |Or use `-Wconf:msg=legacy-binding:s` to silence this warning.""", fix))
        }
        None
      } else
        Some(LookupAmbiguous(s"it is both defined in ${outer.owner} and available as ${inherited.fullLocationString}"))

    def apply(thisContext: Context, name: Name)(qualifies: Symbol => Boolean): NameLookup = {
      lookupError  = null
      inaccessible = null
      defSym       = NoSymbol
      pre          = NoPrefix
      cx           = thisContext
      symbolDepth  = -1
      foundInPrefix = false
      foundInSuper = false

      def finish(qual: Tree, sym: Symbol): NameLookup = (
        if (lookupError ne null) lookupError
        else sym match {
          case NoSymbol if inaccessible ne null => inaccessible
          case NoSymbol                         => LookupNotFound
          case _                                => LookupSucceeded(qual, sym)
        }
      )
      def finishDefSym(sym: Symbol, pre0: Type): NameLookup = {
        val qual =
          if (!thisContext.unit.isJava && thisContext.requiresQualifier(sym)) gen.mkAttributedQualifier(pre0)
          else EmptyTree
        finish(qual, sym)
      }

      def lookupInPrefix(name: Name): Symbol =
        if (thisContext.unit.isJava)
          thisContext.javaFindMember(pre, name, qualifies) match {
            case (_, NoSymbol) =>
              NoSymbol
            case (pre1, sym) =>
              pre = pre1
              sym
          }
        else
          pre.member(name).filter(qualifies)

      def accessibleInPrefix(s: Symbol) =
        thisContext.isAccessible(s, pre, superAccess = false)

      def searchPrefix = {
        cx = cx.enclClass
        val found0 = lookupInPrefix(name)
        val found1 = found0.filter(accessibleInPrefix)
        if (found0.exists && !found1.exists && inaccessible == null)
          inaccessible = LookupInaccessible(found0, analyzer.lastAccessCheckDetails)

        found1
      }

      def lookupInScope(owner: Symbol, pre: Type, scope: Scope): Symbol = {
        var e = scope.lookupEntry(name)
        while (e != null && !qualifies(e.sym)) {
          e = scope.lookupNextEntry(e)
        }
        if (e == null) {
          NoSymbol
        } else {
          val e1 = e
          val e1Sym = e.sym
          var syms: mutable.ListBuffer[Symbol] = null
          e = scope.lookupNextEntry(e)
          while (e ne null) {
            if (e.depth == e1.depth && e.sym != e1Sym && qualifies(e.sym)) {
              if (syms eq null) {
                syms = new mutable.ListBuffer[Symbol]
                syms += e1Sym
              }
              syms += e.sym
            }
            e = scope.lookupNextEntry(e)
          }
          // we have a winner: record the symbol depth
          symbolDepth = (cx.depth - cx.scope.nestingLevel) + e1.depth

          if (syms eq null) e1Sym
          else owner.newOverloaded(pre, syms.toList)
        }
      }

      // Constructor lookup should only look in the decls of the enclosing class
      // not in the self-type, nor in the enclosing context, nor in imports (scala/bug#4460, scala/bug#6745)
      if (name == nme.CONSTRUCTOR) {
        val enclClassSym = cx.enclClass.owner
        val scope = cx.enclClass.prefix.baseType(enclClassSym).decls
        val constructorSym = lookupInScope(enclClassSym, cx.enclClass.prefix, scope)
        return finishDefSym(constructorSym, cx.enclClass.prefix)
      }

      // cx.scope eq null arises during FixInvalidSyms in Duplicators
      def nextDefinition(lastDef: Symbol, lastPre: Type): Unit = {
        var inPrefix = false
        defSym = NoSymbol
        while (defSym == NoSymbol && (cx ne NoContext) && (cx.scope ne null)) {
          pre    = cx.enclClass.prefix
          defSym = lookupInScope(cx.owner, pre, cx.scope) match {
            case NoSymbol => inPrefix = true; searchPrefix
            case found    => inPrefix = false; found
          }
          if (!defSym.exists) cx = cx.outer // push further outward
        }
        if ((defSym.isAliasType || lastDef.isAliasType) && pre.memberType(defSym) =:= lastPre.memberType(lastDef))
          defSym = NoSymbol
        if (defSym.isStable && lastDef.isStable &&
          (lastPre.memberType(lastDef).termSymbol == defSym || pre.memberType(defSym).termSymbol == lastDef))
          defSym = NoSymbol
        foundInPrefix = inPrefix && defSym.exists
        foundInSuper  = foundInPrefix && defSym.alternatives.forall(_.owner != cx.owner)
      }
      nextDefinition(NoSymbol, NoPrefix)

      if (symbolDepth < 0)
        symbolDepth = cx.depth

      var impSel: ImportSelector = null
      var impSym: Symbol = NoSymbol
      val importCursor = new ImportCursor(thisContext, name)
      import importCursor.{imp1, imp2}

      // The symbol resolved by the given import for `name`, paired with the selector that was used.
      // If `requireExplicit`, then only "named" or "specific" selectors are considered.
      // In addition, the symbol must be accessible (in the current context) and satisfy the `qualifies` predicate.
      def lookupImport(imp: ImportInfo, requireExplicit: Boolean): (ImportSelector, Symbol) = {
        val pair @ (sel, sym) = imp.importedSelectedSymbol(name, requireExplicit)
        if (sym == NoSymbol) pair
        else {
          val sym1 = thisContext.importedAccessibleSymbol(imp, sym).filter(qualifies)
          if (sym1 eq sym) pair
          else (sel, sym1)
        }
      }

      /* Java: A single-type-import declaration d in a compilation unit c of package p
       * that imports a type named n shadows, throughout c, the declarations of:
       *
       *  1) any top level type named n declared in another compilation unit of p
       *
       * A type-import-on-demand declaration never causes any other declaration to be shadowed.
       *
       * Scala: Bindings of different kinds have a defined precedence order:
       *
       *  1) Definitions and declarations in lexical scope have the highest precedence.
       *  1b) Definitions and declarations that are either inherited, or made
       *      available by a package clause and also defined in the same compilation unit
       *      as the reference to them, have the next highest precedence.
       *  2) Explicit imports have next highest precedence.
       *  3) Wildcard imports have next highest precedence.
       *  4) Bindings made available by a package clause,
       *     but not also defined in the same compilation unit as the reference to them,
       *     as well as bindings supplied by the compiler but not explicitly written in source code,
       *     have the lowest precedence.
       */

      /* Level 4 (see above) */
      def foreignDefined = defSym.exists && thisContext.isPackageOwnedInDifferentUnit(defSym)  // SI-2458

      // Find the first candidate import
      def advanceCursorToNextImport(): Unit = {
        val defIsLevel4 = foreignDefined
        // can the import at this depth compete with the definition?
        // If not, we can stop inspecting outer scopes (including more imports).
        // A competing import can either shadow the definition or render it ambiguous.
        //
        @inline def importCanShadowAtDepth(imp: ImportInfo) = {
          @inline def importCompetesWithDefinition =
            if (thisContext.unit.isJava) imp.depth == symbolDepth && defIsLevel4
            else defIsLevel4
          !cx(ContextMode.InPackageClauseName) &&
            (imp.depth > symbolDepth || importCompetesWithDefinition)
        }

        while (!impSym.exists && importCursor.imp1Exists && importCanShadowAtDepth(importCursor.imp1)) {
          val javaRule = thisContext.unit.isJava && defIsLevel4
          val (sel, sym) = lookupImport(imp1, requireExplicit = javaRule)
          impSel = sel
          impSym = sym
          if (!impSym.exists)
            importCursor.advanceImp1Imp2()
        }
      }
      advanceCursorToNextImport()

      val preferDef: Boolean = defSym.exists && (!impSym.exists || {
        // Does the import just import the defined symbol?
        def reconcileAmbiguousImportAndDef: Boolean = {
          val res = impSym == defSym
          if (res) log(s"Suppressing ambiguous import, taking $defSym for $name")
          res
        }
        // 4) root imported symbols have same (lowest) precedence as package-owned symbols in different compilation units.
        if (imp1.depth < symbolDepth && imp1.isRootImport && foreignDefined)
          true
        // 4) imported symbols have higher precedence than package-owned symbols in different compilation units.
        //    except that in Java, the import must be "explicit" (level 2)
        else if (thisContext.unit.isJava && imp1.depth == symbolDepth && foreignDefined)
          !importCursor.imp1Explicit
        else if (!thisContext.unit.isJava && imp1.depth >= symbolDepth && foreignDefined)
          false
        // Defined symbols take precedence over erroneous imports.
        else if (impSym.isError || impSym.name == nme.CONSTRUCTOR)
          true
        // Try to reconcile them before giving up
        else if (foreignDefined && reconcileAmbiguousImportAndDef)
          true
        // Otherwise they are irreconcilably ambiguous
        else
          return ambiguousDefnAndImport(defSym.alternatives.head.owner, imp1)
      })

      // If the defSym is at 4, and there is a def at 1b in scope due to packaging, then the reference is ambiguous.
      // Also if defSym is at 1b inherited, the reference can be rendered ambiguous by a def at 1a in scope.
      val possiblyAmbiguousDefinition =
        foundInSuper && cx.owner.isClass ||
        foreignDefined && !defSym.hasPackageFlag
      if (possiblyAmbiguousDefinition && !thisContext.unit.isJava) {
        val defSym0 = defSym
        val pre0    = pre
        val cx0     = cx
        val depth0  = symbolDepth
        val wasFoundInSuper = foundInSuper
        val foundCompetingSymbol: () => Boolean =
          if (foreignDefined)
            // if the first found symbol (defSym0) is level 4 (foreignDefined), a lower level (1 or 1b) defSym is competing
            () => defSym.exists && !foreignDefined
          else {
            // if defSym0 is level 1 or 1b, another defSym is competing if defined in an outer scope in the same file
            () => defSym.exists && !(pre.typeSymbol.isPackageClass && !defSym.owner.isPackageClass) && !foundInSuper && !foreignDefined
            //                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            //                       defined in package object (or inherited into package object)
          }
        while ((cx ne NoContext) && cx.depth >= symbolDepth) cx = cx.outer
        if (wasFoundInSuper)
          while ((cx ne NoContext) && (cx.owner eq cx0.owner)) cx = cx.outer
        var done = false
        while (!done) {
          nextDefinition(defSym0, pre0)
          done = (cx eq NoContext) || foundCompetingSymbol()
          if (!done && (cx ne NoContext)) cx = cx.outer
        }
        val nonOverlapping = defSym.exists && {
          if (defSym.isOverloaded || defSym0.isOverloaded) !defSym.alternatives.exists(defSym0.alternatives.contains)
          else defSym ne defSym0
        }
        if (nonOverlapping) {
          val ambiguity =
            if (preferDef) ambiguousDefinitions(defSym, defSym0, wasFoundInSuper, cx0.enclClass.owner, thisContext.enclClass.owner)
            else Some(ambiguousDefnAndImport(owner = defSym.owner, imp1))
          if (ambiguity.nonEmpty) return ambiguity.get
        }
        defSym = defSym0
        pre    = pre0
        cx     = cx0
        symbolDepth = depth0
      }

      if (preferDef) impSym = NoSymbol else defSym = NoSymbol

      // At this point only one or the other of defSym and impSym might be set.
      if (defSym.exists) finishDefSym(defSym, pre)
      else if (impSym.exists) {
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
        while (lookupError == null && importCursor.keepLooking) {
          // If not at the same depth, only an explicit import can induce an ambiguity.
          val (sel, other) = lookupImport(imp2, requireExplicit = !importCursor.sameDepth)

          @inline def imp1wins(): Unit = { importCursor.advanceImp2() }
          @inline def imp2wins(): Unit = { impSel = sel ; impSym = other ; importCursor.advanceImp1Imp2() }
          if (!other.exists) // imp1 wins; drop imp2 and continue.
            imp1wins()
          else if (importCursor.imp2Wins) // imp2 wins; drop imp1 and continue.
            imp2wins()
          else thisContext.resolveAmbiguousImport(name, imp1, imp2) match {
            case Some(imp) => if (imp eq imp1) imp1wins() else imp2wins()
            case _         => lookupError = ambiguousImports(imp1, imp2)
          }
        }

        // the choice has been made
        if (lookupError == null) {
          // implicit searcher decides when import was used
          if (thisContext.contextMode.inNone(InImplicitSearch))
            imp1.recordUsage(impSel, impSym)

          // optimization: don't write out package prefixes
          finish(duplicateAndResetPos.transform(imp1.qual), impSym)
        }
        else finish(EmptyTree, NoSymbol)
      }
      else finish(EmptyTree, NoSymbol)
    }
  }

  /** A `Context` focussed on an `Import` tree */
  final class ImportContext private[Contexts] (
                            tree: Tree, owner: Symbol, scope: Scope,
                            unit: CompilationUnit, outer: Context,
                            override val isRootImport: Boolean, depth: Int,
                            reporter: ContextReporter) extends Context(tree, owner, scope, unit, outer, depth, reporter) {
    private[this] val impInfo: ImportInfo = new ImportInfo(tree.asInstanceOf[Import], outerDepth, isRootImport)

    override final def imports      = impInfo :: super.imports
    override final def firstImport  = Some(impInfo)
    override final def importOrNull = impInfo

    override final def toString     = s"${super.toString} with ImportContext { $impInfo; outer.owner = ${outer.owner} }"
  }

  /** A reporter for use during type checking. It has multiple modes for handling errors.
   *
   *  The default (immediate mode) is to send the error to the global reporter.
   *  When switched into buffering mode via makeBuffering, errors and warnings are buffered and not be reported
   *  (there's a special case for ambiguity errors for some reason: those are force to the reporter when context.ambiguousErrors,
   *   or else they are buffered -- TODO: can we simplify this?)
   *
   *  When using the type checker after typers, an error results in a TypeError being thrown. TODO: get rid of this mode.
   *
   *  To handle nested contexts, reporters share buffers. TODO: only buffer in BufferingReporter, emit immediately in ImmediateReporter
   */
  abstract class ContextReporter(private[this] var _errorBuffer: mutable.LinkedHashSet[AbsTypeError] = null, private[this] var _warningBuffer: mutable.LinkedHashSet[ContextWarning] = null) {
    def issue(err: AbsTypeError)(implicit context: Context): Unit = errorAndDumpIfDebug(context.fixPosition(err.errPos), addDiagString(err.errMsg), err.actions)

    def echo(msg: String): Unit = echo(NoPosition, msg)
    def echo(pos: Position, msg: String): Unit = reporter.echo(pos, msg)

    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol, actions: List[CodeAction] = Nil): Unit =
      runReporting.warning(pos, msg, category, site, actions)

    def error(pos: Position, msg: String, actions: List[CodeAction]): Unit

    final def errorAndDumpIfDebug(pos: Position, msg: String, actions: List[CodeAction]): Unit = {
      error(pos, msg, actions)
      if (settings.VdebugTypeError.value) {
        Thread.dumpStack()
      }
    }

    protected def handleSuppressedAmbiguous(err: AbsAmbiguousTypeError): Unit = ()

    def makeImmediate: ContextReporter = this
    def makeBuffering: ContextReporter = this
    def isBuffering: Boolean           = false
    def isThrowing: Boolean            = false

    /** Emit an ambiguous error according to context.ambiguousErrors
     *
     *  - when true, use global.reporter regardless of whether we're buffering (TODO: can we change this?)
     *  - else, let this context reporter decide
     */
    final def issueAmbiguousError(err: AbsAmbiguousTypeError)(implicit context: Context): Unit =
      if (context.ambiguousErrors) reporter.error(context.fixPosition(err.errPos), addDiagString(err.errMsg), err.actions) // force reporting... see TODO above
      else handleSuppressedAmbiguous(err)

    @inline final def withFreshErrorBuffer[T](expr: => T): T = {
      val previousBuffer = _errorBuffer
      _errorBuffer = null
      val res = expr // expr will read _errorBuffer
      _errorBuffer = previousBuffer
      res
    }

    final def propagateErrorsTo[T](target: ContextReporter): Unit = {
      if (this ne target) {  // `this eq target` in e.g., test/files/neg/divergent-implicit.scala
        if (hasErrors) {
          // assert(target.errorBuffer ne _errorBuffer)
          if (target.isBuffering) {
            target ++= errors
          } else {
            errors.foreach(e => target.errorAndDumpIfDebug(e.errPos, e.errMsg, e.actions))
          }
          // TODO: is clearAllErrors necessary? (no tests failed when dropping it)
          // NOTE: even though `this ne target`, it may still be that `target.errorBuffer eq _errorBuffer`,
          // so don't clear the buffer, but null out the reference so that a new one will be created when necessary (should be never??)
          // (we should refactor error buffering to avoid mutation on shared buffers)
          clearAllErrors()
        }
        // TODO propagate warnings if no errors, like `silent` does?
      }
    }

    final def hasErrors: Boolean = _errorBuffer != null && errorBuffer.nonEmpty

    // TODO: everything below should be pushed down to BufferingReporter (related to buffering)
    // Implicit relies on this most heavily, but there you know reporter.isInstanceOf[BufferingReporter]
    // can we encode this statically?

    // have to pass in context because multiple contexts may share the same ContextReporter
    def reportFirstDivergentError(fun: Tree, param: Symbol, paramTp: Type)(implicit context: Context): Unit =
      errors.collectFirst {
        case dte: DivergentImplicitTypeError => dte
      } match {
        case Some(divergent) =>
          // DivergentImplicit error has higher priority than "no implicit found"
          // no need to issue the problem again if we are still in silent mode
          if (context.reportErrors) {
            context.issue(divergent.withPt(paramTp))
            errorBuffer.filterInPlace {
              case dte: DivergentImplicitTypeError => false
              case _ => true
            }
          }
        case _ =>
          NoImplicitFoundError(fun, param)(context)
      }

    def retainDivergentErrorsExcept(saved: DivergentImplicitTypeError) =
      errorBuffer.filterInPlace {
        case err: DivergentImplicitTypeError => err ne saved
        case _ => false
      }

    def propagateImplicitTypeErrorsTo(target: ContextReporter) = {
      errors foreach {
        case err@(_: DivergentImplicitTypeError | _: AmbiguousImplicitTypeError) =>
          target.errorBuffer += err
        case _ =>
      }
      // debuglog("propagateImplicitTypeErrorsTo: " + errors)
    }

    protected def addDiagString(msg: String)(implicit context: Context): String = {
      val diagUsedDefaultsMsg = "Error occurred in an application involving default arguments."
      if (context.contextMode.inAny(ContextMode.DiagUsedDefaults) && !(msg endsWith diagUsedDefaultsMsg)) msg + "\n" + diagUsedDefaultsMsg
      else msg
    }

    final def emitWarnings() = if (_warningBuffer != null) {
      _warningBuffer foreach {
        case ContextWarning(pos, msg, category, site, actions) => runReporting.warning(pos, msg, category, site, actions)
      }
      _warningBuffer = null
    }

    // [JZ] Contexts, pre- the scala/bug#7345 refactor, avoided allocating the buffers until needed. This
    // is replicated here out of conservatism.
    private def newBuffer[A] = mutable.LinkedHashSet.empty[A] // Important to use LinkedHS for stable results.
    final protected def errorBuffer = { if (_errorBuffer == null) _errorBuffer = newBuffer; _errorBuffer }
    final protected def warningBuffer = { if (_warningBuffer == null) _warningBuffer = newBuffer; _warningBuffer }

    final def errors: Seq[AbsTypeError]        = errorBuffer.toVector
    final def warnings: Seq[ContextWarning]    = warningBuffer.toVector
    final def firstError: Option[AbsTypeError] = errorBuffer.headOption

    // TODO: remove ++= and clearAll* entirely in favor of more high-level combinators like withFreshErrorBuffer
    final private[typechecker] def ++=(errors: Iterable[AbsTypeError]): Unit = errorBuffer ++= errors

    // null references to buffers instead of clearing them,
    // as the buffers may be shared between different reporters
    final def clearAll(): Unit       = { _errorBuffer = null; _warningBuffer = null }
    final def clearAllErrors(): Unit = { _errorBuffer = null }
  }

  private[typechecker] class ImmediateReporter(_errorBuffer: mutable.LinkedHashSet[AbsTypeError] = null, _warningBuffer: mutable.LinkedHashSet[ContextWarning] = null) extends ContextReporter(_errorBuffer, _warningBuffer) {
    override def makeBuffering: ContextReporter = new BufferingReporter(errorBuffer, warningBuffer)
    def error(pos: Position, msg: String, actions: List[CodeAction]): Unit = runReporting.error(pos, msg, actions)
 }

  private[typechecker] class BufferingReporter(_errorBuffer: mutable.LinkedHashSet[AbsTypeError] = null, _warningBuffer: mutable.LinkedHashSet[ContextWarning] = null) extends ContextReporter(_errorBuffer, _warningBuffer) {
    override def isBuffering = true

    override def issue(err: AbsTypeError)(implicit context: Context): Unit             = errorBuffer += err

    // this used to throw new TypeError(pos, msg) -- buffering lets us report more errors (test/files/neg/macro-basic-mamdmi)
    // the old throwing behavior was relied on by diagnostics in manifestOfType
    def error(pos: Position, msg: String, actions: List[CodeAction]): Unit = errorBuffer += TypeErrorWrapper(new TypeError(pos, msg), actions)

    override def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol, actions: List[CodeAction]): Unit =
      warningBuffer += ContextWarning(pos, msg, category, site, actions)

    override protected def handleSuppressedAmbiguous(err: AbsAmbiguousTypeError): Unit = errorBuffer += err

    // TODO: emit all buffered errors, warnings
    override def makeImmediate: ContextReporter = new ImmediateReporter(errorBuffer, warningBuffer)
  }

  /** Used after typer (specialization relies on TypeError being thrown, among other post-typer phases).
   *
   * TODO: get rid of it, use ImmediateReporter and a check for reporter.hasErrors where necessary
   */
  private[typechecker] class ThrowingReporter extends ContextReporter {
    override def isThrowing = true
    def error(pos: Position, msg: String, actions: List[CodeAction]): Unit = throw new TypeError(pos, msg)
  }

  /** Used during a run of [[scala.tools.nsc.typechecker.TreeCheckers]]? */
  private[typechecker] class CheckingReporter extends ContextReporter {
    def error(pos: Position, msg: String, actions: List[CodeAction]): Unit = onTreeCheckerError(pos, msg)
  }

  class ImportInfo(val tree: Import, val depth: Int, val isRootImport: Boolean) {
    def pos = tree.pos
    def posOf(sel: ImportSelector) =
      if (sel.namePos >= 0) tree.pos withPoint sel.namePos else tree.pos

    /** The prefix expression */
    def qual: Tree = tree.symbol.info match {
      case ImportType(expr) => expr
      case ErrorType        => tree setType NoType // fix for #2870
      case bad              => throw new FatalError(s"symbol ${tree.symbol} has bad type: ${bad}")
    }

    /** Is name imported explicitly, not via wildcard? */
    def isExplicitImport(name: Name): Boolean = tree.selectors.exists(_.introduces(name))

    /** The symbol with name `name` imported from import clause `tree`. */
    def importedSymbol(name: Name): Symbol = importedSelectedSymbol(name, requireExplicit = false)._2

    /** If requireExplicit is true, wildcard imports are not considered. */
    def importedSelectedSymbol(name: Name, requireExplicit: Boolean): (ImportSelector, Symbol) = {
      var result: Symbol = NoSymbol
      var renamed = false
      var selectors = tree.selectors
      @inline def current = selectors.head
      @inline def maybeNonLocalMember(nom: Name): Symbol =
        if (qual.tpe.isError) NoSymbol
        else if (pos.source.isJava) {
          val (_, sym) = NoContext.javaFindMember(qual.tpe, nom, _ => true)
          // We don't need to propagate the new prefix back out to the result of `Context.lookupSymbol`
          // because typechecking .java sources doesn't need it.
          sym
        }
        else {
          val tp = qual.tpe
          val sym = tp.typeSymbol
          // opening package objects is delayed (scala/scala#9661), but that can lead to missing symbols for
          // package object types that are forced early through Definitions; see scala/bug#12740 / scala/scala#10333
          if (phase.id < currentRun.typerPhase.id && sym.hasPackageFlag && analyzer.packageObjects.deferredOpen.remove(sym))
            openPackageModule(sym)
          tp.nonLocalMember(nom)
        }
      while ((selectors ne Nil) && result == NoSymbol) {
        if (current.introduces(name))
          result = maybeNonLocalMember(current.name asTypeOf name)
        else if (!current.isWildcard && current.hasName(name))
          renamed = true
        else if (current.isWildcard && !renamed && !requireExplicit)
          result = maybeNonLocalMember(name)

        if (result == NoSymbol)
          selectors = selectors.tail
      }

      // Harden against the fallout from bugs like scala/bug#6745 and #5389
      // Enforce no importing universal members from root import Predef modules.
      //
      // [JZ] I considered issuing a devWarning and moving the
      //      check inside the above loop, as I believe that
      //      this always represents a mistake on the part of
      //      the caller.
      result.filter(sym =>
        if (isRootImport) !definitions.isUnimportableUnlessRenamed(sym)
        else definitions.isImportable(sym)
      ) match {
        case filtered: NoSymbol => TupleOfNullAndNoSymbol
        case _                  => (current, result)
      }
    }

    def fullSelectorString(s: ImportSelector): String =
      s"${if (qual.tpe.isError) tree.toString else qual.tpe.typeSymbol.fullName}.${selectorString(s)}"

    private def selectorString(s: ImportSelector): String =
      if (s.isWildcard) "_"
      else if (s.isRename) s"${s.name} => ${s.rename}"
      else s.name.decoded

    /** Optionally record that a selector was used to import the given symbol. */
    def recordUsage(sel: ImportSelector, result: Symbol): Unit = {
      debuglog(s"In $this at ${ pos.source.file.name }:${ posOf(sel).line }, selector '${ selectorString(sel)
        }' resolved to ${
          if (tree.symbol.hasCompleteInfo) s"(qual=$qual, $result)"
          else s"(expr=${tree.expr}, ${result.fullLocationString})"
        }")
      if (settings.warnUnusedImport && !isRootImport && result != NoSymbol && pos != NoPosition)
        allUsedSelectors(this) += sel
    }

    def allImportedSymbols: Iterable[Symbol] =
      importableMembers(qual.tpe).flatMap(transformImport(tree.selectors, _))

    @tailrec
    private def transformImport(selectors: List[ImportSelector], sym: Symbol): List[Symbol] = selectors match {
      case Nil => Nil
      case sel :: Nil if sel.isWildcard =>
        if (isRootImport && definitions.isUnimportableUnlessRenamed(sym)) Nil
        else List(sym)
      case (sel @ ImportSelector(from, _, to, _)) :: _ if from == (if (from.isTermName) sym.name.toTermName else sym.name.toTypeName) =>
        if (sel.isMask) Nil
        else List(sym.cloneSymbol(sym.owner, sym.rawflags, to))
      case _ :: rest => transformImport(rest, sym)
    }

    override def hashCode = tree.##
    override def equals(other: Any) = other match {
      case that: ImportInfo => tree == that.tree
      case _                => false
    }
    override def toString = tree.toString
  }

  type ImportType = global.ImportType
  val ImportType = global.ImportType

  /** Walks a pair of references (`imp1` and `imp2`) up the context chain to ImportContexts */
  private final class ImportCursor(var ctx: Context, name: Name) {
    private var imp1Ctx = ctx.enclosingImport
    private var imp2Ctx = imp1Ctx.outer.enclosingImport

    def advanceImp1Imp2(): Unit = {
      imp1Ctx = imp2Ctx; imp2Ctx = imp1Ctx.outer.enclosingImport
    }
    def advanceImp2(): Unit = {
      imp2Ctx = imp2Ctx.outer.enclosingImport
    }
    def imp1Exists: Boolean = imp1Ctx.importOrNull != null
    def imp1: ImportInfo = imp1Ctx.importOrNull
    def imp2: ImportInfo = imp2Ctx.importOrNull

    // We continue walking down the imports as long as the tail is non-empty, which gives us:
    //   imports  ==  imp1 :: imp2 :: _
    // And at least one of the following is true:
    //   - imp1 and imp2 are at the same depth
    //   - imp1 is a wildcard import, so all explicit imports from outer scopes must be checked
    def keepLooking: Boolean = imp2Exists && (sameDepth || !imp1Explicit)
    def imp2Wins: Boolean = sameDepth && !imp1Explicit && imp2Explicit
    def sameDepth: Boolean = imp1.depth == imp2.depth

    private def imp2Exists = imp2Ctx.importOrNull != null
    def imp1Explicit = imp1 isExplicitImport name
    private def imp2Explicit = imp2 isExplicitImport name
  }

  private val TupleOfNullAndNoSymbol = (null, NoSymbol)
}

object ContextMode {
  import scala.language.implicitConversions
  private implicit def liftIntBitsToContextState(bits: Int): ContextMode = apply(bits)
  def apply(bits: Int): ContextMode = new ContextMode(bits)
  final val NOmode: ContextMode                   = 0

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

  /** Should a dead code warning be issued for a Nothing-typed argument to the current application. */
  final val SuppressDeadArgWarning: ContextMode   = 1 << 17

  /** Were default arguments used? */
  final val DiagUsedDefaults: ContextMode         = 1 << 18

  /** Are we currently typing the core or args of an annotation?
   *  When set, Java annotations may be instantiated directly.
   */
  final val TypingAnnotation: ContextMode         = 1 << 19

  final val InPackageClauseName: ContextMode      = 1 << 20

  /** Context created with makeImplicit, for use in implicit search.
   *  Controls whether import elements are marked used on lookup.
   */
  final val InImplicitSearch: ContextMode         = 1 << 21

  /** TODO: The "sticky modes" are EXPRmode, PATTERNmode, TYPEmode.
   *  To mimic the sticky mode behavior, when captain stickyfingers
   *  comes around we need to propagate those modes but forget the other
   *  context modes which were once mode bits; those being so far the
   *  ones listed here.
   */
  final val FormerNonStickyModes: ContextMode = (
    PatternAlternative | StarPatterns | SuperInit | SecondTry | ReturnExpr | TypeConstructorAllowed
  )

  final val DefaultMode: ContextMode = MacrosEnabled

  private val contextModeNameMap = Map(
    AmbiguousErrors        -> "AmbiguousErrors",
    ConstructorSuffix      -> "ConstructorSuffix",
    SelfSuperCall          -> "SelfSuperCall",
    ImplicitsEnabled       -> "ImplicitsEnabled",
    MacrosEnabled          -> "MacrosEnabled",
    ReTyping               -> "ReTyping",
    PatternAlternative     -> "PatternAlternative",
    StarPatterns           -> "StarPatterns",
    SuperInit              -> "SuperInit",
    SecondTry              -> "SecondTry",
    TypeConstructorAllowed -> "TypeConstructorAllowed",
    DiagUsedDefaults       -> "DiagUsedDefaults",
    SuppressDeadArgWarning -> "SuppressDeadArgWarning",
    TypingAnnotation       -> "TypingAnnotation",
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
    else contextModeNameMap.view.filterKeys(inAll).values.toList.sorted.mkString(" ")
}
