/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.annotation.tailrec

/**
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Contexts { self: Analyzer =>
  import global._
  import definitions.{ JavaLangPackage, ScalaPackage, PredefModule }

  object NoContext extends Context {
    outer      = this
    enclClass  = this
    enclMethod = this

    override def nextEnclosing(p: Context => Boolean): Context = this
    override def enclosingContextChain: List[Context] = Nil
    override def implicitss: List[List[ImplicitInfo]] = Nil
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

  private val startContext = {
    NoContext.make(
    Template(List(), emptyValDef, List()) setSymbol global.NoSymbol setType global.NoType,
    rootMirror.RootClass,
    rootMirror.RootClass.info.decls)
  }

  private lazy val allUsedSelectors =
    mutable.Map[ImportInfo, Set[ImportSelector]]() withDefaultValue Set()
  private lazy val allImportInfos =
    mutable.Map[CompilationUnit, List[ImportInfo]]() withDefaultValue Nil

  def clearUnusedImports() {
    allUsedSelectors.clear()
    allImportInfos.clear()
  }
  def warnUnusedImports(unit: CompilationUnit) = {
    val imps = allImportInfos(unit).reverse.distinct

    for (imp <- imps) {
      val used = allUsedSelectors(imp)
      def isMask(s: ImportSelector) = s.name != nme.WILDCARD && s.rename == nme.WILDCARD

      imp.tree.selectors filterNot (s => isMask(s) || used(s)) foreach { sel =>
        unit.warning(imp posOf sel, "Unused import")
      }
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

    if (settings.noimports.value) Nil
    else if (unit.isJava) RootImports.javaList
    else if (settings.nopredef.value || treeInfo.noPredefImportForUnit(unit.body)) RootImports.javaAndScalaList
    else RootImports.completeList
  }

  def rootContext(unit: CompilationUnit): Context             = rootContext(unit, EmptyTree, false)
  def rootContext(unit: CompilationUnit, tree: Tree): Context = rootContext(unit, tree, false)
  def rootContext(unit: CompilationUnit, tree: Tree, erasedTypes: Boolean): Context = {
    var sc = startContext
    for (sym <- rootImports(unit)) {
      sc = sc.makeNewImport(sym)
      sc.depth += 1
    }
    val c = sc.make(unit, tree, sc.owner, sc.scope, sc.imports)
    if (erasedTypes) c.setThrowErrors() else c.setReportErrors()
    c.implicitsEnabled = !erasedTypes
    c.enrichmentEnabled = c.implicitsEnabled
    c
  }

  def resetContexts() {
    var sc = startContext
    while (sc != NoContext) {
      sc.tree match {
        case Import(qual, _) => qual setType singleType(qual.symbol.owner.thisType, qual.symbol)
        case _               =>
      }
      sc = sc.outer
    }
  }

  private object Errors {
    final val ReportErrors     = 1 << 0
    final val BufferErrors     = 1 << 1
    final val AmbiguousErrors  = 1 << 2
    final val notThrowMask     = ReportErrors | BufferErrors
    final val AllMask          = ReportErrors | BufferErrors | AmbiguousErrors
  }

  class Context private[typechecker] {
    import Errors._

    var unit: CompilationUnit = NoCompilationUnit
    var tree: Tree = _                      // Tree associated with this context
    var owner: Symbol = NoSymbol            // The current owner
    var scope: Scope = _                    // The current scope
    var outer: Context = _                  // The next outer context
    var enclClass: Context = _              // The next outer context whose tree is a
                                            // template or package definition
    @inline final def savingEnclClass[A](c: Context)(a: => A): A = {
      val saved = enclClass
      enclClass = c
      try a finally enclClass = saved
    }

    var enclMethod: Context = _             // The next outer context whose tree is a method
    var variance: Int = _                   // Variance relative to enclosing class
    private var _undetparams: List[Symbol] = List() // Undetermined type parameters,
                                                    // not inherited to child contexts
    var depth: Int = 0
    var imports: List[ImportInfo] = List()   // currently visible imports
    var openImplicits: List[(Type,Tree)] = List()   // types for which implicit arguments
                                             // are currently searched
    // for a named application block (Tree) the corresponding NamedApplyInfo
    var namedApplyBlockInfo: Option[(Tree, NamedApplyInfo)] = None
    var prefix: Type = NoPrefix
    var inConstructorSuffix = false         // are we in a secondary constructor
                                            // after the this constructor call?
    var returnsSeen = false                 // for method context: were returns encountered?
    var inSelfSuperCall = false             // is this context (enclosed in) a constructor call?
    // (the call to the super or self constructor in the first line of a constructor)
    // in this context the object's fields should not be in scope

    var diagnostic: List[String] = Nil      // these messages are printed when issuing an error
    var implicitsEnabled = false
    var macrosEnabled = true
    var enrichmentEnabled = false // to selectively allow enrichment in patterns, where other kinds of implicit conversions are not allowed
    var checking = false
    var retyping = false

    var savedTypeBounds: List[(Symbol, Type)] = List() // saved type bounds
       // for type parameters which are narrowed in a GADT

    var typingIndentLevel: Int = 0
    def typingIndent = "  " * typingIndentLevel

    var buffer: mutable.Set[AbsTypeError] = _

    def enclClassOrMethod: Context =
      if ((owner eq NoSymbol) || (owner.isClass) || (owner.isMethod)) this
      else outer.enclClassOrMethod

    def undetparamsString =
      if (undetparams.isEmpty) ""
      else undetparams.mkString("undetparams=", ", ", "")
    def undetparams = _undetparams
    def undetparams_=(ps: List[Symbol]) = { _undetparams = ps }

    def extractUndetparams() = {
      val tparams = undetparams
      undetparams = List()
      tparams
    }

    private[this] var mode = 0

    def errBuffer = buffer
    def hasErrors = buffer.nonEmpty

    def state: Int = mode
    def restoreState(state0: Int) = mode = state0

    def reportErrors    = (state & ReportErrors)     != 0
    def bufferErrors    = (state & BufferErrors)     != 0
    def ambiguousErrors = (state & AmbiguousErrors)  != 0
    def throwErrors     = (state & notThrowMask)     == 0

    def setReportErrors()    = mode = (ReportErrors | AmbiguousErrors)
    def setBufferErrors()    = {
      //assert(bufferErrors || !hasErrors, "When entering the buffer state, context has to be clean. Current buffer: " + buffer)
      mode = BufferErrors
    }
    def setThrowErrors()     = mode &= (~AllMask)
    def setAmbiguousErrors(report: Boolean) = if (report) mode |= AmbiguousErrors else mode &= notThrowMask

    def updateBuffer(errors: mutable.Set[AbsTypeError]) = buffer ++= errors
    def condBufferFlush(removeP: AbsTypeError => Boolean) {
      val elems = buffer.filter(removeP)
      buffer --= elems
    }
    def flushBuffer() { buffer.clear() }
    def flushAndReturnBuffer(): mutable.Set[AbsTypeError] = {
      val current = buffer.clone()
      buffer.clear()
      current
    }

    def withImplicitsEnabled[T](op: => T): T = {
      val saved = implicitsEnabled
      implicitsEnabled = true
      try op
      finally implicitsEnabled = saved
    }

    def withImplicitsDisabled[T](op: => T): T = {
      val saved = implicitsEnabled
      implicitsEnabled = false
      val savedP = enrichmentEnabled
      enrichmentEnabled = false
      try op
      finally {
        implicitsEnabled = saved
        enrichmentEnabled = savedP
      }
    }

    def withImplicitsDisabledAllowEnrichment[T](op: => T): T = {
      val saved = implicitsEnabled
      implicitsEnabled = false
      val savedP = enrichmentEnabled
      enrichmentEnabled = true
      try op
      finally {
        implicitsEnabled = saved
        enrichmentEnabled = savedP
      }
    }

    def withMacrosEnabled[T](op: => T): T = {
      val saved = macrosEnabled
      macrosEnabled = true
      try op
      finally macrosEnabled = saved
    }

    def withMacrosDisabled[T](op: => T): T = {
      val saved = macrosEnabled
      macrosEnabled = false
      try op
      finally macrosEnabled = saved
    }

    def make(unit: CompilationUnit, tree: Tree, owner: Symbol,
             scope: Scope, imports: List[ImportInfo]): Context = {
      val c   = new Context
      c.unit  = unit
      c.tree  = tree
      c.owner = owner
      c.scope = scope
      c.outer = this

      tree match {
        case Template(_, _, _) | PackageDef(_, _) =>
          c.enclClass = c
          c.prefix = c.owner.thisType
          c.inConstructorSuffix = false
        case _ =>
          c.enclClass = this.enclClass
          c.prefix =
            if (c.owner != this.owner && c.owner.isTerm) NoPrefix
            else this.prefix
          c.inConstructorSuffix = this.inConstructorSuffix
      }
      tree match {
        case DefDef(_, _, _, _, _, _) =>
          c.enclMethod = c
        case _ =>
          c.enclMethod = this.enclMethod
      }
      c.variance = this.variance
      c.depth = if (scope == this.scope) this.depth else this.depth + 1
      c.imports = imports
      c.inSelfSuperCall = inSelfSuperCall
      c.restoreState(this.state)
      c.diagnostic = this.diagnostic
      c.typingIndentLevel = typingIndentLevel
      c.implicitsEnabled = this.implicitsEnabled
      c.macrosEnabled = this.macrosEnabled
      c.enrichmentEnabled = this.enrichmentEnabled
      c.checking = this.checking
      c.retyping = this.retyping
      c.openImplicits = this.openImplicits
      c.buffer = if (this.buffer == null) mutable.LinkedHashSet[AbsTypeError]() else this.buffer // need to initialize
      registerContext(c.asInstanceOf[analyzer.Context])
      debuglog("[context] ++ " + c.unit + " / " + tree.summaryString)
      c
    }

    def makeNewImport(sym: Symbol): Context =
      makeNewImport(gen.mkWildcardImport(sym))

    def makeNewImport(imp: Import): Context = {
      val impInfo = new ImportInfo(imp, depth)
      if (settings.lint.value && imp.pos.isDefined) // pos.isDefined excludes java.lang/scala/Predef imports
        allImportInfos(unit) ::= impInfo

      make(unit, imp, owner, scope, impInfo :: imports)
    }

    def make(tree: Tree, owner: Symbol, scope: Scope): Context =
      if (tree == this.tree && owner == this.owner && scope == this.scope) this
      else make0(tree, owner, scope)

    private def make0(tree: Tree, owner: Symbol, scope: Scope): Context =
      make(unit, tree, owner, scope, imports)

    def makeNewScope(tree: Tree, owner: Symbol): Context =
      make(tree, owner, newNestedScope(scope))
    // IDE stuff: distinguish between scopes created for typing and scopes created for naming.

    def make(tree: Tree, owner: Symbol): Context =
      make0(tree, owner, scope)

    def make(tree: Tree): Context =
      make(tree, owner)

    def makeSilent(reportAmbiguousErrors: Boolean, newtree: Tree = tree): Context = {
      val c = make(newtree)
      c.setBufferErrors()
      c.setAmbiguousErrors(reportAmbiguousErrors)
      c.buffer = mutable.LinkedHashSet[AbsTypeError]()
      c
    }

    def makeImplicit(reportAmbiguousErrors: Boolean) = {
      val c = makeSilent(reportAmbiguousErrors)
      c.implicitsEnabled = false
      c.enrichmentEnabled = false
      c
    }

    def makeConstructorContext = {
      var baseContext = enclClass.outer
      while (baseContext.tree.isInstanceOf[Template])
        baseContext = baseContext.outer
      val argContext = baseContext.makeNewScope(tree, owner)
      argContext.inSelfSuperCall = true
      argContext.restoreState(this.state)
      def enterElems(c: Context) {
        def enterLocalElems(e: ScopeEntry) {
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
      enterElems(this)
      argContext
    }

    private def addDiagString(msg: String) = {
      val ds =
        if (diagnostic.isEmpty) ""
        else diagnostic.mkString("\n","\n", "")
      if (msg endsWith ds) msg else msg + ds
    }

    private def unitError(pos: Position, msg: String) =
      unit.error(pos, if (checking) "\n**** ERROR DURING INTERNAL CHECKING ****\n" + msg else msg)

    @inline private def issueCommon(err: AbsTypeError)(pf: PartialFunction[AbsTypeError, Unit]) {
      if (settings.Yissuedebug.value) {
        log("issue error: " + err.errMsg)
        (new Exception).printStackTrace()
      }
      if (pf isDefinedAt err) pf(err)
      else if (bufferErrors) { buffer += err }
      else throw new TypeError(err.errPos, err.errMsg)
    }

    def issue(err: AbsTypeError) {
      issueCommon(err) { case _ if reportErrors =>
        unitError(err.errPos, addDiagString(err.errMsg))
      }
    }

    def issueAmbiguousError(pre: Type, sym1: Symbol, sym2: Symbol, err: AbsTypeError) {
      issueCommon(err) { case _ if ambiguousErrors =>
        if (!pre.isErroneous && !sym1.isErroneous && !sym2.isErroneous)
          unitError(err.errPos, err.errMsg)
      }
    }

    def issueAmbiguousError(err: AbsTypeError) {
      issueCommon(err) { case _ if ambiguousErrors => unitError(err.errPos, addDiagString(err.errMsg)) }
    }

    // TODO remove
    def error(pos: Position, err: Throwable) =
      if (reportErrors) unitError(pos, addDiagString(err.getMessage()))
      else throw err

    def error(pos: Position, msg: String) = {
      val msg1 = addDiagString(msg)
      if (reportErrors) unitError(pos, msg1)
      else throw new TypeError(pos, msg1)
    }

    def warning(pos: Position, msg: String): Unit = warning(pos, msg, false)
    def warning(pos: Position, msg: String, force: Boolean) {
      if (reportErrors || force) unit.warning(pos, msg)
    }

    def isLocal(): Boolean = tree match {
      case Block(_,_)       => true
      case PackageDef(_, _) => false
      case EmptyTree        => false
      case _                => outer.isLocal()
    }

    def isNameInScope(name: Name) = lookupSymbol(name, _ => true).isSuccess

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

    override def toString = "Context(%s@%s unit=%s scope=%s errors=%b, reportErrors=%b, throwErrors=%b)".format(
      owner.fullName, tree.shortClass, unit, scope.##, hasErrors, reportErrors, throwErrors
    )
    /** Is `sub` a subclass of `base` or a companion object of such a subclass?
     */
    def isSubClassOrCompanion(sub: Symbol, base: Symbol) =
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

    /** Is `sym` accessible as a member of `pre` in current context?
     */
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

      /** Are we inside definition of `ab`? */
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

      /** Is protected access to target symbol permitted */
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
             || isProtectedAccessOK(sym)
             || (sym.allOverriddenSymbols exists isProtectedAccessOK)
                // that last condition makes protected access via self types work.
             )
        )
        // note: phase.erasedTypes disables last test, because after addinterfaces
        // implementation classes are not in the superclass chain. If we enable the
        // test, bug780 fails.
      }
    }

    def pushTypeBounds(sym: Symbol) {
      savedTypeBounds ::= ((sym, sym.info))
    }

    def restoreTypeBounds(tp: Type): Type = {
      var current = tp
      for ((sym, info) <- savedTypeBounds) {
        debuglog("resetting " + sym + " to " + info);
        sym.info match {
          case TypeBounds(lo, hi) if (hi <:< lo && lo <:< hi) =>
            current = current.instantiateTypeParams(List(sym), List(lo))
//@M TODO: when higher-kinded types are inferred, probably need a case PolyType(_, TypeBounds(...)) if ... =>
          case _ =>
        }
        sym.setInfo(info)
      }
      savedTypeBounds = List()
      current
    }

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
      val pre = imp.qual.tpe
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
          } else if (imports != nextOuter.imports) {
            assert(imports.tail == nextOuter.imports, (imports, nextOuter.imports))
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

      while (!impSym.exists && imports.nonEmpty && imp1.depth > symbolDepth) {
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
          def imp1wins = { imports = imp1 :: imports.tail.tail }
          def imp2wins = { impSym = other ; imports = imports.tail }

          if (!other.exists) // imp1 wins; drop imp2 and continue.
            imp1wins
          else if (sameDepth && !imp1Explicit && imp2Explicit) // imp2 wins; drop imp1 and continue.
            imp2wins
          else resolveAmbiguousImport(name, imp1, imp2) match {
            case Some(imp) => if (imp eq imp1) imp1wins else imp2wins
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
      if (settings.lint.value && selectors.nonEmpty && result != NoSymbol && pos != NoPosition)
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
