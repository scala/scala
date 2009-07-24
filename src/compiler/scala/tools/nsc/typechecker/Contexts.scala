/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.tools.nsc.util.{Position,NoPosition}
import scala.collection.mutable.ListBuffer

/** This trait ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Contexts { self: Analyzer =>
  import global._

  val NoContext = new Context {
    override def implicitss: List[List[ImplicitInfo]] = List()
    outer = this
  }
  NoContext.enclClass = NoContext
  NoContext.enclMethod = NoContext

  private val startContext = {
    NoContext.make(
    global.Template(List(), emptyValDef, List()) setSymbol global.NoSymbol setType global.NoType,
    global.definitions.RootClass,
    global.definitions.RootClass.info.decls)
  }

  /** List of objects and packages to import from in
   *  a root context.  This list is sensitive to the
   *  compiler settings.
   */
  protected def rootImports(unit: CompilationUnit, tree: Tree): List[Symbol] = {
    import definitions._
    val imps = new ListBuffer[Symbol]
    if (!settings.noimports.value) {
      assert(isDefinitionsInitialized)
      imps += JavaLangPackage
      if (!unit.isJava) {
        assert(ScalaPackage ne null, "Scala package is null")
        imps += ScalaPackage
        if (!(treeInfo.isPredefUnit(unit.body) || treeInfo.containsLeadingPredefImport(List(unit.body))))
          imps += PredefModule
      }
    }
    imps.toList
  }

  def rootContext(unit: CompilationUnit): Context =
    rootContext(unit, EmptyTree, false)

  def rootContext(unit: CompilationUnit, tree: Tree, erasedTypes: Boolean): Context = {
    import definitions._
    var sc = startContext
    def addImport(pkg: Symbol) {
      assert(pkg ne null)
      val qual = gen.mkAttributedStableRef(pkg)
      sc = sc.makeNewImport(
        Import(qual, List((nme.WILDCARD, null)))
        .setSymbol(NoSymbol.newImport(NoPosition).setFlag(SYNTHETIC).setInfo(ImportType(qual)))
        .setType(NoType))
      sc.depth += 1
    }
    for (imp <- rootImports(unit, tree))
      addImport(imp)
    val c = sc.make(unit, tree, sc.owner, sc.scope, sc.imports)
    c.reportAmbiguousErrors = !erasedTypes
    c.reportGeneralErrors = !erasedTypes
    c.implicitsEnabled = !erasedTypes
    c
  }

  def resetContexts() {
    var sc = startContext
    while (sc != NoContext) {
      sc.tree match {
        case Import(qual, _) => qual.tpe = singleType(qual.symbol.owner.thisType, qual.symbol)
        case _ =>
      }
      sc = sc.outer
    }
  }
  class Context private[typechecker] {
    var unit: CompilationUnit = _
    var tree: Tree = _ // Tree associated with this context
    var owner: Symbol = NoSymbol// The current owner
    var scope: Scope = _                    // The current scope
    var outer: Context = _                  // The next outer context
    var enclClass: Context = _              // The next outer context whose tree is a
                                            // template or package definition
    var enclMethod: Context = _             // The next outer context whose tree is a method
    var variance: Int = _                   // Variance relative to enclosing class
    private var _undetparams: List[Symbol] = List() // Undetermined type parameters,
                                                    // not inherited to child contexts
    var depth: Int = 0
    var imports: List[ImportInfo] = List()   // currently visible imports
    var openImplicits: List[Type] = List()   // types for which implicit arguments
                                             // are currently searched
    // for a named application block (Tree) the corresponding NamedApplyInfo
    var namedApplyBlockInfo: Option[(Tree, NamedApplyInfo)] = None
    var prefix: Type = NoPrefix
    var inConstructorSuffix = false         // are we in a secondary constructor
                                            // after the this constructor call?
    var returnsSeen = false                 // for method context: were returns encountered?
    var reportAmbiguousErrors = false
    var reportGeneralErrors = false
    var diagnostic: List[String] = Nil      // these messages are printed when issuing an error
    var implicitsEnabled = false
    var checking = false
    var retyping = false

    var savedTypeBounds: List[(Symbol, Type)] = List() // saved type bounds
       // for type parameters which are narrowed in a GADT

    def intern0 : Context = {
      if (this eq NoContext) return this
      val txt = new Context
      txt.unit = unit
      txt.tree = tree
      txt.owner = owner
      txt.scope = scope
      assert(outer ne this) // stupid
      txt.outer = outer // already interned
      def fix(what : Context) =
        if (what eq this) txt
        else what
      txt.enclClass = fix(enclClass)
      txt.enclMethod = fix(enclMethod)
      txt.implicitsEnabled = implicitsEnabled
      txt.variance = variance
      txt._undetparams = _undetparams
      txt.depth = depth
      txt.imports = imports
      txt.openImplicits = openImplicits
      txt.prefix = prefix
      txt.inConstructorSuffix = inConstructorSuffix
      txt.returnsSeen = returnsSeen
      txt.reportGeneralErrors = reportGeneralErrors
      txt.checking = checking
      txt.retyping = retyping
      txt.savedTypeBounds = savedTypeBounds
      txt
    }
    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that                         => super.equals(that)
    }

    def undetparams = _undetparams
    def undetparams_=(ps: List[Symbol]) = {
      //System.out.println("undetparams = " + ps);//debug
      _undetparams = ps
    }

    def extractUndetparams() = {
      val tparams = undetparams
      undetparams = List()
      tparams
    }

    /**
     *  @param unit    ...
     *  @param tree    ...
     *  @param owner   ...
     *  @param scope   ...
     *  @param imports ...
     *  @return        ...
     */
    def make(unit: CompilationUnit, tree: Tree, owner: Symbol,
             scope: Scope, imports: List[ImportInfo]): Context = {
      val c = new Context
      c.unit = unit
      c.tree = sanitize(tree)
      c.owner = owner
      c.scope = scope

      c.outer = intern(this)
      def internIf(txt : Context) = {
        if (txt eq this) c.outer // already interned!
        else txt
      }

      tree match {
        case Template(_, _, _) | PackageDef(_, _) =>
          c.enclClass = c
          c.prefix = c.owner.thisType
          c.inConstructorSuffix = false
        case _ =>
          c.enclClass = internIf(this.enclClass)
          c.prefix =
            if (c.owner != this.owner && c.owner.isTerm) NoPrefix
            else this.prefix
          c.inConstructorSuffix = this.inConstructorSuffix
      }
      tree match {
        case DefDef(_, _, _, _, _, _) =>
          c.enclMethod = c
        case _ =>
          c.enclMethod = internIf(this.enclMethod)
      }
      c.variance = this.variance
      c.depth = if (scope == this.scope) this.depth else this.depth + 1
      c.imports = imports
      c.reportAmbiguousErrors = this.reportAmbiguousErrors
      c.reportGeneralErrors = this.reportGeneralErrors
      c.diagnostic = this.diagnostic
      c.implicitsEnabled = this.implicitsEnabled
      c.checking = this.checking
      c.retyping = this.retyping
      c.openImplicits = this.openImplicits
      registerContext(c.asInstanceOf[analyzer.Context])
      c
    }

    def make(unit: CompilationUnit): Context = {
      val c = make(unit, EmptyTree, owner, scope, imports)
      c.reportAmbiguousErrors = true
      c.reportGeneralErrors = true
      c.implicitsEnabled = true
      c
    }

    def makeNewImport(imp: Import): Context =
      make(unit, imp, owner, scope, new ImportInfo(imp, depth) :: imports)



    def make(tree: Tree, owner: Symbol, scope: Scope): Context = {
      if (tree == this.tree && owner == this.owner && scope == this.scope) this
      else make0(tree, owner, scope)
    }
    private def make0(tree : Tree, owner : Symbol, scope : Scope) : Context = {
      make(unit, tree, owner, scope, imports)
    }

    def makeNewScope(tree: Tree, owner: Symbol)(implicit kind : ScopeKind): Context =
      make(tree, owner, scopeFor(scope, tree, kind))
    // IDE stuff: distinguish between scopes created for typing and scopes created for naming.

    def make(tree: Tree, owner: Symbol): Context =
      make0(tree, owner, scope)

    def make(tree: Tree): Context =
      make(tree, owner)

    def makeSilent(reportAmbiguousErrors: Boolean): Context = {
      val c = make(tree)
      c.reportGeneralErrors = false
      c.reportAmbiguousErrors = reportAmbiguousErrors
      c
    }

    def makeImplicit(reportAmbiguousErrors: Boolean) = {
      val c = makeSilent(reportAmbiguousErrors)
      c.implicitsEnabled = false
      c
    }

    def makeConstructorContext = {
      var baseContext = enclClass.outer
      //todo: find out why we need next line
      while (baseContext.tree.isInstanceOf[Template])
        baseContext = baseContext.outer
      val argContext = baseContext.makeNewScope(tree, owner)(Constructor0ScopeKind)
      argContext.reportGeneralErrors = this.reportGeneralErrors
      argContext.reportAmbiguousErrors = this.reportAmbiguousErrors
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

    //todo: remove
    def makeConstructorSuffixContext = {
      val c = make(tree)
      c.inConstructorSuffix = true
      c
    }

    private def diagString =
      if (diagnostic.isEmpty) ""
      else diagnostic.mkString("\n","\n", "")

    def error(pos: Position, err: Error) {
      val msg = err.getMessage() + diagString
      if (reportGeneralErrors)
        unit.error(pos, if (checking) "**** ERROR DURING INTERNAL CHECKING ****\n" + msg else msg)
      else
        throw err
    }

    def error(pos: Position, msg: String) {
      val msg1 = msg + diagString
      if (reportGeneralErrors)
        unit.error(pos, if (checking) "**** ERROR DURING INTERNAL CHECKING ****\n" + msg1 else msg1)
      else
        throw new TypeError(pos, msg1)
    }

    def warning(pos:  Position, msg: String) {
      if (reportGeneralErrors) unit.warning(pos, msg)
    }

    /**
     *  @param pos  ...
     *  @param pre  ...
     *  @param sym1 ...
     *  @param sym2 ...
     *  @param rest ...
     */
    def ambiguousError(pos: Position, pre: Type, sym1: Symbol,
                       sym2: Symbol, rest: String) {
      val msg =
        ("ambiguous reference to overloaded definition,\n" +
         "both " + sym1 + sym1.locationString + " of type " + pre.memberType(sym1) +
         "\nand  " + sym2 + sym2.locationString + " of type " + pre.memberType(sym2) +
         "\nmatch " + rest)
      if (reportAmbiguousErrors) {
        if (!pre.isErroneous && !sym1.isErroneous && !sym2.isErroneous)
          unit.error(pos, msg)
      } else throw new TypeError(pos, msg)
    }

    def outerContext(clazz: Symbol): Context = {
      var c = this
      while (c != NoContext && c.owner != clazz) c = c.outer.enclClass
      c
    }

    def isLocal(): Boolean = tree match {
      case Block(_,_) => true
      case PackageDef(_, _) => false
      case EmptyTree => false
      case _ => outer.isLocal()
    }

    def nextEnclosing(p: Context => Boolean): Context =
      if (this == NoContext || p(this)) this else outer.nextEnclosing(p)

    override def toString(): String = {
      if (this == NoContext) "NoContext"
      else owner.toString() + " @ " + tree.getClass() +
           " " + tree.toString() + ", scope = " + scope.hashCode() +
           " " + scope.toList + "\n:: " + outer.toString()
    }

    /** Return closest enclosing context that defines a superclass of `clazz', or a
     *  companion module of a superclass of `clazz', or NoContext if none exists */
    def enclosingSuperClassContext(clazz: Symbol): Context = {
      var c = this.enclClass
      while (c != NoContext &&
             !clazz.isNonBottomSubClass(c.owner) &&
             !(c.owner.isModuleClass && clazz.isNonBottomSubClass(c.owner.linkedClassOfModule)))
        c = c.outer.enclClass
      c
    }

    /** Return closest enclosing context that defines a subclass of `clazz', or NoContext
     *  if none exists */
    def enclosingSubClassContext(clazz: Symbol): Context = {
      var c = this.enclClass
      while (c != NoContext && !c.owner.isNonBottomSubClass(clazz))
        c = c.outer.enclClass
      c
    }

    /** Is <code>sym</code> accessible as a member of tree `site' with type
     *  <code>pre</code> in current context?
     *
     *  @param sym         ...
     *  @param pre         ...
     *  @param superAccess ...
     *  @return            ...
     */
    def isAccessible(sym: Symbol, pre: Type, superAccess: Boolean): Boolean = {

      /** Are we inside definition of `sym'? */
      def accessWithin(sym: Symbol): Boolean = this.owner.ownerChain contains sym
/*
        var c = this
        while (c != NoContext && c.owner != owner) {
          if (c.outer eq null) assert(false, "accessWithin(" + owner + ") " + c);//debug
          if (c.outer.enclClass eq null) assert(false, "accessWithin(" + owner + ") " + c);//debug
          c = c.outer.enclClass
        }
        c != NoContext
      }
*/
      /** Is `clazz' a subclass of an enclosing class? */
      def isSubClassOfEnclosing(clazz: Symbol): Boolean =
        enclosingSuperClassContext(clazz) != NoContext

      def isSubThisType(pre: Type, clazz: Symbol): Boolean = pre match {
        case ThisType(pclazz) => pclazz isNonBottomSubClass clazz
        case _ => false
      }

      (pre == NoPrefix) || {
        val ab = sym.accessBoundary(sym.owner)
        ((ab.isTerm || ab == definitions.RootClass)
         ||
         (accessWithin(ab) || accessWithin(ab.linkedClassOfClass)) &&
         (!sym.hasFlag(LOCAL) ||
          (sym hasFlag PROTECTED) && isSubThisType(pre, sym.owner) ||
          pre =:= sym.owner.thisType)
         ||
         (sym hasFlag PROTECTED) &&
         (superAccess ||
          (pre.widen.typeSymbol.isNonBottomSubClass(sym.owner) &&
           (isSubClassOfEnclosing(pre.widen.typeSymbol) || phase.erasedTypes))))
        // note: phase.erasedTypes disables last test, because after addinterfaces
        // implementation classes are not in the superclass chain. If we enable the
        // test, bug780 fails.
      }
    }

    def pushTypeBounds(sym: Symbol) {
      savedTypeBounds = (sym, sym.info) :: savedTypeBounds
    }

    def restoreTypeBounds(tp: Type): Type = {
      var current = tp
      for ((sym, info) <- savedTypeBounds) {
        if (settings.debug.value) log("resetting " + sym + " to " + info);
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

    def resetCache : Unit = {
      implicitsRunId = NoRunId
      implicitsCache = null
      if (outer != null && outer != this) outer.resetCache
    }
    private def collectImplicits(syms: List[Symbol], pre: Type): List[ImplicitInfo] =
      for (sym <- syms if sym.hasFlag(IMPLICIT) && isAccessible(sym, pre, false))
      yield new ImplicitInfo(sym.name, pre, sym)

    private def collectImplicitImports(imp: ImportInfo): List[ImplicitInfo] = {
      val pre = imp.qual.tpe
      def collect(sels: List[(Name, Name)]): List[ImplicitInfo] = sels match {
        case List() => List()
        case List((nme.WILDCARD, _)) => collectImplicits(pre.implicitMembers, pre)
        case (from, to) :: sels1 =>
          var impls = collect(sels1) filter (info => info.name != from)
          if (to != nme.WILDCARD) {
            val sym = imp.importedSymbol(to)
            if (sym.hasFlag(IMPLICIT)) impls = new ImplicitInfo(to, pre, sym) :: impls
          }
          impls
      }
      if (settings.debug.value)
        log("collect implicit imports " + imp + "=" + collect(imp.tree.selectors))//debug
      collect(imp.tree.selectors)
    }

    def implicitss: List[List[ImplicitInfo]] = {
      val nextOuter =
        if (owner.isConstructor) {
          if (outer.tree.isInstanceOf[Template]) outer.outer.outer
          else outer.outer
        } else outer
        // can we can do something smarter to bring back the implicit cache?
      if (implicitsRunId != currentRunId) {
        implicitsRunId = currentRunId
        implicitsCache = List()
        val newImplicits: List[ImplicitInfo] =
          if (owner != nextOuter.owner && owner.isClass && !owner.isPackageClass) {
            if (!owner.isInitialized) return nextOuter.implicitss
            if (settings.debug.value)
              log("collect member implicits " + owner + ", implicit members = " +
                  owner.thisType.implicitMembers)//debug
            val savedEnclClass = enclClass
            this.enclClass = this
            val res = collectImplicits(owner.thisType.implicitMembers, owner.thisType)
            this.enclClass = savedEnclClass
            res
          } else if (scope != nextOuter.scope && !owner.isPackageClass) {
            if (settings.debug.value)
              log("collect local implicits " + scope.toList)//debug
            collectImplicits(scope.toList, NoPrefix)
          } else if (imports != nextOuter.imports) {
            assert(imports.tail == nextOuter.imports)
            collectImplicitImports(imports.head)
          } else List()
        implicitsCache = if (newImplicits.isEmpty) nextOuter.implicitss
                         else newImplicits :: nextOuter.implicitss
      }
      implicitsCache
    }
    override def hashCode = {
      var hc = 0
      implicit def b2i(b : Boolean) = if (b) 1 else 0
      // assum enclClass/enclMethod/outer are all interned already.
      hc += tree.hashCodeStructure
      def f(txt : Context) = if (txt eq this) 0 else System.identityHashCode(txt)
      hc += f(enclClass)
      hc += f(enclMethod)
      hc += f(outer)
      hc += owner.hashCode
      hc += scope.hashCode
      hc += variance.hashCode
      hc += _undetparams.hashCode
      hc += depth
      hc += imports.hashCode
      hc += prefix.hashCode
      hc += inConstructorSuffix
      hc += checking
      hc += retyping
      hc += savedTypeBounds.hashCode
      hc += (if (unit eq null) 0 else unit.hashCode)
      hc
    }

  }
  class ImportInfo(val tree: Import, val depth: Int) {
    /** The prefix expression */
    def qual: Tree = tree.symbol.info match {
      case ImportType(expr) => expr
      case ErrorType => tree
      case _ => throw new FatalError("symbol " + tree.symbol + " has bad type: " + tree.symbol.info);//debug
    }

    /** Is name imported explicitly, not via wildcard? */
    def isExplicitImport(name: Name): Boolean =
      tree.selectors exists (_._2 == name.toTermName)

    /** The symbol with name <code>name</code> imported from import clause
     *  <code>tree</code>.
     */
    def importedSymbol(name: Name): Symbol = {
      var result: Symbol = NoSymbol
      var renamed = false
      var selectors = tree.selectors
      while (selectors != Nil && result == NoSymbol) {
        if (selectors.head._1 != nme.WILDCARD)
          notifyImport(name, qual.tpe, selectors.head._1, selectors.head._2)

        if (selectors.head._2 == name.toTermName)
          result = qual.tpe.member(
            if (name.isTypeName) selectors.head._1.toTypeName else selectors.head._1)
        else if (selectors.head._1 == name.toTermName)
          renamed = true
        else if (selectors.head._1 == nme.WILDCARD && !renamed)
          result = qual.tpe.member(name)
        selectors = selectors.tail
      }
      result
    }

    override def toString() = tree.toString()

    override def hashCode = tree.hashCodeStructure + depth
    override def equals(that : Any) = that match {
      case that : ImportInfo =>
        depth == that.depth && (tree equalsStructure that.tree)
      case _ => false
    }
  }

  case class ImportType(expr: Tree) extends Type {
    override def equals(that : Any) = that match {
      case ImportType(expr) => this.expr == expr
      case _                => false
    }
    override def hashCode = expr.hashCode
    override def safeToString = "ImportType("+expr+")"
  }
  protected def intern(txt : Context) = txt

}
