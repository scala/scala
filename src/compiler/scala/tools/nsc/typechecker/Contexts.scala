/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import symtab.Flags._
import scala.tools.nsc.util.Position

/** This trait ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Contexts requires Analyzer {
  import global._

  val NoContext = new Context {
    override def implicitss: List[List[ImplicitInfo]] = List()
    outer = this;
  }
  NoContext.enclClass = NoContext
  NoContext.enclMethod = NoContext

  private val startContext = {
    NoContext.make(
    global.Template(List(), List()) setSymbol global.NoSymbol setType global.NoType,
    global.definitions.RootClass,
    global.definitions.RootClass.info.decls)
  }

  def rootContext(unit: CompilationUnit): Context =
    rootContext(unit, EmptyTree, false)

  def rootContext(unit: CompilationUnit, tree: Tree, erasedTypes: boolean): Context = {
    import definitions._
    var sc = startContext
    def addImport(pkg: Symbol): unit = {
      assert(pkg ne null)
      val qual = gen.mkAttributedStableRef(pkg)
      sc = sc.makeNewImport(
        Import(qual, List((nme.WILDCARD, null)))
        .setSymbol(NoSymbol.newImport(NoPos).setFlag(SYNTHETIC).setInfo(ImportType(qual)))
        .setType(NoType))
      sc.depth = sc.depth + 1
    }
    if (!settings.noimports.value) {
      assert(isDefinitionsInitialized)
      addImport(JavaLangPackage)
      assert(ScalaPackage ne null, "Scala package is null")
      addImport(ScalaPackage)
      if (!(settings.nopredefs.value || treeInfo.containsLeadingPredefImport(List(unit.body))))
        addImport(PredefModule)
    }
    val c = sc.make(unit, tree, sc.owner, sc.scope, sc.imports)
    c.reportAmbiguousErrors = !erasedTypes
    c.reportGeneralErrors = !erasedTypes
    c.implicitsEnabled = !erasedTypes
    c
  }

  def resetContexts: unit = {
    var sc = startContext
    while (sc != NoContext) {
      sc.tree match {
        case Import(qual, _) => qual.tpe = singleType(qual.symbol.owner.thisType, qual.symbol)
        case _ =>
      }
      sc = sc.outer
    }
  }
  class Context {
    var unit: CompilationUnit = _
    var tree: Tree = _                      // Tree associated with this context
    var owner: Symbol = NoSymbol// The current owner
    var scope: Scope = _                    // The current scope
    var outer: Context = _                  // The next outer context
    var enclClass: Context = _              // The next outer context whose tree is a
                                            // template or package definition
    var enclMethod: Context = _             // The next outer context whose tree is a method
    var variance: int = _                   // Variance relative to enclosing class
    private var _undetparams: List[Symbol] = List() // Undetermined type parameters,
                                                    // not inherited to child contexts
    var depth: int = 0
    var imports: List[ImportInfo] = List()

    var prefix: Type = NoPrefix
    var inConstructorSuffix = false         // are we in a secondary constructor
                                            // after the this constructor call?
    var returnsSeen = false                 // for method context: were returns encountered?
    var reportAmbiguousErrors = false
    var reportGeneralErrors = false
    var implicitsEnabled = false
    var checking = false
    var retyping = false

    var savedTypeBounds: List[(Symbol, Type)] = List()

    override def equals(that : Any) = that match {
      case that if (super.equals(that)) => true
      case NoContext => false
      case that : Context =>
        val a0 = if (tree eq null) tree == that.tree else tree equalsStructure that.tree;
        val a1 = owner == that.owner;
        val a2 = scope == that.scope;
        val a3 = outer == that.outer;
        val a4 = {
          if (enclClass eq this) {
            that.enclClass eq that;
          } else enclClass == that.enclClass;
        }
        val a5 = {
          if (enclMethod eq this)
            that.enclMethod eq that;
          else enclMethod == that.enclMethod;
        }
        val a6 = variance == that.variance;
        val a7 = _undetparams == that._undetparams;
        val a8 = depth == that.depth;
        val a9 = if (imports.length != that.imports.length) false else
          (for (val x <- imports.zip(that.imports)) yield
              (x._1.tree equalsStructure x._2.tree) && x._1.depth == x._2.depth).
            foldLeft(true)((x,y) => x && y);

        val a10 = prefix == that.prefix;
        val a11 = inConstructorSuffix == that.inConstructorSuffix;
        val a12 = implicitsEnabled == that.implicitsEnabled;
        val a13 = checking == that.checking;
        val a14 = retyping == that.retyping;
        val a15 = savedTypeBounds == that.savedTypeBounds;
        a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7 && a8 && a9 && a10 && a11 && a12 && a13 && a14 && a15
      case _ => false;
    }


    def undetparams = _undetparams
    def undetparams_=(ps: List[Symbol]) = {
      //System.out.println("undetparams = " + ps);//debug
      _undetparams = ps
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
      c.tree = tree
      c.owner = owner
      c.scope = scope
      tree match {
        case Template(_, _) | PackageDef(_, _) =>
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
      c.reportAmbiguousErrors = this.reportAmbiguousErrors
      c.reportGeneralErrors = this.reportGeneralErrors
      c.implicitsEnabled = this.implicitsEnabled
      c.checking = this.checking
      c.retyping = this.retyping
      c.outer = this
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

    def make(tree: Tree, owner: Symbol, scope: Scope): Context =
      make(unit, tree, owner, scope, imports)

    def makeNewScope(tree: Tree, owner: Symbol): Context =
      make(tree, owner, newScope(scope))

    def make(tree: Tree, owner: Symbol): Context =
      make(tree, owner, scope)

    def make(tree: Tree): Context =
      make(tree, owner)

    def makeSilent(reportAmbiguousErrors: boolean): Context = {
      val c = make(tree)
      c.reportGeneralErrors = false
      c.reportAmbiguousErrors = reportAmbiguousErrors
      c
    }

    def makeImplicit(reportAmbiguousErrors: boolean) = {
      val c = makeSilent(reportAmbiguousErrors)
      c.implicitsEnabled = false
      c
    }

    def makeConstructorContext = {
      var baseContext = enclClass.outer
      //todo: find out why we need next line
      while (baseContext.tree.isInstanceOf[Template])
        baseContext = baseContext.outer
      val argContext = baseContext.makeNewScope(tree, owner)
      for (val sym <- scope.toList) argContext.scope enter sym
      argContext
    }

    def makeConstructorSuffixContext = {
      val c = make(tree)
      c.inConstructorSuffix = true
      c
    }

    def error(pos: Int, err: Error) {
      val msg = err.getMessage()
      if (reportGeneralErrors)
        unit.error(pos, if (checking) "**** ERROR DURING INTERNAL CHECKING ****\n" + msg else msg)
      else
        throw err
    }

    def error(pos: PositionType, msg: String) {
      if (reportGeneralErrors)
        unit.error(pos, if (checking) "**** ERROR DURING INTERNAL CHECKING ****\n" + msg else msg)
      else
        throw new TypeError(pos, msg)
    }

    def warning(pos:  PositionType, msg: String) {
      if (reportGeneralErrors) unit.warning(pos, msg)
    }

    /**
     *  @param pos  ...
     *  @param pre  ...
     *  @param sym1 ...
     *  @param sym2 ...
     *  @param rest ...
     */
    def ambiguousError(pos: PositionType, pre: Type, sym1: Symbol,
                       sym2: Symbol, rest: String): unit = {
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

    def isLocal(): boolean = tree match {
      case Block(_,_) => true
      case PackageDef(_, _) => false
      case EmptyTree => false
      case _ => outer.isLocal()
    }

    def nextEnclosing(p: Context => boolean): Context =
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
    def isAccessible(sym: Symbol, pre: Type, superAccess: boolean): boolean = {

      /** Are we inside definition of `owner'? */
      def accessWithin(owner: Symbol): boolean = {
        var c = this
        while (c != NoContext && c.owner != owner) {
          if (c.outer eq null) assert(false, "accessWithin(" + owner + ") " + c);//debug
          if (c.outer.enclClass eq null) assert(false, "accessWithin(" + owner + ") " + c);//debug
          c = c.outer.enclClass
        }
        c != NoContext
      }

      /** Is `clazz' a subclass of an enclosing class? */
      def isSubClassOfEnclosing(clazz: Symbol): boolean =
        enclosingSuperClassContext(clazz) != NoContext

      def isSubThisType(pre: Type, clazz: Symbol): boolean = pre match {
        case ThisType(pclazz) => pclazz isNonBottomSubClass clazz
        case _ => false
      }

      (pre == NoPrefix) || {
        val ab = sym.accessBoundary(sym.owner)
        ((ab == NoSymbol)
         ||
         (accessWithin(ab) || accessWithin(ab.linkedClassOfClass)) &&
         (!sym.hasFlag(LOCAL) ||
          (sym hasFlag PROTECTED) && isSubThisType(pre, sym.owner) ||
          pre =:= sym.owner.thisType)
         ||
         (sym hasFlag PROTECTED) &&
         (superAccess ||
          (pre.widen.symbol.isNonBottomSubClass(sym.owner) &&
           (isSubClassOfEnclosing(pre.widen.symbol) || phase.erasedTypes))))
        // note: phase.erasedTypes disables last test, because fater addinterfaces
        // implementation classes are not in the superclass chain. If we enable the
        // test, bug780 fails.
      }
    }

    def pushTypeBounds(sym: Symbol): unit = {
      savedTypeBounds = (sym, sym.info) :: savedTypeBounds
    }

    def restoreTypeBounds(tp: Type): Type = {
      var current = tp
      for (val (sym, info) <- savedTypeBounds) {
        if (settings.debug.value) log("resetting " + sym + " to " + info);
        sym.info match {
          case TypeBounds(lo, hi) if (hi <:< lo && lo <:< hi) =>
            current = current.subst(List(sym), List(lo))
          case _ =>
        }
        sym.setInfo(info)
      }
      savedTypeBounds = List()
      current
    }

    private var implicitsCache: List[List[ImplicitInfo]] = null
    private var implicitsRunId = NoRunId

    private def collectImplicits(syms: List[Symbol], pre: Type): List[ImplicitInfo] =
      for (val sym <- syms; sym.hasFlag(IMPLICIT) && isAccessible(sym, pre, false))
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
      if (implicitsRunId != currentRunId) {
        implicitsRunId = currentRunId
        implicitsCache = List()
        val newImplicits: List[ImplicitInfo] =
          if (owner != nextOuter.owner && owner.isClass && !owner.isPackageClass) {
            if (!owner.isInitialized) return nextOuter.implicitss
            if (settings.debug.value)
              log("collect member implicits " + owner + ", implicit members = " +
                  owner.thisType.implicitMembers)//debug
            collectImplicits(owner.thisType.implicitMembers, owner.thisType)
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
  }

  class ImportInfo(val tree: Import, val depth: int) {

    /** The prefix expression */
    def qual: Tree = tree.symbol.info match {
      case ImportType(expr) => expr
      case _ => throw new FatalError("symbol " + tree.symbol + " has bad type: " + tree.symbol.info);//debug
    }

    /** Is name imported explicitly, not via wildcard? */
    def isExplicitImport(name: Name): boolean =
      tree.selectors exists (._2.==(name.toTermName))

    /** The symbol with name <code>name</code> imported from import clause
     *  <code>tree</code>.
     */
    def importedSymbol(name: Name): Symbol = {
      var result: Symbol = NoSymbol
      var renamed = false
      var selectors = tree.selectors
      while (selectors != Nil && result == NoSymbol) {
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
  }

  class ImplicitInfo(val name: Name, val pre: Type, val sym: Symbol) {
    private var tpeCache: Type = null
    def tpe: Type = {
      if (tpeCache eq null) tpeCache = pre.memberType(sym)
      tpeCache
    }
    override def toString = "ImplicitInfo(" + name + "," + pre + "," + sym + ")"
  }

  val NoImplicitInfo = new ImplicitInfo(null, null, null)

  case class ImportType(expr: Tree) extends Type
}
