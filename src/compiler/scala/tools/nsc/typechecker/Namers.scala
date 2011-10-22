/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.ref.WeakReference
import symtab.Flags._
import scala.tools.nsc.io.AbstractFile

/** This trait declares methods to create symbols and to enter them into scopes.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Namers { self: Analyzer =>
  import global._
  import definitions._

  private var _lockedCount = 0
  def lockedCount = this._lockedCount

  /** Convert to corresponding type parameters all skolems of method parameters
   *  which appear in `tparams`.
   */
  class DeSkolemizeMap(tparams: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args)
      if (sym.isTypeSkolem && (tparams contains sym.deSkolemize)) =>
//        println("DESKOLEMIZING "+sym+" in "+sym.owner)
        mapOver(typeRef(NoPrefix, sym.deSkolemize, args))
/*
      case PolyType(tparams1, restpe) =>
        new DeSkolemizeMap(tparams1 ::: tparams).mapOver(tp)
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = parents mapConserve (this)
        if (parents1 eq parents) tp else ClassInfoType(parents1, decls, clazz)
*/
      case _ =>
        mapOver(tp)
    }
  }

  /** Replaces any Idents for which cond is true with fresh TypeTrees().
   *  Does the same for any trees containing EmptyTrees.
   */
  private class TypeTreeSubstituter(cond: Name => Boolean) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(name) if cond(name) => TypeTree()
      case _                         => super.transform(tree)
    }
    def apply(tree: Tree) = {
      val r = transform(tree)
      if (r.exists(_.isEmpty)) TypeTree()
      else r
    }
  }

  private def isTemplateContext(ctx: Context): Boolean = ctx.tree match {
    case Template(_, _, _) => true
    case Import(_, _)      => isTemplateContext(ctx.outer)
    case _                 => false
  }

  private class NormalNamer(context: Context) extends Namer(context)
  def newNamer(context: Context): Namer = new NormalNamer(context)

  // In the typeCompleter (templateSig) of a case class (resp it's module),
  // synthetic `copy` (reps `apply`, `unapply`) methods are added. To compute
  // their signatures, the corresponding ClassDef is needed.
  // During naming, for each case class module symbol, the corresponding ClassDef
  // is stored in this map. The map is cleared lazily, i.e. when the new symbol
  // is created with the same name, the old one (if present) is wiped out, or the
  // entry is deleted when it is used and no longer needed.
  private val caseClassOfModuleClass = perRunCaches.newWeakMap[Symbol, WeakReference[ClassDef]]()

  // Default getters of constructors are added to the companion object in the
  // typeCompleter of the constructor (methodSig). To compute the signature,
  // we need the ClassDef. To create and enter the symbols into the companion
  // object, we need the templateNamer of that module class.
  // This map is extended during naming of classes, the Namer is added in when
  // it's available, i.e. in the type completer (templateSig) of the module class.
  private[typechecker] val classAndNamerOfModule = perRunCaches.newMap[Symbol, (ClassDef, Namer)]()

  def resetNamer() {
    classAndNamerOfModule.clear()
  }

  abstract class Namer(val context: Context) {
    val typer = newTyper(context)

    private lazy val innerNamer = makeInnerNamer()

    def enterValueParams(vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      for (vparams <- vparamss) yield {
        for (param <- vparams) yield {
          val sym = assignSymbol(param, param.name, mask = ValueParameterFlags)
          setPrivateWithin(param, sym)
          enterInScope(sym)
          sym setInfo monoTypeCompleter(param)
        }
      }
    }

    private def owner       = context.owner
    private def contextFile = context.unit.source.file
    private def isInJava    = context.unit.isJava
    private def typeErrorHandler[T](pos: Position, alt: T): PartialFunction[Throwable, T] = {
      case ex: TypeError =>
        typer.reportTypeError(pos, ex)
        alt
    }
    private def isPureField(vd: ValDef) = {
      val ValDef(mods, name, _, _) = vd

      !mods.isLazy && (
           !owner.isClass
        || (mods.isPrivateLocal && !mods.isCaseAccessor)
        || name.startsWith(nme.OUTER)
        || isInJava
      )
    }

    def setPrivateWithin[Sym <: Symbol](tree: Tree, sym: Sym, mods: Modifiers): Sym = {
      if (!sym.isPrivateLocal && mods.hasAccessBoundary)
        sym.privateWithin = typer.qualifyingClass(tree, mods.privateWithin, true)
      sym
    }
    def setPrivateWithin(tree: MemberDef, sym: Symbol): Symbol =
      setPrivateWithin(tree, sym, tree.mods)

    def inConstructorFlag: Long =
      if (owner.isConstructor && !context.inConstructorSuffix || owner.isEarlyInitialized) INCONSTRUCTOR
      else 0l

    def moduleClassFlags(moduleFlags: Long) =
      (moduleFlags & ModuleToClassFlags) | inConstructorFlag

    private def resetKeepingFlags(sym: Symbol, keeping: Long): Symbol = {
      val keep = sym.flags & keeping
      sym reset NoType
      sym setFlag keep
    }

    def updatePosFlags(sym: Symbol, pos: Position, flags: Long): Symbol = {
      debuglog("[overwrite] " + sym)
      resetKeepingFlags(sym, LOCKED)
      sym setFlag flags
      sym setPos pos

      if (sym.isModule && sym.moduleClass != NoSymbol)
        updatePosFlags(sym.moduleClass, pos, moduleClassFlags(flags))

      if (sym.owner.isPackageClass) {
        val companion = companionSymbolOf(sym, context)
        if (companion != NoSymbol) {
          val assignNoType = companion.rawInfo match {
            case _: SymLoader => true
            case tp           => tp.isComplete && (runId(sym.validTo) != currentRunId)
          }
          // pre-set linked symbol to NoType, in case it is not loaded together with this symbol.
          if (assignNoType)
            companion setInfo NoType
        }
      }
      sym
    }
    protected def makeConstructorScope(classContext : Context) : Context = {
      val outerContext = classContext.outer.outer
      outerContext.makeNewScope(outerContext.tree, outerContext.owner)
    }
    private def makeInnerNamer() = {
      if (!isTemplateContext(context)) this
      else newNamer(context.make(context.tree, owner, new Scope))
    }

    def namerOf(sym: Symbol): Namer = {
      def primaryConstructorParamNamer: Namer = { //todo: can we merge this with SCCmode?
        val classContext     = context.enclClass
        val paramContext     = makeConstructorScope(classContext)
        val unsafeTypeParams = owner.unsafeTypeParams

        unsafeTypeParams foreach (paramContext.scope enter _)
        newNamer(paramContext)
      }

      val usePrimary = sym.isTerm && (
           (sym.isParamAccessor)
        || (sym.isParameter && sym.owner.isPrimaryConstructor)
      )

      if (usePrimary) primaryConstructorParamNamer
      else innerNamer
    }

    protected def conflict(newS: Symbol, oldS: Symbol) = (
       (   !oldS.isSourceMethod
        || nme.isSetterName(newS.name)
        || newS.owner.isPackageClass
       ) &&
      !(   // @M: allow repeated use of `_` for higher-order type params
           (newS.owner.isTypeParameter || newS.owner.isAbstractType)
           // FIXME: name comparisons not successful, are these underscores
           // sometimes nme.WILDCARD and sometimes tpnme.WILDCARD?
        && (newS.name.toString == nme.WILDCARD.toString)
       )
    )

    private def doubleDefError(pos: Position, sym: Symbol) {
      val s1 = if (sym.isModule) "case class companion " else ""
      val s2 = if (sym.isSynthetic) "(compiler-generated) " + s1 else ""
      val s3 = if (sym.isCase) "case class " + sym.name else "" + sym

      context.error(pos, sym.name + " is already defined as " + s2 + s3)
    }

    private def allowsOverload(sym: Symbol) = (
      sym.isSourceMethod && sym.owner.isClass && !sym.owner.isPackageClass
    )

    private def inCurrentScope(m: Symbol): Boolean = {
      if (owner.isClass) owner == m.owner
      else m.owner.isClass && context.scope == m.owner.info.decls
    }

    /** Enter symbol into context's scope and return symbol itself */
    def enterInScope(sym: Symbol): Symbol = enterInScope(sym, context.scope)

    /** Enter symbol into given scope and return symbol itself */
    def enterInScope(sym: Symbol, scope: Scope): Symbol = {
      // allow for overloaded methods
      if (!allowsOverload(sym)) {
        val prev = scope.lookupEntry(sym.name)
        if ((prev ne null) && prev.owner == scope && conflict(sym, prev.sym)) {
          doubleDefError(sym.pos, prev.sym)
          sym setInfo ErrorType
          scope unlink prev.sym // let them co-exist...
          // FIXME: The comment "let them co-exist" is confusing given that the
          // line it comments unlinks one of them.  What does it intend?
        }
      }
      scope enter sym
    }

    def enterSym(tree: Tree): Context = {
      def dispatch() = {
        var returnContext = this.context
        tree match {
          case tree @ PackageDef(_, _)                       => enterPackage(tree)
          case tree @ ClassDef(_, _, _, _)                   => enterClassDef(tree)
          case tree @ ModuleDef(_, _, _)                     => enterModuleDef(tree)
          case tree @ ValDef(_, _, _, _)                     => enterValDef(tree)
          case tree @ DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => enterConstructor(tree)
          case tree @ DefDef(_, _, _, _, _, _)               => enterDefDef(tree)
          case tree @ TypeDef(_, _, _, _)                    => enterTypeDef(tree)
          case DocDef(_, defn)                               => enterSym(defn)
          case tree @ Import(_, _)                           =>
            assignSymbol(tree)
            returnContext = context.makeNewImport(tree)
          case _ =>
        }
        returnContext
      }
      tree.symbol match {
        case NoSymbol => try dispatch() catch typeErrorHandler(tree.pos, this.context)
        case sym      => enterExistingSym(sym)
      }
    }

    /** Creates a new symbol and assigns it to the tree, returning the symbol
     */
    def assignSymbol(tree: Tree): Symbol =
      logAssignSymbol(tree, tree match {
        case PackageDef(pid, _) => createPackageSymbol(tree.pos, pid)
        case Import(_, _)       => createImportSymbol(tree)
        case mdef: MemberDef    => createMemberSymbol(mdef, mdef.name, -1L)
        case _                  => abort("Unexpected tree: " + tree)
      })
    def assignSymbol(tree: MemberDef, name: Name, mask: Long): Symbol =
      logAssignSymbol(tree, createMemberSymbol(tree, name, mask))

    def assignAndEnterSymbol(tree: MemberDef): Symbol = {
      val sym = assignSymbol(tree, tree.name, -1L)
      setPrivateWithin(tree, sym)
      enterInScope(sym)
    }
    def assignAndEnterLazyInfoSymbol(tree: MemberDef): Symbol = {
      val sym = assignAndEnterSymbol(tree)
      sym setInfo completerOf(tree)
      log("[+info] " + sym.fullLocationString)
      sym
    }

    private def logAssignSymbol(tree: Tree, sym: Symbol): Symbol = {
      log("[+symbol] " + sym.hasFlagsToString(-1L) + " " + sym)
      tree.symbol = sym
      sym
    }

    /** Create a new symbol at the context owner based on the given tree.
     *  A different name can be given.  If the modifier flags should not be
     *  be transferred to the symbol as they are, supply a mask containing
     *  the flags to keep.
     */
    private def createMemberSymbol(tree: MemberDef, name: Name, mask: Long): Symbol = {
      val pos         = tree.pos
      val isParameter = tree.mods.isParameter
      val sym         = tree match {
        case TypeDef(_, _, _, _) if isParameter     => owner.newAbstractType(pos, name.toTypeName)
        case TypeDef(_, _, _, _)                    => owner.newAliasType(pos, name.toTypeName)
        case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => owner.newConstructor(pos)
        case DefDef(_, _, _, _, _, _)               => owner.newMethod(pos, name.toTermName)
        case ClassDef(_, _, _, _)                   => owner.newClass(pos, name.toTypeName)
        case ModuleDef(_, _, _)                     => owner.newModule(pos, name)
        case ValDef(_, _, _, _) if isParameter      => owner.newValueParameter(pos, name)
        case PackageDef(pid, _)                     => createPackageSymbol(pos, pid)
        case ValDef(_, _, _, _)                     => owner.newValue(pos, name)
      }
      sym setFlag (tree.mods.flags & mask)
    }

    private def createFieldSymbol(tree: ValDef): TermSymbol = (
      owner.newValue(tree.pos, nme.getterToLocal(tree.name))
        setFlag tree.mods.flags & FieldFlags | PrivateLocal
    )

    private def createImportSymbol(tree: Tree) =
      NoSymbol.newImport(tree.pos) setInfo completerOf(tree)

    /** All PackageClassInfoTypes come from here. */
    private def createPackageSymbol(pos: Position, pid: RefTree): Symbol = {
      val pkgOwner = pid match {
        case Ident(_)                 => if (owner == EmptyPackageClass) RootClass else owner
        case Select(qual: RefTree, _) => createPackageSymbol(pos, qual).moduleClass
      }
      val existing = pkgOwner.info.decls.lookup(pid.name)

      if (existing.isPackage && pkgOwner == existing.owner)
        existing
      else {
        val pkg          = pkgOwner.newPackage(pos, pid.name.toTermName)
        val pkgClass     = pkg.moduleClass
        val pkgClassInfo = new PackageClassInfoType(newPackageScope(pkgClass), pkgClass)

        pkgClass setInfo pkgClassInfo
        pkg setInfo pkgClass.tpe
        enterInScope(pkg, pkgOwner.info.decls)
      }
    }

    private def enterClassSymbol(tree: ClassDef, clazz: ClassSymbol): Symbol = {
      val file = contextFile
      if (clazz.sourceFile != null && clazz.sourceFile != contextFile)
        debugwarn("!!! Source mismatch in " + clazz + ": " + clazz.sourceFile + " vs. " + contextFile)

      clazz.sourceFile = contextFile
      if (clazz.sourceFile != null) {
        assert(currentRun.canRedefine(clazz) || clazz.sourceFile == currentRun.symSource(clazz), clazz.sourceFile)
        currentRun.symSource(clazz) = clazz.sourceFile
      }
      registerTopLevelSym(clazz)
      assert(clazz.name.toString.indexOf('(') < 0, clazz.name)  // )
      clazz
    }

    def enterClassSymbol(tree: ClassDef): Symbol = {
      val existing = context.scope.lookup(tree.name)
      val isRedefinition = (
           existing.isType
        && existing.owner.isPackageClass
        && context.scope == existing.owner.info.decls
        && currentRun.canRedefine(existing)
      )
      val clazz: Symbol = {
        if (isRedefinition) {
          updatePosFlags(existing, tree.pos, tree.mods.flags)
          setPrivateWithin(tree, existing)
          existing
        }
        else assignAndEnterSymbol(tree) setFlag inConstructorFlag
      }
      clazz match {
        case csym: ClassSymbol if csym.owner.isPackageClass => enterClassSymbol(tree, csym)
        case _                                              => clazz
      }
    }

    def enterModuleDef(tree: ModuleDef) = {
      val sym = enterModuleSymbol(tree)
      sym.moduleClass setInfo namerOf(sym).moduleClassTypeCompleter(tree)
      sym setInfo completerOf(tree)
    }

    /** Enter a module symbol. The tree parameter can be either
     *  a module definition or a class definition.
     */
    def enterModuleSymbol(tree : ModuleDef): Symbol = {
      var m: Symbol = context.scope.lookup(tree.name)
      val moduleFlags = tree.mods.flags | MODULE
      if (m.isModule && !m.isPackage && inCurrentScope(m) && (currentRun.canRedefine(m) || m.isSynthetic)) {
        updatePosFlags(m, tree.pos, moduleFlags)
        setPrivateWithin(tree, m)
        if (m.moduleClass != NoSymbol)
          setPrivateWithin(tree, m.moduleClass)

        context.unit.synthetics -= m
        tree.symbol = m
      }
      else {
        m = assignAndEnterSymbol(tree)
        m.moduleClass setFlag moduleClassFlags(moduleFlags)
        setPrivateWithin(tree, m.moduleClass)
      }
      if (m.owner.isPackageClass && !m.isPackage) {
        m.moduleClass.sourceFile = contextFile
        currentRun.symSource(m) = m.moduleClass.sourceFile
        registerTopLevelSym(m)
      }
      m
    }

    def enterSyms(trees: List[Tree]): Namer = {
      trees.foldLeft(this: Namer) { (namer, t) =>
        val ctx = namer enterSym t
        if (ctx eq namer.context) namer
        else newNamer(ctx)
      }
    }
    def applicableTypeParams(owner: Symbol): List[Symbol] =
      if (owner.isTerm || owner.isPackageClass) Nil
      else applicableTypeParams(owner.owner) ::: owner.typeParams

    /** If no companion object for clazz exists yet, create one by applying `creator` to
     *  class definition tree.
     *  @return the companion object symbol.
     */
    def ensureCompanionObject(tree: ClassDef, creator: => Tree): Symbol = {
      val m = companionModuleOf(tree.symbol, context)
      // @luc: not sure why "currentRun.compiles(m)" is needed, things breaks
      // otherwise. documentation welcome.
      //
      // @PP: I tried to reverse engineer said documentation.  The only tests
      // which fail are buildmanager tests, as follows.  Given A.scala:
      //   case class Foo()
      // If you recompile A.scala, the Changes Map is
      //   Map(class Foo -> Nil, object Foo -> Nil)
      // But if you remove the 'currentRun.compiles(m)' condition, it is
      //   Map(class Foo -> Nil)
      // What exactly this implies and whether this is a sensible way to
      // enforce it, I don't know.
      if (m != NoSymbol && currentRun.compiles(m)) m
      else enterSyntheticSym(creator)
    }

    private def checkSelectors(tree: Import): Unit = {
      val Import(expr, selectors) = tree
      val base = expr.tpe

      def checkNotRedundant(pos: Position, from: Name, to0: Name) {
        def check(to: Name) = {
          val e = context.scope.lookupEntry(to)

          if (e != null && e.owner == context.scope && e.sym.exists)
            typer.permanentlyHiddenWarning(pos, to0, e.sym)
          else if (context ne context.enclClass) {
            val defSym = context.prefix.member(to) filter (
              sym => sym.exists && context.isAccessible(sym, context.prefix, false))

            if (defSym != NoSymbol)
              typer.permanentlyHiddenWarning(pos, to0, defSym)
          }
        }
        if (!tree.symbol.isSynthetic && expr.symbol != null && !expr.symbol.isInterpreterWrapper) {
          if (base.member(from) != NoSymbol)
            check(to0)
          if (base.member(from.toTypeName) != NoSymbol)
            check(to0.toTypeName)
        }
      }
      def checkSelector(s: ImportSelector) = {
        val ImportSelector(from, fromPos, to, _) = s
        def isValid(original: Name) =
          original.bothNames forall (x => (base nonLocalMember x) == NoSymbol)

        if (from != nme.WILDCARD && base != ErrorType) {
          if (isValid(from)) {
            if (currentRun.compileSourceFor(expr, from)) {
              // side effecting, apparently
              typeSig(tree)
            }
            // for Java code importing Scala objects
            else if (!nme.isModuleName(from) || isValid(nme.stripModuleSuffix(from)))
              notAMemberError(tree.pos, expr, from)
          }
          // Setting the position at the import means that if there is
          // more than one hidden name, the second will not be warned.
          // So it is the position of the actual hidden name.
          checkNotRedundant(tree.pos withPoint fromPos, from, to)
        }
      }
      def noDuplicates(names: List[Name], message: String) {
        def loop(xs: List[Name]): Unit = xs match {
          case Nil      => ()
          case hd :: tl =>
            if (hd == nme.WILDCARD || !(tl contains hd)) loop(tl)
            else context.error(tree.pos, hd.decode + " " + message)
        }
        loop(names filterNot (x => x == null || x == nme.WILDCARD))
      }
      selectors foreach checkSelector

      // checks on the whole set
      noDuplicates(selectors map (_.name), "is renamed twice")
      noDuplicates(selectors map (_.rename), "appears twice as a target of a renaming")
    }

    def enterCopyMethodOrGetter(tree: Tree, tparams: List[TypeDef]): Symbol = {
      val sym          = tree.symbol
      val lazyType     = completerOf(tree, tparams)
      def completeCopyFirst = sym.isSynthetic && (!sym.hasDefaultFlag || sym.owner.info.member(nme.copy).isSynthetic)
      def completeCopyMethod(clazz: Symbol) {
        // the 'copy' method of case classes needs a special type completer to make
        // bug0054.scala (and others) work. the copy method has to take exactly the same
        // parameter types as the primary constructor.
        val constructorType = clazz.primaryConstructor.tpe
        val subst           = new SubstSymMap(clazz.typeParams, tparams map (_.symbol))
        val vparamss        = tree match { case x: DefDef => x.vparamss ; case _ => Nil }
        val cparamss        = constructorType.paramss

        for ((vparams, cparams) <- vparamss zip cparamss) {
          for ((param, cparam) <- vparams zip cparams) {
            // need to clone the type cparam.tpe???
            // problem is: we don't have the new owner yet (the new param symbol)
            param.tpt setType subst(cparam.tpe)
          }
        }
      }
      sym setInfo {
        mkTypeCompleter(tree) { copySym =>
          if (completeCopyFirst)
            completeCopyMethod(copySym.owner)

          lazyType complete sym
        }
      }
    }
    def completerOf(tree: Tree): TypeCompleter = completerOf(tree, treeInfo.typeParameters(tree))
    def completerOf(tree: Tree, tparams: List[TypeDef]): TypeCompleter = {
      val mono = namerOf(tree.symbol) monoTypeCompleter tree
      if (tparams.isEmpty) mono
      else {
        val sym = tree.symbol
        //@M! TypeDef's type params are handled differently
        //@M e.g., in [A[x <: B], B], A and B are entered first as both are in scope in the definition of x
        //@M x is only in scope in `A[x <: B]'
        if (!sym.isAbstractType) //@M TODO: change to isTypeMember ?
          newNamer(context.makeNewScope(tree, sym)) enterSyms tparams

        new PolyTypeCompleter(tparams, mono, tree, context) //@M
      }
    }

    def enterIfNotThere(sym: Symbol) {
      val scope = context.scope
      var e = scope.lookupEntry(sym.name)
      while ((e ne null) && (e.owner eq scope) && (e.sym ne sym)) e = e.tail
      if (!((e ne null) && (e.owner eq scope))) context.scope.enter(sym)
    }

    def enterValDef(tree: ValDef) {
      val ValDef(mods, name, tp, rhs) = tree

      if (isPureField(tree))
        enterPureField(tree)
      else {
        if (mods.isPrivateLocal && !mods.isLazy)
          context.error(tree.pos, "private[this] not allowed for case class parameters")
        if (nme.isSetterName(name))
          context.error(tree.pos, "Names of vals or vars may not end in `_='")

        // add getter and possibly also setter
        val getter = enterGetterDef(tree, name)
        if (mods.isMutable)
          enterSetterDef(tree, name)

        tree.symbol = (
          if (mods.isDeferred) getter setPos tree.pos       // unfocus getter position, no separate value
          else if (mods.isLazy) enterLazyVal(tree) setLazyAccessor getter
          else enterStrictVal(tree)
        )

        if (!forMSIL)
          addBeanGetterSetter(tree, getter)
      }
    }

    def enterPackage(tree: PackageDef) {
      val sym = assignSymbol(tree)
      newNamer(context.make(tree, sym.moduleClass, sym.info.decls)) enterSyms tree.stats
    }

    def enterLazyVal(tree: ValDef): TermSymbol = {
      // If the owner is not a class, this is a lazy val from a method,
      // and not a field.  It has $lzy appended to its name and its flags
      // are different.  Unknown: why the IMPLICIT flag is reset.
      val sym = (
        if (owner.isClass) createFieldSymbol(tree)
        else owner.newValue(tree.pos, tree.name + "$lzy") setFlag tree.mods.flags resetFlag IMPLICIT
      )
      enterValSymbol(tree, sym setFlag MUTABLE)
    }
    def enterStrictVal(tree: ValDef): TermSymbol = {
      enterValSymbol(tree, createFieldSymbol(tree))
    }
    def enterValSymbol(tree: ValDef, sym: TermSymbol): TermSymbol = {
      enterInScope(sym)
      sym setInfo namerOf(sym).monoTypeCompleter(tree)
    }

    def enterConstructor(tree: DefDef) = assignAndEnterLazyInfoSymbol(tree) setFlag owner.getFlag(ConstrFlags)
    def enterTypeDef(tree: TypeDef)    = assignAndEnterLazyInfoSymbol(tree)
    def enterPureField(tree: ValDef)   = assignAndEnterLazyInfoSymbol(tree)

    def enterDefDef(tree: DefDef) {
      val DefDef(mods, name, tparams, _, _, _) = tree
      val bridgeFlag = if (mods hasAnnotationNamed tpnme.bridgeAnnot) BRIDGE else 0
      val sym        = assignAndEnterSymbol(tree) setFlag bridgeFlag

      if (name == nme.copy || (tree.symbol.name.startsWith(nme.copy + nme.DEFAULT_GETTER_STRING)))
        enterCopyMethodOrGetter(tree, tparams)
      else
        sym setInfo completerOf(tree, tparams)
    }

    def enterClassDef(tree: ClassDef) {
      val ClassDef(mods, name, tparams, impl) = tree
      tree.symbol = enterClassSymbol(tree)
      tree.symbol setInfo completerOf(tree)

      if (mods.isCase) {
        if (treeInfo.firstConstructorArgs(impl.body).size > MaxFunctionArity)
          context.error(tree.pos, "Implementation restriction: case classes cannot have more than " + MaxFunctionArity + " parameters.")

        val m = ensureCompanionObject(tree, caseModuleDef(tree))
        caseClassOfModuleClass(m.moduleClass) = new WeakReference(tree)
      }
      val hasDefault = impl.body exists {
        case DefDef(_, nme.CONSTRUCTOR, _, vparamss, _, _)  => vparamss.flatten exists (_.mods.hasDefault)
        case _                                              => false
      }
      if (hasDefault) {
        val m = ensureCompanionObject(tree, companionModuleDef(tree))
        classAndNamerOfModule(m) = (tree, null)
      }
      val owner = tree.symbol.owner
      if (owner.isPackageObjectClass) {
        context.unit.warning(tree.pos,
          "it is not recommended to define classes/objects inside of package objects.\n" +
          "If possible, define " + tree.symbol + " in " + owner.skipPackageObject + " instead."
        )
      }
    }

    // this logic is needed in case typer was interrupted half
    // way through and then comes back to do the tree again. In
    // that case the definitions that were already attributed as
    // well as any default parameters of such methods need to be
    // re-entered in the current scope.
    protected def enterExistingSym(sym: Symbol): Context = {
      if (forInteractive && sym != null && sym.owner.isTerm) {
        enterIfNotThere(sym)
        if (sym.isLazy) {
          val acc = sym.lazyAccessor
          if (acc != NoSymbol) enterIfNotThere(acc)
        }
        defaultParametersOfMethod(sym) foreach enterIfNotThere
      }
      this.context
    }

    def enterSyntheticSym(tree: Tree): Symbol = {
      enterSym(tree)
      context.unit.synthetics(tree.symbol) = tree
      tree.symbol
    }
    def enterAccessorMethod(tree: MemberDef, name: Name, flags: Long): Symbol = {
      val sym = owner.newMethod(tree.pos.focus, name.toTermName) setFlag flags
      setPrivateWithin(tree, sym)
      enterInScope(sym)
    }
    def enterGetterDef(tree: ValDef, name: Name) = {
      val getter = enterAccessorMethod(tree, name, getterFlags(tree.mods.flags))
      getter setInfo namerOf(getter).getterTypeCompleter(tree)
    }
    def enterSetterDef(tree: ValDef, name: Name) = {
      val setter = enterAccessorMethod(tree, nme.getterToSetter(name), setterFlags(tree.mods.flags))
      setter setInfo namerOf(setter).setterTypeCompleter(tree)
    }

    private def addBeanGetterSetter(tree: ValDef, getter: Symbol) {
      val ValDef(mods, name, tpt, _) = tree
      val hasBP     = mods hasAnnotationNamed tpnme.BeanPropertyAnnot
      val hasBoolBP = mods hasAnnotationNamed tpnme.BooleanBeanPropertyAnnot

      if (hasBP || hasBoolBP) {
        if (!name(0).isLetter)
          context.error(tree.pos, "`BeanProperty' annotation can be applied only to fields that start with a letter")
        else if (mods.isPrivate)
          // avoids name clashes with private fields in traits
          context.error(tree.pos, "`BeanProperty' annotation can be applied only to non-private fields")
        else {
          val flags         = mods.flags & BeanPropertyFlags
          val beanName      = name.toString.capitalize
          val getterName    = if (hasBoolBP) "is" + beanName else "get" + beanName
          val getterMods    = Modifiers(flags, mods.privateWithin, Nil) setPositions mods.positions

          enterSyntheticSym {
            atPos(tree.pos.focus) {
              DefDef(getterMods, getterName, Nil, List(Nil), tpt.duplicate,
                if (mods.isDeferred) EmptyTree
                else Select(This(getter.owner.name.toTypeName), name)
              )
            }
          }

          if (mods.isMutable) {
            // can't use "enterSyntheticSym", because the parameter type is not yet
            // known. instead, uses the same machinery as for the non-bean setter:
            // create and enter the symbol here, add the tree in Typer.addGettterSetter.
            val setter = enterAccessorMethod(tree, "set" + beanName, flags)
            setter setPos tree.pos.focus setInfo namerOf(setter).setterTypeCompleter(tree)
          }
        }
      }
    }

// --- Lazy Type Assignment --------------------------------------------------

    def initializeLowerBounds(tp: Type): Type = {
      tp match {
        case TypeBounds(lo, _) =>
          // check that lower bound is not an F-bound
          for (TypeRef(_, sym, _) <- lo)
            sym.initialize
        case _ =>
      }
      tp
    }

    def monoTypeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      logAndValidate(sym) {
        val tp   = initializeLowerBounds(typeSig(tree))
        val info = if (sym.isJavaDefined) RestrictJavaArraysMap(tp) else tp
        sym setInfo info

        // this early test is there to avoid infinite baseTypes when
        // adding setters and getters --> bug798
        val needsCycleCheck = (sym.isAliasType || sym.isAbstractType) && !sym.isParameter
        if (needsCycleCheck && !typer.checkNonCyclic(tree.pos, tp))
          sym setInfo ErrorType
      }
    }

    def moduleClassTypeCompleter(tree: Tree) = {
      mkTypeCompleter(tree) { sym =>
        val moduleSymbol = tree.symbol
        assert(moduleSymbol.moduleClass == sym, moduleSymbol.moduleClass)
        moduleSymbol.info // sets moduleClass info as a side effect.
      }
    }

    def getterTypeCompleter(tree: ValDef) = mkTypeCompleter(tree) { sym =>
      logAndValidate(sym)(sym setInfo NullaryMethodType(typeSig(tree)))
    }

    def setterTypeCompleter(tree: ValDef) = mkTypeCompleter(tree) { sym =>
      logAndValidate(sym) {
        val param = sym.newSyntheticValueParam(typeSig(tree))
        sym setInfo MethodType(List(param), UnitClass.tpe)
      }
    }

    def selfTypeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      val selftpe = typer.typedType(tree).tpe
      sym setInfo {
        if (selftpe.typeSymbol isNonBottomSubClass sym.owner) selftpe
        else intersectionType(List(sym.owner.tpe, selftpe))
      }
    }

    /** This method has a big impact on the eventual compiled code.
     *  At this point many values have the most specific possible
     *  type (e.g. in val x = 42, x's type is Int(42), not Int) but
     *  most need to be widened to avoid undesirable propagation of
     *  those singleton types.
     *
     *  However, the compilation of pattern matches into switch
     *  statements depends on constant folding, which will only take
     *  place for those values which aren't widened.  The "final"
     *  modifier is the present means of signaling that a constant
     *  value should not be widened, so it has a use even in situations
     *  whether it is otherwise redundant (such as in a singleton.)
     */
    private def widenIfNecessary(sym: Symbol, tpe: Type, pt: Type): Type = {
      val getter =
        if (sym.isValue && sym.owner.isClass && sym.isPrivate)
          sym.getter(sym.owner)
        else sym
      def isHidden(tp: Type): Boolean = tp match {
        case SingleType(pre, sym) =>
          (sym isLessAccessibleThan getter) || isHidden(pre)
        case ThisType(sym) =>
          sym isLessAccessibleThan getter
        case p: SimpleTypeProxy =>
          isHidden(p.underlying)
        case _ =>
          false
      }

      val tpe1 = dropRepeatedParamType(tpe.deconst)
      val tpe2 = tpe1.widen

      // This infers Foo.type instead of "object Foo"
      // See Infer#adjustTypeArgs for the polymorphic case.
      if (tpe.typeSymbolDirect.isModuleClass) tpe1
      else if (sym.isVariable || sym.isMethod && !sym.hasAccessorFlag)
        if (tpe2 <:< pt) tpe2 else tpe1
      else if (isHidden(tpe)) tpe2
      // In an attempt to make pattern matches involving method local vals
      // compilable into switches, for a time I had a more generous condition:
      //    `if (sym.isFinal || sym.isLocal) tpe else tpe1`
      // This led to issues with expressions like classOf[List[_]] which apparently
      // depend on being deconst-ed here, so this is again the original:
      else if (!sym.isFinal) tpe1
      else tpe
    }

    def enterSelf(self: ValDef) {
      val clazz = context.owner
      val ValDef(mods, name, tpt, rhs) = self

      if (!tpt.isEmpty) {
        clazz.typeOfThis = selfTypeCompleter(self.tpt)
        self.symbol      = clazz.thisSym.setPos(self.pos)
      }
      else {
        self.tpt defineType NoType
        if (self.name != nme.WILDCARD) {
          clazz.typeOfThis = clazz.tpe
          self.symbol = clazz.thisSym
        }
        else if (self ne emptyValDef) {
          self.symbol = clazz.newThisSym(self.pos) setInfo clazz.tpe
        }
      }
      if (self.name != nme.WILDCARD) {
        self.symbol.name = self.name
        self.symbol = context.scope enter self.symbol
      }
    }

    private def templateSig(templ: Template): Type = {
      val clazz = context.owner
      def checkParent(tpt: Tree): Type = {
        val tp = tpt.tpe
        val inheritsSelf = tp.typeSymbol == owner
        if (inheritsSelf)
          context.error(tpt.pos, ""+tp.typeSymbol+" inherits itself")

        if (inheritsSelf || tp.isError) AnyRefClass.tpe
        else tp
      }

      val parents = typer.parentTypes(templ) map checkParent
      enterSelf(templ.self)

      val decls = new Scope
      val templateNamer = newNamer(context.make(templ, clazz, decls))
      templateNamer enterSyms templ.body

      // add apply and unapply methods to companion objects of case classes,
      // unless they exist already; here, "clazz" is the module class
      if (clazz.isModuleClass) {
        Namers.this.caseClassOfModuleClass get clazz foreach { cdefRef =>
          val cdef = cdefRef()
          addApplyUnapply(cdef, templateNamer)
          caseClassOfModuleClass -= clazz
        }
      }

      // add the copy method to case classes; this needs to be done here, not in SyntheticMethods, because
      // the namer phase must traverse this copy method to create default getters for its parameters.
      // here, clazz is the ClassSymbol of the case class (not the module).
      // @check: this seems to work only if the type completer of the class runs before the one of the
      // module class: the one from the module class removes the entry from caseClassOfModuleClass (see above).
      if (clazz.isClass && !clazz.hasModuleFlag) {
        val modClass = companionModuleOf(clazz, context).moduleClass
        Namers.this.caseClassOfModuleClass get modClass map { cdefRef =>
          val cdef = cdefRef()

          def hasCopy(decls: Scope) = (decls lookup nme.copy) != NoSymbol
          if (!hasCopy(decls) &&
                  !parents.exists(p => hasCopy(p.typeSymbol.info.decls)) &&
                  !parents.flatMap(_.baseClasses).distinct.exists(bc => hasCopy(bc.info.decls)))
            addCopyMethod(cdef, templateNamer)
        }
      }

      // if default getters (for constructor defaults) need to be added to that module, here's the namer
      // to use. clazz is the ModuleClass. sourceModule works also for classes defined in methods.
      val module = clazz.sourceModule
      classAndNamerOfModule get module foreach {
        case (cdef, _) =>
          classAndNamerOfModule(module) = (cdef, templateNamer)
      }

      debuglog({
        val ps = parents map (_.typeSymbol) mkString ("\n  ", ", ", "")
        val ds = decls map (">> " + _) mkString ("\n  ", "\n  ", "")

        "ClassInfoType(%s, %s, %s)".format(ps, ds, clazz)
      })
      ClassInfoType(parents, decls, clazz)
    }

    private def classSig(tparams: List[TypeDef], impl: Template): Type = {
      val tparams0   = typer.reenterTypeParams(tparams)
      val resultType = templateSig(impl)

      polyType(tparams0, resultType)
    }

    private def methodSig(mods: Modifiers, tparams: List[TypeDef],
                          vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): Type = {
      val meth  = owner
      val clazz = meth.owner
      // enters the skolemized version into scope, returns the deSkolemized symbols
      val tparamSyms = typer.reenterTypeParams(tparams)
      // since the skolemized tparams are in scope, the TypeRefs in vparamSymss refer to skolemized tparams
      var vparamSymss = enterValueParams(vparamss)
      // DEPMETTODO: do we need to skolemize value parameter symbols?
      if (tpt.isEmpty && meth.name == nme.CONSTRUCTOR) {
        tpt defineType context.enclClass.owner.tpe
        tpt setPos meth.pos.focus
      }

      /** Called for all value parameter lists, right to left
       *  @param vparams the symbols of one parameter list
       *  @param restpe  the result type (possibly a MethodType)
       */
      def makeMethodType(vparams: List[Symbol], restpe: Type) = {
        // TODODEPMET: check that we actually don't need to do anything here
        // new dependent method types: probably OK already, since 'enterValueParams' above
        // enters them in scope, and all have a lazy type. so they may depend on other params. but: need to
        // check that params only depend on ones in earlier sections, not the same. (done by checkDependencies,
        // so re-use / adapt that)
        val params = vparams map (vparam =>
          if (meth.isJavaDefined) vparam.setInfo(objToAny(vparam.tpe)) else vparam)
         // TODODEPMET necessary?? new dependent types: replace symbols in restpe with the ones in vparams
        if (meth.isJavaDefined) JavaMethodType(params, restpe)
        else MethodType(params, restpe)
      }

      def thisMethodType(restpe: Type) = {
        val checkDependencies = new DependentTypeChecker(context)
        checkDependencies check vparamSymss
        // DEPMETTODO: check not needed when they become on by default
        checkDependencies(restpe)

        polyType(
          tparamSyms, // deSkolemized symbols  -- TODO: check that their infos don't refer to method args?
          if (vparamSymss.isEmpty) NullaryMethodType(restpe)
          // vparamss refer (if they do) to skolemized tparams
          else (vparamSymss :\ restpe) (makeMethodType)
        )
      }

      var resultPt = if (tpt.isEmpty) WildcardType else typer.typedType(tpt).tpe
      val site = clazz.thisType

      def overriddenSymbol = intersectionType(clazz.info.parents).nonPrivateMember(meth.name).filter(sym => {
        // luc: added .substSym from skolemized to deSkolemized
        // site.memberType(sym): PolyType(tparams, MethodType(..., ...)) ==> all references to tparams are deSkolemized
        // thisMethodType: tparams in PolyType are deSkolemized, the references in the MethodTypes are skolemized. ==> the two didn't match
        // for instance, B.foo would not override A.foo, and the default on parameter b would not be inherited
        //   class A { def foo[T](a: T)(b: T = a) = a }
        //   class B extends A { override def foo[U](a: U)(b: U) = b }
        sym != NoSymbol && (site.memberType(sym) matches thisMethodType(resultPt).substSym(tparams map (_.symbol), tparamSyms))
      })

      // fill in result type and parameter types from overridden symbol if there is a unique one.
      if (clazz.isClass && (tpt.isEmpty || vparamss.exists(_.exists(_.tpt.isEmpty)))) {
        // try to complete from matching definition in base type
        for (vparams <- vparamss; vparam <- vparams)
          if (vparam.tpt.isEmpty) vparam.symbol setInfo WildcardType
        val overridden = overriddenSymbol
        if (overridden != NoSymbol && !overridden.isOverloaded) {
          overridden.cookJavaRawInfo() // #3404 xform java rawtypes into existentials
          resultPt = site.memberType(overridden) match {
            case PolyType(tparams, rt) => rt.substSym(tparams, tparamSyms)
            case mt => mt
          }

          for (vparams <- vparamss) {
            var pps = resultPt.params
            for (vparam <- vparams) {
              if (vparam.tpt.isEmpty) {
                val paramtpe = pps.head.tpe
                vparam.symbol setInfo paramtpe
                vparam.tpt defineType paramtpe
                vparam.tpt setPos vparam.pos.focus
              }
              pps = pps.tail
            }
            resultPt = resultPt.resultType
          }
          resultPt match {
            case NullaryMethodType(rtpe) => resultPt = rtpe
            case MethodType(List(), rtpe) => resultPt = rtpe
            case _ =>
          }
          if (tpt.isEmpty) {
            // provisionally assign `meth` a method type with inherited result type
            // that way, we can leave out the result type even if method is recursive.
            meth setInfo thisMethodType(resultPt)
          }
        }
      }
      // Add a () parameter section if this overrides some method with () parameters.
      if (clazz.isClass && vparamss.isEmpty && overriddenSymbol.alternatives.exists(
        _.info.isInstanceOf[MethodType])) {
        vparamSymss = List(List())
      }
      for (vparams <- vparamss; vparam <- vparams if vparam.tpt.isEmpty) {
        context.error(vparam.pos, "missing parameter type")
        vparam.tpt defineType ErrorType
      }

      addDefaultGetters(meth, vparamss, tparams, overriddenSymbol)

      thisMethodType({
        val rt = if (tpt.isEmpty) {
          // replace deSkolemized symbols with skolemized ones
          // (for resultPt computed by looking at overridden symbol, right?)
          val pt = resultPt.substSym(tparamSyms, tparams map (_.symbol))
          // compute result type from rhs
          tpt defineType widenIfNecessary(meth, typer.computeType(rhs, pt), pt)
          tpt setPos meth.pos.focus
          tpt.tpe
        } else typer.typedType(tpt).tpe
        // #2382: return type of default getters are always @uncheckedVariance
        if (meth.hasDefaultFlag)
          rt.withAnnotation(AnnotationInfo(uncheckedVarianceClass.tpe, List(), List()))
        else rt
      })
    }

    /**
     * For every default argument, insert a method computing that default
     *
     * Also adds the "override" and "defaultparam" (for inherited defaults) flags
     * Typer is too late, if an inherited default is used before the method is
     * typechecked, the corresponding param would not yet have the "defaultparam"
     * flag.
     */
    private def addDefaultGetters(meth: Symbol, vparamss: List[List[ValDef]], tparams: List[TypeDef], overriddenSymbol: => Symbol) {
      val clazz      = meth.owner
      val isConstr   = meth.isConstructor
      val overridden = if (isConstr || !clazz.isClass) NoSymbol else overriddenSymbol
      val overrides  = overridden != NoSymbol && !overridden.isOverloaded
      // value parameters of the base class (whose defaults might be overridden)
      var baseParamss = (vparamss, overridden.tpe.paramss) match {
        // match empty and missing parameter list
        case (Nil, List(Nil)) => Nil
        case (List(Nil), Nil) => List(Nil)
        case (_, paramss)     => paramss
      }
      assert(
        !overrides || vparamss.length == baseParamss.length,
        "" + meth.fullName + ", "+ overridden.fullName
      )

      // cache the namer used for entering the default getter symbols
      var ownerNamer: Option[Namer] = None
      var moduleNamer: Option[(ClassDef, Namer)] = None
      var posCounter = 1

      // For each value parameter, create the getter method if it has a
      // default argument. previous denotes the parameter lists which
      // are on the left side of the current one. These get added to the
      // default getter. Example:
      //
      //   def foo(a: Int)(b: Int = a)      becomes
      //   foo$default$1(a: Int) = a
      //
      vparamss.foldLeft(Nil: List[List[ValDef]]) { (previous, vparams) =>
        assert(!overrides || vparams.length == baseParamss.head.length, ""+ meth.fullName + ", "+ overridden.fullName)
        var baseParams = if (overrides) baseParamss.head else Nil
        for (vparam <- vparams) {
          val sym = vparam.symbol
          // true if the corresponding parameter of the base class has a default argument
          val baseHasDefault = overrides && baseParams.head.hasDefaultFlag
          if (sym.hasDefaultFlag) {
            // generate a default getter for that argument
            val oflag = if (baseHasDefault) OVERRIDE else 0
            val name = nme.defaultGetterName(meth.name, posCounter)

            // Create trees for the defaultGetter. Uses tools from Unapplies.scala
            var deftParams = tparams map copyUntyped[TypeDef]
            val defvParamss = previous map (_.map(p => {
              // in the default getter, remove the default parameter
              val p1 = atPos(p.pos.focus) { ValDef(p.mods &~ DEFAULTPARAM, p.name, p.tpt.duplicate, EmptyTree) }
              UnTyper.traverse(p1)
              p1
            }))

            val parentNamer = if (isConstr) {
              val (cdef, nmr) = moduleNamer.getOrElse {
                val module = companionModuleOf(clazz, context)
                module.initialize // call type completer (typedTemplate), adds the
                                  // module's templateNamer to classAndNamerOfModule
                classAndNamerOfModule get module match {
                  case s @ Some((cdef, nmr)) if nmr != null =>
                    moduleNamer = s
                    (cdef, nmr)
                  case _ =>
                    return // fix #3649 (prevent crash in erroneous source code)
                           // nmr == null can happen in IDE; this is really an ugly hack on top[ of an ugly hack but it seems to work
                }
              }
              deftParams = cdef.tparams map copyUntypedInvariant
              nmr
            }
            else ownerNamer getOrElse {
              val ctx = context.nextEnclosing(c => c.scope.toList.contains(meth))
              assert(ctx != NoContext, meth)
              val nmr = newNamer(ctx)
              ownerNamer = Some(nmr)
              nmr
            }

            // If the parameter type mentions any type parameter of the method, let the compiler infer the
            // return type of the default getter => allow "def foo[T](x: T = 1)" to compile.
            // This is better than always using Wildcard for inferring the result type, for example in
            //    def f(i: Int, m: Int => Int = identity _) = m(i)
            // if we use Wildcard as expected, we get "Nothing => Nothing", and the default is not usable.
            val names = deftParams map { case TypeDef(_, name, _, _) => name }
            val subst = new TypeTreeSubstituter(names contains _)

            val defTpt = subst(copyUntyped(vparam.tpt match {
              // default getter for by-name params
              case AppliedTypeTree(_, List(arg)) if sym.hasFlag(BYNAMEPARAM) => arg
              case t => t
            }))
            val defRhs = copyUntyped(vparam.rhs)

            val defaultTree = atPos(vparam.pos.focus) {
              DefDef(
                Modifiers(meth.flags & DefaultGetterFlags) | SYNTHETIC | DEFAULTPARAM | oflag,
                name, deftParams, defvParamss, defTpt, defRhs)
            }
            if (!isConstr)
              clazz.resetFlag(INTERFACE) // there's a concrete member now
            val default = parentNamer.enterSyntheticSym(defaultTree)
            if (forInteractive && default.owner.isTerm) {
              // enter into map from method symbols to default arguments.
              // if compiling the same local block several times (which can happen in interactive mode)
              // we might otherwise not find the default symbol, because the second time it the
              // method symbol will be re-entered in the scope but the default parameter will not.
              defaultParametersOfMethod(meth) += default
            }
          } else if (baseHasDefault) {
            // the parameter does not have a default itself, but the
            // corresponding parameter in the base class does.
            sym.setFlag(DEFAULTPARAM)
          }
          posCounter += 1
          if (overrides) baseParams = baseParams.tail
        }
        if (overrides) baseParamss = baseParamss.tail
        previous ::: List(vparams)
      }
    }

    //@M! an abstract type definition (abstract type member/type parameter)
    // may take type parameters, which are in scope in its bounds
    private def typeDefSig(tpsym: Symbol, tparams: List[TypeDef], rhs: Tree) = {
      // log("typeDefSig(" + tpsym + ", " + tparams + ")")
      val tparamSyms = typer.reenterTypeParams(tparams) //@M make tparams available in scope (just for this abstypedef)
      val tp = typer.typedType(rhs).tpe match {
        case TypeBounds(lt, rt) if (lt.isError || rt.isError) =>
          TypeBounds.empty
        case tp @ TypeBounds(lt, rt) if (tpsym hasFlag JAVA) =>
          TypeBounds(lt, objToAny(rt))
        case tp =>
          tp
      }
      // see neg/bug1275, #3419
      // used to do a rudimentary kind check here to ensure overriding in refinements
      // doesn't change a type member's arity (number of type parameters), e.g.
      //
      //    trait T { type X[A] }
      //    type S = T { type X }
      //    val x: S
      //
      // X in x.X[A] will get rebound to the X in the refinement, which
      // does not take any type parameters. This mismatch does not crash
      // the compiler (anymore), but leads to weird type errors, as
      // x.X[A] will become NoType internally. It's not obvious the
      // error refers to the X in the refinement and not the original X.
      //
      // However, separate compilation requires the symbol info to be
      // loaded to do this check, but loading the info will probably
      // lead to spurious cyclic errors.  So omit the check.
      polyType(tparamSyms, tp)
    }

    /** Given a case class
     *   case class C[Ts] (ps: Us)
     *  Add the following methods to toScope:
     *  1. if case class is not abstract, add
     *   <synthetic> <case> def apply[Ts](ps: Us): C[Ts] = new C[Ts](ps)
     *  2. add a method
     *   <synthetic> <case> def unapply[Ts](x: C[Ts]) = <ret-val>
     *  where <ret-val> is the caseClassUnapplyReturnValue of class C (see UnApplies.scala)
     *
     * @param cdef is the class definition of the case class
     * @param namer is the namer of the module class (the comp. obj)
     */
    def addApplyUnapply(cdef: ClassDef, namer: Namer) {
      if (!cdef.symbol.hasAbstractFlag)
        namer.enterSyntheticSym(caseModuleApplyMeth(cdef))

      namer.enterSyntheticSym(caseModuleUnapplyMeth(cdef))
    }

    def addCopyMethod(cdef: ClassDef, namer: Namer) {
      caseClassCopyMeth(cdef) foreach namer.enterSyntheticSym
    }

    def typeSig(tree: Tree): Type = {
      // log("typeSig " + tree)
      /** For definitions, transform Annotation trees to AnnotationInfos, assign
       *  them to the sym's annotations. Type annotations: see Typer.typedAnnotated
       *  We have to parse definition annotations here (not in the typer when traversing
       *  the MemberDef tree): the typer looks at annotations of certain symbols; if
       *  they were added only in typer, depending on the compilation order, they may
       *  or may not be visible.
       */
      def annotate(annotated: Symbol) = {
        // typeSig might be called multiple times, e.g. on a ValDef: val, getter, setter
        // parse the annotations only once.
        if (!annotated.isInitialized) tree match {
          case defn: MemberDef =>
            val ainfos = defn.mods.annotations filter { _ != null } map { ann =>
              // need to be lazy, #1782
              LazyAnnotationInfo(() => typer.typedAnnotation(ann))
            }
            if (!ainfos.isEmpty)
              annotated.setRawAnnotations(ainfos)
            if (annotated.isTypeSkolem)
              annotated.deSkolemize.setRawAnnotations(ainfos)
          case _ =>
        }
      }

      val sym: Symbol = tree.symbol
      // @Lukas: I am not sure this is the right way to do things.
      // We used to only decorate the module class with annotations, which is
      // clearly wrong. Now we decorate both the class and the object.
      // But maybe some annotations are only meant for one of these but not for the other?
      annotate(sym)
      if (sym.isModule) annotate(sym.moduleClass)

      def getSig = tree match {
        case ClassDef(_, _, tparams, impl) =>
          newNamer(context.makeNewScope(tree, sym)).classSig(tparams, impl)

        case ModuleDef(_, _, impl) =>
          val clazz = sym.moduleClass
          val namer = newNamer(context.makeNewScope(tree, clazz))
          val tp    = namer templateSig impl
          clazz setInfo tp
          //clazz.typeOfThis = singleType(sym.owner.thisType, sym);
          clazz.tpe

        case DefDef(mods, _, tparams, vparamss, tpt, rhs) =>
          newNamer(context.makeNewScope(tree, sym)).methodSig(mods, tparams, vparamss, tpt, rhs)

        case vdef @ ValDef(mods, name, tpt, rhs) =>
          val typer1 = typer.constrTyperIf(sym.hasFlag(PARAM | PRESUPER) && !mods.isJavaDefined && sym.owner.isConstructor)
          if (tpt.isEmpty) {
            if (rhs.isEmpty) {
              context.error(tpt.pos, "missing parameter type");
              ErrorType
            } else {
              tpt defineType widenIfNecessary(
                sym,
                newTyper(typer1.context.make(vdef, sym)).computeType(rhs, WildcardType),
                WildcardType)
              tpt setPos vdef.pos.focus
              tpt.tpe
            }
          } else typer1.typedType(tpt).tpe

        case TypeDef(_, _, tparams, rhs) =>
          newNamer(context.makeNewScope(tree, sym)).typeDefSig(sym, tparams, rhs) //@M!

        case Import(expr, selectors) =>
          val expr1 = typer.typedQualifier(expr)
          typer checkStable expr1
          if (expr1.symbol != null && expr1.symbol.isRootPackage)
            context.error(tree.pos, "_root_ cannot be imported")

          val newImport = treeCopy.Import(tree, expr1, selectors).asInstanceOf[Import]
          checkSelectors(newImport)
          transformed(tree) = newImport
          // copy symbol and type attributes back into old expression
          // so that the structure builder will find it.
          expr.symbol = expr1.symbol
          expr.tpe = expr1.tpe
          ImportType(expr1)
      }

      val result =
        try getSig
        catch typeErrorHandler(tree.pos, ErrorType)

      result match {
        case PolyType(tparams @ (tp :: _), _) if tp.owner.isTerm =>
          new DeSkolemizeMap(tparams) mapOver result
        case _ =>
          result
      }
    }

    class LogTransitions[S](onEnter: S => String, onExit: S => String) {
      val enabled = settings.debug.value
      @inline final def apply[T](entity: S)(body: => T): T = {
        if (enabled) log(onEnter(entity))
        try body
        finally if (enabled) log(onExit(entity))
      }
    }
    private val logDefinition = new LogTransitions[Symbol](
      sym => "[define] >> " + sym.defaultFlagString + " " + sym.fullLocationString,
      sym => "[define] << " + sym
    )
    private def logAndValidate(sym: Symbol)(body: => Unit) {
      logDefinition(sym)(body)
      validate(sym)
    }

    /** Convert Java generic array type T[] to (T with Object)[]
     *  (this is necessary because such arrays have a representation which is incompatible
     *   with arrays of primitive types.)
     */
    private object RestrictJavaArraysMap extends TypeMap {
      def apply(tp: Type): Type = tp match {
        case TypeRef(pre, ArrayClass, List(elemtp))
        if elemtp.typeSymbol.isAbstractType && !(elemtp <:< ObjectClass.tpe) =>
          TypeRef(pre, ArrayClass, List(intersectionType(List(elemtp, ObjectClass.tpe))))
        case _ =>
          mapOver(tp)
      }
    }

    /** Check that symbol's definition is well-formed. This means:
     *   - no conflicting modifiers
     *   - `abstract` modifier only for classes
     *   - `override` modifier never for classes
     *   - `def` modifier never for parameters of case classes
     *   - declarations only in mixins or abstract classes (when not @native)
     */
    def validate(sym: Symbol) {
      def checkNoConflict(flag1: Int, flag2: Int) {
        if (sym.hasFlag(flag1) && sym.hasFlag(flag2))
          context.error(sym.pos,
            if (flag1 == DEFERRED)
              "abstract member may not have " + flagsToString(flag2) + " modifier";
            else
              "illegal combination of modifiers: " +
              flagsToString(flag1) + " and " + flagsToString(flag2) +
              " for: " + sym);
      }

      if (sym.isConstructor) {
        if (sym.isAnyOverride)
          context.error(sym.pos, "`override' modifier not allowed for constructors")
        if (sym.isImplicit)
          context.error(sym.pos, "`implicit' modifier not allowed for constructors")
      }

      if (sym.hasFlag(IMPLICIT) && !sym.isTerm)
        context.error(sym.pos, "`implicit' modifier can be used only for values, variables and methods")
      if (sym.hasFlag(IMPLICIT) && sym.owner.isPackageClass)
        context.error(sym.pos, "`implicit' modifier cannot be used for top-level objects")
      if (sym.hasFlag(SEALED) && !sym.isClass)
        context.error(sym.pos, "`sealed' modifier can be used only for classes")
      if (sym.hasFlag(ABSTRACT) && !sym.isClass)
        context.error(sym.pos, "`abstract' modifier can be used only for classes; " +
          "\nit should be omitted for abstract members")
      if (sym.isAnyOverride && !sym.hasFlag(TRAIT) && sym.isClass)
        context.error(sym.pos, "`override' modifier not allowed for classes")
      if (sym.isAbstractOverride && !sym.owner.isTrait)
        context.error(sym.pos, "`abstract override' modifier only allowed for members of traits")
      if (sym.isLazy && sym.hasFlag(PRESUPER))
        context.error(sym.pos, "`lazy' definitions may not be initialized early")
      if (sym.info.typeSymbol == FunctionClass(0) &&
          sym.isValueParameter && sym.owner.isCaseClass)
        context.error(sym.pos, "pass-by-name arguments not allowed for case class parameters")
      if (sym hasFlag DEFERRED) { // virtual classes count, too
        if (sym.hasAnnotation(NativeAttr))
          sym.resetFlag(DEFERRED)
        else if (!sym.isValueParameter && !sym.isTypeParameterOrSkolem &&
          !context.tree.isInstanceOf[ExistentialTypeTree] &&
          (!sym.owner.isClass || sym.owner.isModuleClass || sym.owner.isAnonymousClass)) {
            context.error(sym.pos,
              "only classes can have declared but undefined members" + abstractVarMessage(sym))
            sym.resetFlag(DEFERRED)
        }
      }

      checkNoConflict(DEFERRED, PRIVATE)
      checkNoConflict(FINAL, SEALED)
      checkNoConflict(PRIVATE, PROTECTED)
      // checkNoConflict(PRIVATE, OVERRIDE) // this one leads to bad error messages like #4174, so catch in refchecks
      // checkNoConflict(PRIVATE, FINAL)    // can't do this because FINAL also means compile-time constant
      checkNoConflict(ABSTRACT, FINAL)
      checkNoConflict(DEFERRED, FINAL)

      // @PP: I added this as a sanity check because these flags are supposed to be
      // converted to ABSOVERRIDE before arriving here.
      checkNoConflict(ABSTRACT, OVERRIDE)
    }
  }

  abstract class TypeCompleter extends LazyType {
    val tree: Tree
  }

  def mkTypeCompleter(t: Tree)(c: Symbol => Unit) = new LockingTypeCompleter {
    val tree = t
    def completeImpl(sym: Symbol) = c(sym)
  }

  trait LockingTypeCompleter extends TypeCompleter {
    def completeImpl(sym: Symbol): Unit

    override def complete(sym: Symbol) = {
      _lockedCount += 1
      try completeImpl(sym)
      finally _lockedCount -= 1
    }
  }

  /** A class representing a lazy type with known type parameters.
   */
  class PolyTypeCompleter(tparams: List[TypeDef], restp: TypeCompleter, owner: Tree, ctx: Context) extends LockingTypeCompleter {
    private val ownerSym    = owner.symbol
    override val typeParams = tparams map (_.symbol) //@M
    override val tree       = restp.tree
    if (ownerSym.isTerm)
      typer skolemizeTypeParams tparams

    def completeImpl(sym: Symbol) = {
      // @M an abstract type's type parameters are entered.
      // TODO: change to isTypeMember ?
      if (ownerSym.isAbstractType)
        newNamer(ctx.makeNewScope(owner, ownerSym)) enterSyms tparams //@M
      restp complete sym
    }
  }

  // Can we relax these restrictions? For motivation, see
  //    test/files/pos/depmet_implicit_oopsla_session_2.scala
  //    neg/depmet_try_implicit.scala
  //
  // We should allow forward references since type selections on
  // implicit args are like type parameters.
  //    def foo[T](a: T, x: w.T2)(implicit w: ComputeT2[T])
  // is more compact than:
  //    def foo[T, T2](a: T, x: T2)(implicit w: ComputeT2[T, T2])
  // moreover, the latter is not an encoding of the former, which hides type
  // inference of T2, so you can specify T while T2 is purely computed
  private class DependentTypeChecker(ctx: Context) extends TypeTraverser {
    private[this] val okParams = mutable.Set[Symbol]()
    private[this] val method   = ctx.owner

    def traverse(tp: Type) = tp match {
      case SingleType(_, sym) =>
        if (sym.owner == method && sym.isValueParameter && !okParams(sym))
          ctx.error(sym.pos, "illegal dependent method type" + errorAddendum)

      case _ => mapOver(tp)
    }
    def check(vparamss: List[List[Symbol]]) {
      for (vps <- vparamss) {
        for (p <- vps)
          this(p.info)
        // can only refer to symbols in earlier parameter sections
        // (if the extension is enabled)
        okParams ++= vps
      }
    }
    private def errorAddendum =
      ": parameter appears in the type of another parameter in the same section or an earlier one"
  }

  @deprecated("Use underlyingSymbol instead", "2.10.0")
  def underlying(member: Symbol): Symbol = underlyingSymbol(member)
  @deprecated("Use `companionSymbolOf` instead", "2.10.0")
  def companionClassOf(module: Symbol, ctx: Context): Symbol = companionSymbolOf(module, ctx)
  @deprecated("Use `companionSymbolOf` instead", "2.10.0")
  def companionModuleOf(clazz: Symbol, ctx: Context): Symbol = companionSymbolOf(clazz, ctx)

  /** The companion class or companion module of `original`.
   *  Calling .companionModule does not work for classes defined inside methods.
   *
   *  !!! Then why don't we fix companionModule? Does the presence of these
   *  methods imply all the places in the compiler calling sym.companionModule are
   *  bugs waiting to be reported? If not, why not? When exactly do we need to
   *  call this method?
   */
  def companionSymbolOf(original: Symbol, ctx: Context): Symbol = {
    try original.companionSymbol match {
      case NoSymbol =>
        ctx.lookup(original.name.companionName, original.owner).suchThat(sym =>
          (original.isTerm || sym.hasModuleFlag) &&
          (sym isCoDefinedWith original)
        )
      case sym => sym
    }
    catch {
      case e: InvalidCompanions =>
        ctx.error(original.pos, e.getMessage)
        NoSymbol
    }
  }
}
