/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.annotation.tailrec
import scala.ref.WeakReference
import symtab.Flags._
import scala.tools.nsc.io.AbstractFile

/** This trait declares methods to create symbols and to enter them into scopes.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Namers extends MethodSynthesis {
  self: Analyzer =>

  import global._
  import definitions._

  private var _lockedCount = 0
  def lockedCount = this._lockedCount

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
  def newNamerFor(context: Context, tree: Tree): Namer =
    newNamer(context.makeNewScope(tree, tree.symbol))

  // In the typeCompleter (templateSig) of a case class (resp it's module),
  // synthetic `copy` (reps `apply`, `unapply`) methods are added. To compute
  // their signatures, the corresponding ClassDef is needed.
  // During naming, for each case class module symbol, the corresponding ClassDef
  // is stored in this map. The map is cleared lazily, i.e. when the new symbol
  // is created with the same name, the old one (if present) is wiped out, or the
  // entry is deleted when it is used and no longer needed.
  private val classOfModuleClass = perRunCaches.newWeakMap[Symbol, WeakReference[ClassDef]]()

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

  abstract class Namer(val context: Context) extends MethodSynth with NamerContextErrors { thisNamer =>

    import NamerErrorGen._
    val typer = newTyper(context)

    private lazy val innerNamer =
      if (isTemplateContext(context)) createInnerNamer() else this

    def createNamer(tree: Tree): Namer = {
      val sym = tree match {
        case ModuleDef(_, _, _) => tree.symbol.moduleClass
        case _                  => tree.symbol
      }
      newNamer(context.makeNewScope(tree, sym))
    }
    def createInnerNamer() = {
      newNamer(context.make(context.tree, owner, newScope))
    }
    def createPrimaryConstructorParameterNamer: Namer = { //todo: can we merge this with SCCmode?
      val classContext = context.enclClass
      val outerContext = classContext.outer.outer
      val paramContext = outerContext.makeNewScope(outerContext.tree, outerContext.owner)

      owner.unsafeTypeParams foreach (paramContext.scope enter _)
      newNamer(paramContext)
    }

    def enclosingNamerWithScope(scope: Scope) = {
      var cx = context
      while (cx != NoContext && cx.scope != scope) cx = cx.outer
      if (cx == NoContext || cx == context) thisNamer
      else newNamer(cx)
    }

    def enterValueParams(vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      mmap(vparamss) { param =>
        val sym = assignSymbol(param, param.name, mask = ValueParameterFlags)
        setPrivateWithin(param, sym)
        enterInScope(sym)
        sym setInfo monoTypeCompleter(param)
      }
    }

    protected def owner       = context.owner
    private def contextFile = context.unit.source.file
    private def typeErrorHandler[T](tree: Tree, alt: T): PartialFunction[Throwable, T] = {
      case ex: TypeError =>
        // H@ need to ensure that we handle only cyclic references
        TypeSigError(tree, ex)
        alt
    }
    // PRIVATE | LOCAL are fields generated for primary constructor arguments
    // @PP: ...or fields declared as private[this].  PARAMACCESSOR marks constructor arguments.
    // Neither gets accessors so the code is as far as I know still correct.
    def noEnterGetterSetter(vd: ValDef) = !vd.mods.isLazy && (
         !owner.isClass
      || (vd.mods.isPrivateLocal && !vd.mods.isCaseAccessor)
      || (vd.name startsWith nme.OUTER)
      || (context.unit.isJava)
    )
    def noFinishGetterSetter(vd: ValDef) = (
         vd.mods.isPrivateLocal
      || vd.symbol.isModuleVar
      || vd.symbol.isLazy
    )

    def setPrivateWithin[T <: Symbol](tree: Tree, sym: T, mods: Modifiers): T =
      if (sym.isPrivateLocal || !mods.hasAccessBoundary) sym
      else sym setPrivateWithin typer.qualifyingClass(tree, mods.privateWithin, packageOK = true)

    def setPrivateWithin(tree: MemberDef, sym: Symbol): Symbol =
      setPrivateWithin(tree, sym, tree.mods)

    def inConstructorFlag: Long =
      if (owner.isConstructor && !context.inConstructorSuffix || owner.isEarlyInitialized) INCONSTRUCTOR
      else 0l

    def moduleClassFlags(moduleFlags: Long) =
      (moduleFlags & ModuleToClassFlags) | inConstructorFlag

    def updatePosFlags(sym: Symbol, pos: Position, flags: Long): Symbol = {
      debuglog("[overwrite] " + sym)
      val newFlags = (sym.flags & LOCKED) | flags
      sym reset NoType setFlag newFlags setPos pos
      sym.moduleClass andAlso (updatePosFlags(_, pos, moduleClassFlags(flags)))

      if (sym.owner.isPackageClass) {
        companionSymbolOf(sym, context) andAlso { companion =>
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
    def namerOf(sym: Symbol): Namer = {
      val usePrimary = sym.isTerm && (
           (sym.isParamAccessor)
        || (sym.isParameter && sym.owner.isPrimaryConstructor)
      )

      if (usePrimary) createPrimaryConstructorParameterNamer
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
          DoubleDefError(sym, prev.sym)
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
        case NoSymbol => try dispatch() catch typeErrorHandler(tree, this.context)
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
    def assignAndEnterFinishedSymbol(tree: MemberDef): Symbol = {
      val sym = assignAndEnterSymbol(tree)
      sym setInfo completerOf(tree)
      // log("[+info] " + sym.fullLocationString)
      sym
    }

    private def logAssignSymbol(tree: Tree, sym: Symbol): Symbol = {
      sym.name.toTermName match {
        case nme.IMPORT | nme.OUTER | nme.ANON_CLASS_NAME | nme.ANON_FUN_NAME | nme.CONSTRUCTOR => ()
        case _                                                                                  =>
          log("[+symbol] " + sym.debugLocationString)
      }
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
      val flags       = tree.mods.flags & mask

      tree match {
        case TypeDef(_, _, _, _) if isParameter     => owner.newTypeParameter(name.toTypeName, pos, flags)
        case TypeDef(_, _, _, _)                    => owner.newTypeSymbol(name.toTypeName, pos, flags)
        case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => owner.newConstructor(pos, flags)
        case DefDef(_, _, _, _, _, _)               => owner.newMethod(name.toTermName, pos, flags)
        case ClassDef(_, _, _, _)                   => owner.newClassSymbol(name.toTypeName, pos, flags)
        case ModuleDef(_, _, _)                     => owner.newModule(name, pos, flags)
        case PackageDef(pid, _)                     => createPackageSymbol(pos, pid)
        case ValDef(_, _, _, _)                     =>
          if (isParameter) owner.newValueParameter(name, pos, flags)
          else owner.newValue(name, pos, flags)
      }
    }
    private def createFieldSymbol(tree: ValDef): TermSymbol =
      owner.newValue(nme.getterToLocal(tree.name), tree.pos, tree.mods.flags & FieldFlags | PrivateLocal)

    private def createImportSymbol(tree: Tree) =
      NoSymbol.newImport(tree.pos) setInfo completerOf(tree)

    /** All PackageClassInfoTypes come from here. */
    private def createPackageSymbol(pos: Position, pid: RefTree): Symbol = {
      val pkgOwner = pid match {
        case Ident(_)                 => if (owner.isEmptyPackageClass) rootMirror.RootClass else owner
        case Select(qual: RefTree, _) => createPackageSymbol(pos, qual).moduleClass
      }
      val existing = pkgOwner.info.decls.lookup(pid.name)

      if (existing.isPackage && pkgOwner == existing.owner)
        existing
      else {
        val pkg          = pkgOwner.newPackage(pid.name.toTermName, pos)
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
        m.moduleClass andAlso (setPrivateWithin(tree, _))
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
    def ensureCompanionObject(cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol = {
      val m = companionSymbolOf(cdef.symbol, context)
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
      //
      // @martin: currentRun.compiles is needed because we might have a stale
      // companion object from another run in scope. In that case we should still
      // overwrite the object. I.e.
      // Compile run #1: object Foo { ... }
      // Compile run #2: case class Foo ...
      // The object Foo is still in scope, but because it is not compiled in current run
      // it should be ditched and a new one created.
      if (m != NoSymbol && currentRun.compiles(m)) m
      else enterSyntheticSym(atPos(cdef.pos.focus)(creator(cdef)))
    }

    private def checkSelectors(tree: Import): Unit = {
      import DuplicatesErrorKinds._
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

            defSym andAlso (typer.permanentlyHiddenWarning(pos, to0, _))
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
            // for Java code importing Scala objects
            if (!nme.isModuleName(from) || isValid(nme.stripModuleSuffix(from))) {
              typer.TyperErrorGen.NotAMemberError(tree, expr, from)
              typer.infer.setError(tree)
            }
          }
          // Setting the position at the import means that if there is
          // more than one hidden name, the second will not be warned.
          // So it is the position of the actual hidden name.
          checkNotRedundant(tree.pos withPoint fromPos, from, to)
        }
      }

      def noDuplicates(names: List[Name], check: DuplicatesErrorKinds.Value) {
        def loop(xs: List[Name]): Unit = xs match {
          case Nil      => ()
          case hd :: tl =>
            if (hd == nme.WILDCARD || !(tl contains hd)) loop(tl)
            else DuplicatesError(tree, hd, check)
        }
        loop(names filterNot (x => x == null || x == nme.WILDCARD))
      }
      selectors foreach checkSelector

      // checks on the whole set
      noDuplicates(selectors map (_.name), RenamedTwice)
      noDuplicates(selectors map (_.rename), AppearsTwice)
    }

    def enterCopyMethodOrGetter(tree: Tree, tparams: List[TypeDef]): Symbol = {
      val sym          = tree.symbol
      val lazyType     = completerOf(tree, tparams)
      def completeCopyFirst = sym.isSynthetic && (!sym.hasDefault || sym.owner.info.member(nme.copy).isSynthetic)
      def completeCopyMethod(clazz: Symbol) {
        // the 'copy' method of case classes needs a special type completer to make
        // bug0054.scala (and others) work. the copy method has to take exactly the same
        // parameter types as the primary constructor.
        val constructorType = clazz.primaryConstructor.tpe
        val subst           = new SubstSymMap(clazz.typeParams, tparams map (_.symbol))
        val vparamss        = tree match { case x: DefDef => x.vparamss ; case _ => Nil }
        val cparamss        = constructorType.paramss

        map2(vparamss, cparamss)((vparams, cparams) =>
          map2(vparams, cparams)((param, cparam) =>
            // need to clone the type cparam.tpe???
            // problem is: we don't have the new owner yet (the new param symbol)
            param.tpt setType subst(cparam.tpe)
          )
        )
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
        //@M! TypeDef's type params are handled differently
        //@M e.g., in [A[x <: B], B], A and B are entered first as both are in scope in the definition of x
        //@M x is only in scope in `A[x <: B]'
        if (!tree.symbol.isAbstractType) { //@M TODO: change to isTypeMember ?
          val scope = tree match {
            case ClassDef(_, _, _, _) =>
              val scope = context.makeNewScope(tree, tree.symbol)
              scope.inSelfSuperCall = true
              scope
            case _ =>
              context.makeNewScope(tree, tree.symbol)
          }
          newNamer(scope) enterSyms tparams
        }

        new PolyTypeCompleter(tparams, mono, tree, context) //@M
      }
    }

    def enterIfNotThere(sym: Symbol) {
      val scope = context.scope
      @tailrec def search(e: ScopeEntry) {
        if ((e eq null) || (e.owner ne scope))
          scope enter sym
        else if (e.sym ne sym)  // otherwise, aborts since we found sym
          search(e.tail)
      }
      search(scope lookupEntry sym.name)
    }

    def enterValDef(tree: ValDef) {
      if (noEnterGetterSetter(tree))
        assignAndEnterFinishedSymbol(tree)
      else
        enterGetterSetter(tree)

      // When java enums are read from bytecode, they are known to have
      // constant types by the jvm flag and assigned accordingly.  When
      // they are read from source, the java parser marks them with the
      // STABLE flag, and now we receive that signal.
      if (tree.symbol hasAllFlags STABLE | JAVA)
        tree.symbol setInfo ConstantType(Constant(tree.symbol))
    }

    def enterLazyVal(tree: ValDef, lazyAccessor: Symbol): TermSymbol = {
      // If the owner is not a class, this is a lazy val from a method,
      // with no associated field.  It has an accessor with $lzy appended to its name and
      // its flags are set differently.  The implicit flag is reset because otherwise
      // a local implicit "lazy val x" will create an ambiguity with itself
      // via "x$lzy" as can be seen in test #3927.
      val sym = (
        if (owner.isClass) createFieldSymbol(tree)
        else owner.newValue(tree.name append nme.LAZY_LOCAL, tree.pos, tree.mods.flags & ~IMPLICIT)
      )
      enterValSymbol(tree, sym setFlag MUTABLE setLazyAccessor lazyAccessor)
    }
    def enterStrictVal(tree: ValDef): TermSymbol = {
      enterValSymbol(tree, createFieldSymbol(tree))
    }
    def enterValSymbol(tree: ValDef, sym: TermSymbol): TermSymbol = {
      enterInScope(sym)
      sym setInfo namerOf(sym).monoTypeCompleter(tree)
    }
    def enterPackage(tree: PackageDef) {
      val sym = assignSymbol(tree)
      newNamer(context.make(tree, sym.moduleClass, sym.info.decls)) enterSyms tree.stats
    }
    def enterTypeDef(tree: TypeDef) = assignAndEnterFinishedSymbol(tree)

    def enterDefDef(tree: DefDef): Unit = tree match {
      case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) =>
        assignAndEnterFinishedSymbol(tree)
      case DefDef(mods, name, tparams, _, _, _) =>
        val bridgeFlag = if (mods hasAnnotationNamed tpnme.bridgeAnnot) BRIDGE else 0
        val sym = assignAndEnterSymbol(tree) setFlag bridgeFlag

        if (name == nme.copy || tree.symbol.name.startsWith(nme.copy + nme.DEFAULT_GETTER_STRING))
          enterCopyMethodOrGetter(tree, tparams)
        else
          sym setInfo completerOf(tree, tparams)
    }

    def enterClassDef(tree: ClassDef) {
      val ClassDef(mods, name, tparams, impl) = tree
      val primaryConstructorArity = treeInfo.firstConstructorArgs(impl.body).size
      tree.symbol = enterClassSymbol(tree)
      tree.symbol setInfo completerOf(tree)

      if (mods.isCase) {
        if (primaryConstructorArity > MaxFunctionArity)
          MaxParametersCaseClassError(tree)

        val m = ensureCompanionObject(tree, caseModuleDef)
        classOfModuleClass(m.moduleClass) = new WeakReference(tree)
      }
      val hasDefault = impl.body exists {
        case DefDef(_, nme.CONSTRUCTOR, _, vparamss, _, _)  => mexists(vparamss)(_.mods.hasDefault)
        case _                                              => false
      }
      if (hasDefault) {
        val m = ensureCompanionObject(tree)
        classAndNamerOfModule(m) = (tree, null)
      }
      val owner = tree.symbol.owner
      if (settings.lint.value && owner.isPackageObjectClass && !mods.isImplicit) {
        context.unit.warning(tree.pos,
          "it is not recommended to define classes/objects inside of package objects.\n" +
          "If possible, define " + tree.symbol + " in " + owner.skipPackageObject + " instead."
        )
      }

      // Suggested location only.
      if (mods.isImplicit) {
        if (primaryConstructorArity == 1) {
          log("enter implicit wrapper "+tree+", owner = "+owner)
          enterImplicitWrapper(tree)
        }
        else context.unit.error(tree.pos, "implicit classes must accept exactly one primary constructor parameter")
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
        if (sym.isLazy)
          sym.lazyAccessor andAlso enterIfNotThere

        defaultParametersOfMethod(sym) foreach { symRef => enterIfNotThere(symRef()) }
      }
      this.context
    }

    def enterSyntheticSym(tree: Tree): Symbol = {
      enterSym(tree)
      context.unit.synthetics(tree.symbol) = tree
      tree.symbol
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
        val tp = initializeLowerBounds(typeSig(tree))
        sym setInfo {
          if (sym.isJavaDefined) RestrictJavaArraysMap(tp)
          else tp
        }
        // this early test is there to avoid infinite baseTypes when
        // adding setters and getters --> bug798
        val needsCycleCheck = (sym.isAliasType || sym.isAbstractType) && !sym.isParameter
        if (needsCycleCheck && !typer.checkNonCyclic(tree.pos, tp))
          sym setInfo ErrorType
      }
      // tree match {
      //   case ClassDef(_, _, _, impl) =>
      //     val parentsOK = (
      //          treeInfo.isInterface(sym, impl.body)
      //       || (sym eq ArrayClass)
      //       || (sym isSubClass AnyValClass)
      //     )
      //     if (!parentsOK)
      //       ensureParent(sym, AnyRefClass)
      //   case _ => ()
      // }
    }

    def moduleClassTypeCompleter(tree: Tree) = {
      mkTypeCompleter(tree) { sym =>
        val moduleSymbol = tree.symbol
        assert(moduleSymbol.moduleClass == sym, moduleSymbol.moduleClass)
        moduleSymbol.info // sets moduleClass info as a side effect.
      }
    }

    def accessorTypeCompleter(tree: ValDef, isSetter: Boolean = false) = mkTypeCompleter(tree) { sym =>
      logAndValidate(sym) {
        sym setInfo {
          if (isSetter)
            MethodType(List(sym.newSyntheticValueParam(typeSig(tree))), UnitClass.tpe)
          else
            NullaryMethodType(typeSig(tree))
        }
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
    /** Computes the type of the body in a ValDef or DefDef, and
     *  assigns the type to the tpt's node.  Returns the type.
     */
    private def assignTypeToTree(tree: ValOrDefDef, defnTyper: Typer, pt: Type): Type = {
      // compute result type from rhs
      val typedBody =
        if (tree.symbol.isTermMacro) defnTyper.computeMacroDefType(tree, pt)
        else defnTyper.computeType(tree.rhs, pt)

      val typedDefn = widenIfNecessary(tree.symbol, typedBody, pt)
      assignTypeToTree(tree, typedDefn)
    }

    private def assignTypeToTree(tree: ValOrDefDef, tpe: Type): Type = {
      tree.tpt defineType tpe setPos tree.pos.focus
      tree.tpt.tpe
    }

    // owner is the class with the self type
    def enterSelf(self: ValDef) {
      val ValDef(_, name, tpt, _) = self
      if (self eq emptyValDef)
        return

      val hasName = name != nme.WILDCARD
      val hasType = !tpt.isEmpty
      if (!hasType)
        tpt defineType NoType

      val sym = (
        if (hasType || hasName) {
          owner.typeOfThis = if (hasType) selfTypeCompleter(tpt) else owner.tpe
          val selfSym = owner.thisSym setPos self.pos
          if (hasName) selfSym setName name else selfSym
        }
        else {
          val symName = if (name != nme.WILDCARD) name else nme.this_
          owner.newThisSym(symName, owner.pos) setInfo owner.tpe
        }
      )
      self.symbol = context.scope enter sym
    }

    private def templateSig(templ: Template): Type = {
      val clazz = context.owner
      def checkParent(tpt: Tree): Type = {
        val tp = tpt.tpe
        val inheritsSelf = tp.typeSymbol == owner
        if (inheritsSelf)
          InheritsItselfError(tpt)

        if (inheritsSelf || tp.isError) AnyRefClass.tpe
        else tp
      }

      val parents = typer.parentTypes(templ) map checkParent

      enterSelf(templ.self)

      val decls = newScope
      val templateNamer = newNamer(context.make(templ, clazz, decls))
      templateNamer enterSyms templ.body

      // add apply and unapply methods to companion objects of case classes,
      // unless they exist already; here, "clazz" is the module class
      if (clazz.isModuleClass) {
        Namers.this.classOfModuleClass get clazz foreach { cdefRef =>
          val cdef = cdefRef()
          if (cdef.mods.isCase) addApplyUnapply(cdef, templateNamer)
          classOfModuleClass -= clazz
        }
      }

      // add the copy method to case classes; this needs to be done here, not in SyntheticMethods, because
      // the namer phase must traverse this copy method to create default getters for its parameters.
      // here, clazz is the ClassSymbol of the case class (not the module).
      // @check: this seems to work only if the type completer of the class runs before the one of the
      // module class: the one from the module class removes the entry from classOfModuleClass (see above).
      if (clazz.isClass && !clazz.hasModuleFlag) {
        val modClass = companionSymbolOf(clazz, context).moduleClass
        Namers.this.classOfModuleClass get modClass map { cdefRef =>
          val cdef = cdefRef()

          def hasCopy(decls: Scope) = (decls lookup nme.copy) != NoSymbol
          if (cdef.mods.isCase && !hasCopy(decls) &&
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
      ClassInfoType(parents, decls, clazz)
    }

    private def classSig(tparams: List[TypeDef], impl: Template): Type = {
      val tparams0   = typer.reenterTypeParams(tparams)
      val resultType = templateSig(impl)

      GenPolyType(tparams0, resultType)
    }

    private def methodSig(ddef: DefDef, mods: Modifiers, tparams: List[TypeDef],
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
      var resultPt = if (tpt.isEmpty) WildcardType else typer.typedType(tpt).tpe
      val site = clazz.thisType

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
        if (owner.isJavaDefined)
          // TODODEPMET necessary?? new dependent types: replace symbols in restpe with the ones in vparams
          JavaMethodType(vparams map (p => p setInfo objToAny(p.tpe)), restpe)
        else
          MethodType(vparams, restpe)
      }

      def thisMethodType(restpe: Type) = {
        val checkDependencies = new DependentTypeChecker(context)(this)
        checkDependencies check vparamSymss
        // DEPMETTODO: check not needed when they become on by default
        checkDependencies(restpe)

        GenPolyType(
          tparamSyms, // deSkolemized symbols  -- TODO: check that their infos don't refer to method args?
          if (vparamSymss.isEmpty) NullaryMethodType(restpe)
          // vparamss refer (if they do) to skolemized tparams
          else (vparamSymss :\ restpe) (makeMethodType)
        )
      }

      def transformedResult =
        thisMethodType(resultPt).substSym(tparams map (_.symbol), tparamSyms)

      // luc: added .substSym from skolemized to deSkolemized
      // site.memberType(sym): PolyType(tparams, MethodType(..., ...))
      //    ==> all references to tparams are deSkolemized
      // thisMethodType: tparams in PolyType are deSkolemized, the references in the MethodTypes are skolemized.
      //    ==> the two didn't match
      //
      // for instance, B.foo would not override A.foo, and the default on parameter b would not be inherited
      //   class A { def foo[T](a: T)(b: T = a) = a }
      //   class B extends A { override def foo[U](a: U)(b: U) = b }
      def overriddenSymbol =
        intersectionType(clazz.info.parents).nonPrivateMember(meth.name).filter { sym =>
          sym != NoSymbol && (site.memberType(sym) matches transformedResult)
        }
      // TODO: see whether this or something similar would work instead.
      //
      // def overriddenSymbol = meth.nextOverriddenSymbol

      // fill in result type and parameter types from overridden symbol if there is a unique one.
      if (clazz.isClass && (tpt.isEmpty || mexists(vparamss)(_.tpt.isEmpty))) {
        // try to complete from matching definition in base type
        mforeach(vparamss)(v => if (v.tpt.isEmpty) v.symbol setInfo WildcardType)
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
                vparam.tpt defineType paramtpe setPos vparam.pos.focus
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
      mforeach(vparamss) { vparam =>
        if (vparam.tpt.isEmpty) {
          MissingParameterOrValTypeError(vparam)
          vparam.tpt defineType ErrorType
        }
      }
      addDefaultGetters(meth, vparamss, tparams, overriddenSymbol)

      // macro defs need to be typechecked in advance
      // because @macroImpl annotation only gets assigned during typechecking
      // otherwise we might find ourselves in the situation when we specified -Xmacro-fallback-classpath
      // but macros still don't expand
      // that might happen because macro def doesn't have its link a macro impl yet
      if (ddef.symbol.isTermMacro) {
        val pt = resultPt.substSym(tparamSyms, tparams map (_.symbol))
        typer.computeMacroDefType(ddef, pt)
      }

      thisMethodType({
        val rt = (
          if (!tpt.isEmpty) {
            typer.typedType(tpt).tpe
          } else {
            // replace deSkolemized symbols with skolemized ones
            // (for resultPt computed by looking at overridden symbol, right?)
            val pt = resultPt.substSym(tparamSyms, tparams map (_.symbol))
            assignTypeToTree(ddef, typer, pt)
          }
        )
        // #2382: return type of default getters are always @uncheckedVariance
        if (meth.hasDefault)
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
          val baseHasDefault = overrides && baseParams.head.hasDefault
          if (sym.hasDefault) {
            // generate a default getter for that argument
            val oflag = if (baseHasDefault) OVERRIDE else 0
            val name = nme.defaultGetterName(meth.name, posCounter)

            // Create trees for the defaultGetter. Uses tools from Unapplies.scala
            var deftParams = tparams map copyUntyped[TypeDef]
            val defvParamss = mmap(previous) { p =>
              // in the default getter, remove the default parameter
              val p1 = atPos(p.pos.focus) { ValDef(p.mods &~ DEFAULTPARAM, p.name, p.tpt.duplicate, EmptyTree) }
              UnTyper.traverse(p1)
              p1
            }

            val parentNamer = if (isConstr) {
              val (cdef, nmr) = moduleNamer.getOrElse {
                val module = companionSymbolOf(clazz, context)
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
              defaultParametersOfMethod(meth) += new WeakReference(default)
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
        previous :+ vparams
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
      GenPolyType(tparamSyms, tp)
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
            val ainfos = defn.mods.annotations filterNot (_ eq null) map { ann =>
              // need to be lazy, #1782
              AnnotationInfo lazily (typer typedAnnotation ann)
            }
            if (ainfos.nonEmpty) {
              annotated setAnnotations ainfos
              if (annotated.isTypeSkolem)
                annotated.deSkolemize setAnnotations ainfos
            }
          case _ =>
        }
      }

      val sym: Symbol = tree.symbol
      // @Lukas: I am not sure this is the right way to do things.
      // We used to only decorate the module class with annotations, which is
      // clearly wrong. Now we decorate both the class and the object.
      // But maybe some annotations are only meant for one of these but not for the other?
      //
      // TODO: meta-annotations to indicate class vs. object.
      annotate(sym)
      if (sym.isModule) annotate(sym.moduleClass)

      def getSig = tree match {
        case cdef @ ClassDef(_, name, tparams, impl) =>
          val clazz = tree.symbol
          val result = createNamer(tree).classSig(tparams, impl)
          clazz setInfo result
          if (clazz.isDerivedValueClass) {
            log("Ensuring companion for derived value class " + name + " at " + cdef.pos.show)
            clazz setFlag FINAL
            enclosingNamerWithScope(clazz.owner.info.decls).ensureCompanionObject(cdef)
          }
          result

        case ModuleDef(_, _, impl) =>
          val clazz = sym.moduleClass
          clazz setInfo createNamer(tree).templateSig(impl)
          clazz.tpe

        case ddef @ DefDef(mods, _, tparams, vparamss, tpt, rhs) =>
          // TODO: cleanup parameter list
          createNamer(tree).methodSig(ddef, mods, tparams, vparamss, tpt, rhs)

        case vdef @ ValDef(mods, name, tpt, rhs) =>
          val isBeforeSupercall = (
               (sym hasFlag PARAM | PRESUPER)
            && !mods.isJavaDefined
            && sym.owner.isConstructor
          )
          val typer1 = typer.constrTyperIf(isBeforeSupercall)
          if (tpt.isEmpty) {
            if (rhs.isEmpty) {
              MissingParameterOrValTypeError(tpt)
              ErrorType
            }
            else assignTypeToTree(vdef, newTyper(typer1.context.make(vdef, sym)), WildcardType)
          }
          else typer1.typedType(tpt).tpe

        case TypeDef(_, _, tparams, rhs) =>
          createNamer(tree).typeDefSig(sym, tparams, rhs) //@M!

        case Import(expr, selectors) =>
          val expr1 = typer.typedQualifier(expr)
          typer checkStable expr1
          if (expr1.symbol != null && expr1.symbol.isRootPackage)
            RootImportError(tree)

          if (expr1.isErrorTyped)
            ErrorType
          else {
            val newImport = treeCopy.Import(tree, expr1, selectors).asInstanceOf[Import]
            checkSelectors(newImport)
            transformed(tree) = newImport
            // copy symbol and type attributes back into old expression
            // so that the structure builder will find it.
            expr.symbol = expr1.symbol
            expr.tpe = expr1.tpe
            ImportType(expr1)
          }
      }

      val result =
        try getSig
        catch typeErrorHandler(tree, ErrorType)

      result match {
        case PolyType(tparams @ (tp :: _), _) if tp.owner.isTerm => deskolemizeTypeParams(tparams)(result)
        case _                                                   => result
      }
    }

    def includeParent(tpe: Type, parent: Symbol): Type = tpe match {
      case PolyType(tparams, restpe) =>
        PolyType(tparams, includeParent(restpe, parent))
      case ClassInfoType(parents, decls, clazz) =>
        if (parents exists (_.typeSymbol == parent)) tpe
        else ClassInfoType(parents :+ parent.tpe, decls, clazz)
      case _ =>
        tpe
    }

    def ensureParent(clazz: Symbol, parent: Symbol) = {
      val info0 = clazz.info
      val info1 = includeParent(info0, parent)
      if (info0 ne info1) clazz setInfo info1
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
      sym => "[define] >> " + sym.flagString + " " + sym.fullLocationString,
      sym => "[define] << " + sym
    )
    private def logAndValidate(sym: Symbol)(body: => Unit) {
      logDefinition(sym)(body)
      validate(sym)
    }

    /** Convert Java generic array type T[] to (T with Object)[]
     *  (this is necessary because such arrays have a representation which is incompatible
     *   with arrays of primitive types.)
     *
     *  @note the comparison to Object only works for abstract types bounded by classes that are strict subclasses of Object
     *  if the bound is exactly Object, it will have been converted to Any, and the comparison will fail
     *
     *  see also sigToType
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
      import SymValidateErrors._
      def fail(kind: SymValidateErrors.Value) = SymbolValidationError(sym, kind)

      def checkWithDeferred(flag: Int) {
        if (sym hasFlag flag)
          AbstractMemberWithModiferError(sym, flag)
      }
      def checkNoConflict(flag1: Int, flag2: Int) {
        if (sym hasAllFlags flag1 | flag2)
          IllegalModifierCombination(sym, flag1, flag2)
      }
      if (sym.isImplicit) {
        if (sym.isConstructor)
          fail(ImplicitConstr)
        if (!(sym.isTerm || (sym.isClass && !sym.isTrait)))
          fail(ImplicitNotTermOrClass)
        if (sym.owner.isPackageClass)
          fail(ImplicitAtToplevel)
      }
      if (sym.isClass) {
        if (sym.isAnyOverride && !sym.hasFlag(TRAIT))
          fail(OverrideClass)
      } else {
        if (sym.isSealed)
          fail(SealedNonClass)
        if (sym.hasFlag(ABSTRACT))
          fail(AbstractNonClass)
      }

      if (sym.isConstructor && sym.isAnyOverride)
        fail(OverrideConstr)
      if (sym.isAbstractOverride && !sym.owner.isTrait)
        fail(AbstractOverride)
      if (sym.isLazy && sym.hasFlag(PRESUPER))
        fail(LazyAndEarlyInit)
      if (sym.info.typeSymbol == FunctionClass(0) && sym.isValueParameter && sym.owner.isCaseClass)
        fail(ByNameParameter)
      if (sym.isTrait && sym.isFinal && !sym.isSubClass(AnyValClass))
        checkNoConflict(ABSTRACT, FINAL)

      if (sym.isDeferred) {
        // Is this symbol type always allowed the deferred flag?
        def symbolAllowsDeferred = (
             sym.isValueParameter
          || sym.isTypeParameterOrSkolem
          || context.tree.isInstanceOf[ExistentialTypeTree]
        )
        // Does the symbol owner require no undefined members?
        def ownerRequiresConcrete = (
             !sym.owner.isClass
          ||  sym.owner.isModuleClass
          ||  sym.owner.isAnonymousClass
        )
        if (sym hasAnnotation NativeAttr)
          sym resetFlag DEFERRED
        else if (!symbolAllowsDeferred && ownerRequiresConcrete)
          fail(AbstractVar)

        checkWithDeferred(PRIVATE)
        checkWithDeferred(FINAL)
      }

      checkNoConflict(FINAL, SEALED)
      checkNoConflict(PRIVATE, PROTECTED)
      // checkNoConflict(PRIVATE, OVERRIDE) // this one leads to bad error messages like #4174, so catch in refchecks
      // checkNoConflict(PRIVATE, FINAL)    // can't do this because FINAL also means compile-time constant
      // checkNoConflict(ABSTRACT, FINAL)   // this one gives a bad error for non-@inline classes which extend AnyVal
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

    if (ownerSym.isTerm) {
      val skolems = deriveFreshSkolems(tparams map (_.symbol))
      map2(tparams, skolems)(_ setSymbol _)
    }

    def completeImpl(sym: Symbol) = {
      // @M an abstract type's type parameters are entered.
      // TODO: change to isTypeMember ?
      if (ownerSym.isAbstractType)
        newNamerFor(ctx, owner) enterSyms tparams //@M
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
  private class DependentTypeChecker(ctx: Context)(namer: Namer) extends TypeTraverser {
    private[this] val okParams = mutable.Set[Symbol]()
    private[this] val method   = ctx.owner

    def traverse(tp: Type) = tp match {
      case SingleType(_, sym) =>
        if (sym.owner == method && sym.isValueParameter && !okParams(sym))
          namer.NamerErrorGen.IllegalDependentMethTpeError(sym)(ctx)

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
    try {
      original.companionSymbol orElse {
        ctx.lookup(original.name.companionName, original.owner).suchThat(sym =>
          (original.isTerm || sym.hasModuleFlag) &&
          (sym isCoDefinedWith original)
        )
      }
    }
    catch {
      case e: InvalidCompanions =>
        ctx.unit.error(original.pos, e.getMessage)
        NoSymbol
    }
  }
}
