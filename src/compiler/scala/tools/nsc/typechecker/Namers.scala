/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
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
      def isConstrParam(vd: ValDef) = {
        (sym hasFlag PARAM | PRESUPER) &&
        !vd.mods.isJavaDefined &&
        sym.owner.isConstructor
      }
      val ownerCtx = tree match {
        case vd: ValDef if isConstrParam(vd) =>
          context.makeConstructorContext
        case _ =>
          context
      }
      newNamer(ownerCtx.makeNewScope(tree, sym))
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
         (vd.mods.isPrivateLocal && !vd.mods.isLazy) // all lazy vals need accessors, even private[this]
      || vd.symbol.isModuleVar)

    def setPrivateWithin[T <: Symbol](tree: Tree, sym: T, mods: Modifiers): T =
      if (sym.isPrivateLocal || !mods.hasAccessBoundary) sym
      else sym setPrivateWithin typer.qualifyingClass(tree, mods.privateWithin, packageOK = true)

    def setPrivateWithin(tree: MemberDef, sym: Symbol): Symbol =
      setPrivateWithin(tree, sym, tree.mods)

    def inConstructorFlag: Long = {
      val termOwnedContexts: List[Context] = context.enclosingContextChain.takeWhile(_.owner.isTerm)
      val constructorNonSuffix = termOwnedContexts exists (c => c.owner.isConstructor && !c.inConstructorSuffix)
      val earlyInit            = termOwnedContexts exists (_.owner.isEarlyInitialized)
      if (constructorNonSuffix || earlyInit) INCONSTRUCTOR else 0L
    }

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
          if (sym.isSynthetic || prev.sym.isSynthetic) {
            handleSyntheticNameConflict(sym, prev.sym)
            handleSyntheticNameConflict(prev.sym, sym)
          }
          DoubleDefError(sym, prev.sym)
          sym setInfo ErrorType
          scope unlink prev.sym // let them co-exist...
          // FIXME: The comment "let them co-exist" is confusing given that the
          // line it comments unlinks one of them.  What does it intend?
        }
      }
      scope enter sym
    }

    /** Logic to handle name conflicts of synthetically generated symbols
     *  We handle right now: t6227
     */
    def handleSyntheticNameConflict(sym1: Symbol, sym2: Symbol) = {
      if (sym1.isImplicit && sym1.isMethod && sym2.isModule && sym2.companionClass.isCaseClass)
        validate(sym2.companionClass)
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

    /** Given a ClassDef or ModuleDef, verifies there isn't a companion which
     *  has been defined in a separate file.
     */
    private def validateCompanionDefs(tree: ImplDef) {
      val sym = tree.symbol
      if (sym eq NoSymbol) return

      val ctx    = if (context.owner.isPackageObjectClass) context.outer else context
      val module = if (sym.isModule) sym else ctx.scope lookup tree.name.toTermName
      val clazz  = if (sym.isClass) sym else ctx.scope lookup tree.name.toTypeName
      val fails  = (
           module.isModule
        && clazz.isClass
        && !module.isSynthetic
        && !clazz.isSynthetic
        && (clazz.sourceFile ne null)
        && (module.sourceFile ne null)
        && !(module isCoDefinedWith clazz)
        && module.exists
        && clazz.exists
      )
      if (fails) {
        context.unit.error(tree.pos, (
            s"Companions '$clazz' and '$module' must be defined in same file:\n"
          + s"  Found in ${clazz.sourceFile.canonicalPath} and ${module.sourceFile.canonicalPath}")
        )
      }
    }

    def enterModuleDef(tree: ModuleDef) = {
      val sym = enterModuleSymbol(tree)
      sym.moduleClass setInfo namerOf(sym).moduleClassTypeCompleter(tree)
      sym setInfo completerOf(tree)
      validateCompanionDefs(tree)
      sym
    }

    /** Enter a module symbol. The tree parameter can be either
     *  a module definition or a class definition.
     */
    def enterModuleSymbol(tree : ModuleDef): Symbol = {
      var m: Symbol = context.scope lookupAll tree.name find (_.isModule) getOrElse NoSymbol
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
        // for Import trees, enterSym returns a changed context, so we need a new namer
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

    def enterCopyMethod(copyDef: DefDef): Symbol = {
      val sym      = copyDef.symbol
      val lazyType = completerOf(copyDef)

      /** Assign the types of the class parameters to the parameters of the
       *  copy method. See comment in `Unapplies.caseClassCopyMeth` */
      def assignParamTypes() {
        val clazz = sym.owner
        val constructorType = clazz.primaryConstructor.tpe
        val subst = new SubstSymMap(clazz.typeParams, copyDef.tparams map (_.symbol))
        val classParamss = constructorType.paramss

        map2(copyDef.vparamss, classParamss)((copyParams, classParams) =>
          map2(copyParams, classParams)((copyP, classP) =>
            copyP.tpt setType subst(classP.tpe)
          )
        )
      }

      sym setInfo {
        mkTypeCompleter(copyDef) { sym =>
          assignParamTypes()
          lazyType complete sym
        }
      }
    }

    def completerOf(tree: Tree): TypeCompleter = {
      val mono = namerOf(tree.symbol) monoTypeCompleter tree
      val tparams = treeInfo.typeParameters(tree)
      if (tparams.isEmpty) mono
      else {
        /* @M! TypeDef's type params are handled differently, e.g., in `type T[A[x <: B], B]`, A and B are entered
         * first as both are in scope in the definition of x. x is only in scope in `A[x <: B]`.
         * No symbols are created for the abstract type's params at this point, i.e. the following assertion holds:
         *     !tree.symbol.isAbstractType || { tparams.forall(_.symbol == NoSymbol)
         * (tested with the above example, `trait C { type T[A[X <: B], B] }`). See also comment in PolyTypeCompleter.
         */
        if (!tree.symbol.isAbstractType) //@M TODO: change to isTypeMember ?
          createNamer(tree) enterSyms tparams

        new PolyTypeCompleter(tparams, mono, context) //@M
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

        if (name == nme.copy && sym.isSynthetic)
          enterCopyMethod(tree)
        else
          sym setInfo completerOf(tree)
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
        m.moduleClass.updateAttachment(new ClassForCaseCompanionAttachment(tree))
      }
      val hasDefault = impl.body exists {
        case DefDef(_, nme.CONSTRUCTOR, _, vparamss, _, _)  => mexists(vparamss)(_.mods.hasDefault)
        case _                                              => false
      }
      if (hasDefault) {
        val m = ensureCompanionObject(tree)
        m.updateAttachment(new ConstructorDefaultsAttachment(tree, null))
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
      validateCompanionDefs(tree)
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

        for (defAtt <- sym.attachments.get[DefaultsOfLocalMethodAttachment])
          defAtt.defaultGetters foreach enterIfNotThere
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

    def moduleClassTypeCompleter(tree: ModuleDef) = {
      mkTypeCompleter(tree) { sym =>
        val moduleSymbol = tree.symbol
        assert(moduleSymbol.moduleClass == sym, moduleSymbol.moduleClass)
        moduleSymbol.info // sets moduleClass info as a side effect.
      }
    }

    /* Explicit isSetter required for bean setters (beanSetterSym.isSetter is false) */
    def accessorTypeCompleter(tree: ValDef, isSetter: Boolean) = mkTypeCompleter(tree) { sym =>
      logAndValidate(sym) {
        sym setInfo {
          val tp = if (isSetter) MethodType(List(sym.newSyntheticValueParam(typeSig(tree))), UnitClass.tpe)
                   else NullaryMethodType(typeSig(tree))
          pluginsTypeSigAccessor(tp, typer, tree, sym)
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
      val rhsTpe =
        if (tree.symbol.isTermMacro) defnTyper.computeMacroDefType(tree, pt)
        else defnTyper.computeType(tree.rhs, pt)

      val defnTpe = widenIfNecessary(tree.symbol, rhsTpe, pt)
      tree.tpt defineType defnTpe setPos tree.pos.focus
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
        clazz.attachments.get[ClassForCaseCompanionAttachment] foreach { cma =>
          val cdef = cma.caseClass
          assert(cdef.mods.isCase, "expected case class: "+ cdef)
          addApplyUnapply(cdef, templateNamer)
        }
      }

      // add the copy method to case classes; this needs to be done here, not in SyntheticMethods, because
      // the namer phase must traverse this copy method to create default getters for its parameters.
      // here, clazz is the ClassSymbol of the case class (not the module). (!clazz.hasModuleFlag) excludes
      // the moduleClass symbol of the companion object when the companion is a "case object".
      if (clazz.isCaseClass && !clazz.hasModuleFlag) {
        val modClass = companionSymbolOf(clazz, context).moduleClass
        modClass.attachments.get[ClassForCaseCompanionAttachment] foreach { cma =>
          val cdef = cma.caseClass
          def hasCopy(decls: Scope) = (decls lookup nme.copy) != NoSymbol
          // SI-5956 needs (cdef.symbol == clazz): there can be multiple class symbols with the same name
          if (cdef.symbol == clazz && !hasCopy(decls) &&
                  !parents.exists(p => hasCopy(p.typeSymbol.info.decls)) &&
                  !parents.flatMap(_.baseClasses).distinct.exists(bc => hasCopy(bc.info.decls)))
            addCopyMethod(cdef, templateNamer)
        }
      }

      // if default getters (for constructor defaults) need to be added to that module, here's the namer
      // to use. clazz is the ModuleClass. sourceModule works also for classes defined in methods.
      val module = clazz.sourceModule
      for (cda <- module.attachments.get[ConstructorDefaultsAttachment]) {
        debuglog(s"Storing the template namer in the ConstructorDefaultsAttachment of ${module.debugLocationString}.")
        cda.companionModuleClassNamer = templateNamer
      }
      val classTp = ClassInfoType(parents, decls, clazz)
      pluginsTypeSig(classTp, templateNamer.typer, templ, WildcardType)
    }

    private def classSig(cdef: ClassDef): Type = {
      val clazz = cdef.symbol
      val ClassDef(_, _, tparams, impl) = cdef
      val tparams0   = typer.reenterTypeParams(tparams)
      val resultType = templateSig(impl)

      val res = GenPolyType(tparams0, resultType)
      val pluginsTp = pluginsTypeSig(res, typer, cdef, WildcardType)

      // Already assign the type to the class symbol (monoTypeCompleter will do it again).
      // Allows isDerivedValueClass to look at the info.
      clazz setInfo pluginsTp
      if (clazz.isDerivedValueClass) {
        log("Ensuring companion for derived value class " + cdef.name + " at " + cdef.pos.show)
        clazz setFlag FINAL
        // Don't force the owner's info lest we create cycles as in SI-6357.
        enclosingNamerWithScope(clazz.owner.rawInfo.decls).ensureCompanionObject(cdef)
      }
      pluginsTp
    }

    private def moduleSig(mdef: ModuleDef): Type = {
      val moduleSym = mdef.symbol
      // The info of both the module and the moduleClass symbols need to be assigned. monoTypeCompleter assigns
      // the result of typeSig to the module symbol. The module class info is assigned here as a side-effect.
      val result = templateSig(mdef.impl)
      val pluginsTp = pluginsTypeSig(result, typer, mdef, WildcardType)
      // Assign the moduleClass info (templateSig returns a ClassInfoType)
      val clazz = moduleSym.moduleClass
      clazz setInfo pluginsTp
      // clazz.tpe returns a `ModuleTypeRef(clazz)`, a typeRef that links to the module class `clazz`
      // (clazz.info would the ClassInfoType, which is not what should be assigned to the module symbol)
      clazz.tpe
    }

    /**
     * The method type for `ddef`.
     *
     * If a PolyType(tparams, restp) is returned, `tparams` are the external symbols (not type skolems),
     * i.e. instances of AbstractTypeSymbol. All references in `restp` to the type parameters are TypeRefs
     * to these non-skolems.
     *
     * For type-checking the rhs (in case the result type is inferred), the type skolems of the type parameters
     * are entered in scope. Equally, the parameter symbols entered into scope have types which refer to those
     * skolems: when type-checking the rhs, references to parameters need to have types that refer to the skolems.
     * In summary, typing an rhs happens with respect to the skolems.
     *
     * This means that the method's result type computed by the typer refers to skolems. In order to put it
     * into the method type (the result of methodSig), typeRefs to skolems have to be replaced by references
     * to the non-skolems.
     */
    private def methodSig(ddef: DefDef): Type = {

      // DEPMETTODO: do we need to skolemize value parameter symbols?

      val DefDef(_, _, tparams, vparamss, tpt, _) = ddef

      val meth = owner
      val methOwner = meth.owner
      val site = methOwner.thisType

      /* tparams already have symbols (created in enterDefDef/completerOf), namely the skolemized ones (created
       * by the PolyTypeCompleter constructor, and assigned to tparams). reenterTypeParams enters the type skolems
       * into scope and returns the non-skolems.
       */
      val tparamSyms = typer.reenterTypeParams(tparams)

      val tparamSkolems = tparams.map(_.symbol)

      /* since the skolemized tparams are in scope, the TypeRefs in types of vparamSymss refer to the type skolems
       * note that for parameters with missing types, `methodSig` reassigns types of these symbols (the parameter
       * types from the overridden method).
       */
      var vparamSymss = enterValueParams(vparamss)


      /**
       * Creates a method type using tparamSyms and vparamsSymss as argument symbols and `respte` as result type.
       * All typeRefs to type skolems are replaced by references to the corresponding non-skolem type parameter,
       * so the resulting type is a valid external method type, it does not contain (references to) skolems.
       */
      def thisMethodType(restpe: Type) = {
        val checkDependencies = new DependentTypeChecker(context)(this)
        checkDependencies check vparamSymss
        // DEPMETTODO: check not needed when they become on by default
        checkDependencies(restpe)

        val makeMethodType = (vparams: List[Symbol], restpe: Type) => {
          // TODODEPMET: check that we actually don't need to do anything here
          // new dependent method types: probably OK already, since 'enterValueParams' above
          // enters them in scope, and all have a lazy type. so they may depend on other params. but: need to
          // check that params only depend on ones in earlier sections, not the same. (done by checkDependencies,
          // so re-use / adapt that)
          if (meth.isJavaDefined)
          // TODODEPMET necessary?? new dependent types: replace symbols in restpe with the ones in vparams
            JavaMethodType(vparams map (p => p setInfo objToAny(p.tpe)), restpe)
          else
            MethodType(vparams, restpe)
        }


        val res = GenPolyType(
          tparamSyms, // deSkolemized symbols  -- TODO: check that their infos don't refer to method args?
          if (vparamSymss.isEmpty) NullaryMethodType(restpe)
          // vparamss refer (if they do) to skolemized tparams
          else (vparamSymss :\ restpe) (makeMethodType)
        )
        res.substSym(tparamSkolems, tparamSyms)
      }

      /**
       * Creates a schematic method type which has WildcardTypes for non specified
       * return or parameter types. For instance, in `def f[T](a: T, b) = ...`, the
       * type schema is
       *
       *   PolyType(T, MethodType(List(a: T, b: WildcardType), WildcardType))
       *
       * where T are non-skolems.
       */
      def methodTypeSchema(resTp: Type) = {
        // for all params without type set WildcaradType
        mforeach(vparamss)(v => if (v.tpt.isEmpty) v.symbol setInfo WildcardType)
        thisMethodType(resTp)
      }

      def overriddenSymbol(resTp: Type) = {
        intersectionType(methOwner.info.parents).nonPrivateMember(meth.name).filter { sym =>
          sym != NoSymbol && (site.memberType(sym) matches methodTypeSchema(resTp))
        }
      }
      // TODO: see whether this or something similar would work instead:
      // def overriddenSymbol = meth.nextOverriddenSymbol


      /**
       * If `meth` doesn't have an explicit return type, extracts the return type from the method
       * overridden by `meth` (if there's an unique one). This type is lateron used as the expected
       * type for computing the type of the rhs. The resulting type references type skolems for
       * type parameters (consistent with the result of `typer.typedType(tpt).tpe`).
       *
       * As a first side effect, this method assigns a MethodType constructed using this
       * return type to `meth`. This allows omitting the result type for recursive methods.
       *
       * As another side effect, this method also assigns paramter types from the overridden
       * method to parameters of `meth` that have missing types (the parser accepts missing
       * parameter types under -Yinfer-argument-types).
       */
      def typesFromOverridden(methResTp: Type): Type = {
        val overridden = overriddenSymbol(methResTp)
        if (overridden == NoSymbol || overridden.isOverloaded) {
          methResTp
        } else {
          overridden.cookJavaRawInfo() // #3404 xform java rawtypes into existentials
          var overriddenTp = site.memberType(overridden) match {
              case PolyType(tparams, rt) => rt.substSym(tparams, tparamSkolems)
              case mt => mt
            }
          for (vparams <- vparamss) {
            var overriddenParams = overriddenTp.params
            for (vparam <- vparams) {
              if (vparam.tpt.isEmpty) {
                val overriddenParamTp = overriddenParams.head.tpe
                // references to type parameteres in overriddenParamTp link to the type skolems, so the
                // assigned type is consistent with the other / existing parameter types in vparamSymss.
                vparam.symbol setInfo overriddenParamTp
                vparam.tpt defineType overriddenParamTp setPos vparam.pos.focus
              }
              overriddenParams = overriddenParams.tail
            }
            overriddenTp = overriddenTp.resultType
          }

          overriddenTp match {
            case NullaryMethodType(rtpe) => overriddenTp = rtpe
            case MethodType(List(), rtpe) => overriddenTp = rtpe
            case _ =>
          }

          if (tpt.isEmpty) {
            // provisionally assign `meth` a method type with inherited result type
            // that way, we can leave out the result type even if method is recursive.
            meth setInfo thisMethodType(overriddenTp)
            overriddenTp
          } else {
            methResTp
          }
        }
      }

      if (tpt.isEmpty && meth.name == nme.CONSTRUCTOR) {
        tpt defineType context.enclClass.owner.tpe
        tpt setPos meth.pos.focus
      }

      val methResTp = if (tpt.isEmpty) WildcardType else typer.typedType(tpt).tpe
      val resTpFromOverride = if (methOwner.isClass && (tpt.isEmpty || mexists(vparamss)(_.tpt.isEmpty))) {
          typesFromOverridden(methResTp)
        } else {
          methResTp
        }

      // Add a () parameter section if this overrides some method with () parameters
      if (methOwner.isClass && vparamss.isEmpty &&
        overriddenSymbol(methResTp).alternatives.exists(_.info.isInstanceOf[MethodType])) {
        vparamSymss = ListOfNil
      }

      // issue an error for missing parameter types
      mforeach(vparamss) { vparam =>
        if (vparam.tpt.isEmpty) {
          MissingParameterOrValTypeError(vparam)
          vparam.tpt defineType ErrorType
        }
      }

      addDefaultGetters(meth, vparamss, tparams, overriddenSymbol(methResTp))

      // fast track macros, i.e. macros defined inside the compiler, are hardcoded
      // hence we make use of that and let them have whatever right-hand side they need
      // (either "macro ???" as they used to or just "???" to maximally simplify their compilation)
      if (fastTrack contains meth) meth setFlag MACRO

      // macro defs need to be typechecked in advance
      // because @macroImpl annotation only gets assigned during typechecking
      // otherwise macro defs wouldn't be able to robustly coexist with their clients
      // because a client could be typechecked before a macro def that it uses
      if (meth.isTermMacro) {
        typer.computeMacroDefType(ddef, resTpFromOverride)
      }

      val res = thisMethodType({
        val rt = (
          if (!tpt.isEmpty) {
            methResTp
          } else {
            // return type is inferred, we don't just use resTpFromOverride. Here, C.f has type String:
            //   trait T { def f: Object }; class C <: T { def f = "" }
            // using resTpFromOverride as expected type allows for the following (C.f has type A):
            //   trait T { def f: A }; class C <: T { implicit def b2a(t: B): A = ???; def f = new B }
            assignTypeToTree(ddef, typer, resTpFromOverride)
          })
        // #2382: return type of default getters are always @uncheckedVariance
        if (meth.hasDefault)
          rt.withAnnotation(AnnotationInfo(uncheckedVarianceClass.tpe, List(), List()))
        else rt
      })
      pluginsTypeSig(res, typer, ddef, methResTp)
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
      val methOwner  = meth.owner
      val isConstr   = meth.isConstructor
      val overridden = if (isConstr || !methOwner.isClass) NoSymbol else overriddenSymbol
      val overrides  = overridden != NoSymbol && !overridden.isOverloaded
      // value parameters of the base class (whose defaults might be overridden)
      var baseParamss = (vparamss, overridden.tpe.paramss) match {
        // match empty and missing parameter list
        case (Nil, List(Nil)) => Nil
        case (List(Nil), Nil) => ListOfNil
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
                val module = companionSymbolOf(methOwner, context)
                module.initialize // call type completer (typedTemplate), adds the
                                  // module's templateNamer to classAndNamerOfModule
                module.attachments.get[ConstructorDefaultsAttachment] match {
                  // by martin: the null case can happen in IDE; this is really an ugly hack on top of an ugly hack but it seems to work
                  case Some(cda) =>
                    if (cda.companionModuleClassNamer == null) {
                      debugwarn(s"SI-6576 The companion module namer for $meth was unexpectedly null")
                      return
                    }
                    val p = (cda.classWithDefault, cda.companionModuleClassNamer)
                    moduleNamer = Some(p)
                    p
                  case _ =>
                    return // fix #3649 (prevent crash in erroneous source code)
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
              methOwner.resetFlag(INTERFACE) // there's a concrete member now
            val default = parentNamer.enterSyntheticSym(defaultTree)
            if (forInteractive && default.owner.isTerm) {
              // save the default getters as attachments in the method symbol. if compiling the
              // same local block several times (which can happen in interactive mode) we might
              // otherwise not find the default symbol, because the second time it the method
              // symbol will be re-entered in the scope but the default parameter will not.
              val att = meth.attachments.get[DefaultsOfLocalMethodAttachment] match {
                case Some(att) => att.defaultGetters += default
                case None => meth.updateAttachment(new DefaultsOfLocalMethodAttachment(default))
              }
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

    private def valDefSig(vdef: ValDef) = {
      val ValDef(_, _, tpt, rhs) = vdef
      val result = if (tpt.isEmpty) {
        if (rhs.isEmpty) {
          MissingParameterOrValTypeError(tpt)
          ErrorType
        }
        else assignTypeToTree(vdef, typer, WildcardType)
      } else {
        typer.typedType(tpt).tpe
      }
      pluginsTypeSig(result, typer, vdef, if (tpt.isEmpty) WildcardType else result)

    }

    //@M! an abstract type definition (abstract type member/type parameter)
    // may take type parameters, which are in scope in its bounds
    private def typeDefSig(tdef: TypeDef) = {
      val TypeDef(_, _, tparams, rhs) = tdef
      // log("typeDefSig(" + tpsym + ", " + tparams + ")")
      val tparamSyms = typer.reenterTypeParams(tparams) //@M make tparams available in scope (just for this abstypedef)
      val tp = typer.typedType(rhs).tpe match {
        case TypeBounds(lt, rt) if (lt.isError || rt.isError) =>
          TypeBounds.empty
        case tp @ TypeBounds(lt, rt) if (tdef.symbol hasFlag JAVA) =>
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
      val res = GenPolyType(tparamSyms, tp)
      pluginsTypeSig(res, typer, tdef, WildcardType)
    }

    private def importSig(imp: Import) = {
      val Import(expr, selectors) = imp
      val expr1 = typer.typedQualifier(expr)
      typer checkStable expr1
      if (expr1.symbol != null && expr1.symbol.isRootPackage)
        RootImportError(imp)

      if (expr1.isErrorTyped)
        ErrorType
      else {
        val newImport = treeCopy.Import(imp, expr1, selectors).asInstanceOf[Import]
        checkSelectors(newImport)
        transformed(imp) = newImport
        // copy symbol and type attributes back into old expression
        // so that the structure builder will find it.
        expr.symbol = expr1.symbol
        expr.tpe = expr1.tpe
        ImportType(expr1)
      }
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

    /**
     * TypeSig is invoked by monoTypeCompleters. It returns the type of a definition which
     * is then assigned to the corresponding symbol (typeSig itself does not need to assign
     * the type to the symbol, but it can if necessary).
     */
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
              // need to be lazy, #1782. beforeTyper to allow inferView in annotation args, SI-5892.
              AnnotationInfo lazily {
                val context1 = typer.context.make(ann)
                context1.setReportErrors()
                beforeTyper(newTyper(context1) typedAnnotation ann)
              }
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

      // TODO: meta-annotations to indicate where module annotations should go (module vs moduleClass)
      annotate(sym)
      if (sym.isModule) annotate(sym.moduleClass)

      def getSig = tree match {
        case cdef: ClassDef =>
          createNamer(tree).classSig(cdef)

        case mdef: ModuleDef =>
          createNamer(tree).moduleSig(mdef)

        case ddef: DefDef =>
          createNamer(tree).methodSig(ddef)

        case vdef: ValDef =>
          createNamer(tree).valDefSig(vdef)

        case tdef: TypeDef =>
          createNamer(tree).typeDefSig(tdef) //@M!

        case imp: Import =>
          importSig(imp)
      }

      try getSig
      catch typeErrorHandler(tree, ErrorType)
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
        checkNoConflict(IMPLICIT, CASE)
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
      if (sym.isAbstractOverride) {
          if (!sym.owner.isTrait)
            fail(AbstractOverride)
          if(sym.isType)
            fail(AbstractOverrideOnTypeMember)
      }
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

  /**
   * A class representing a lazy type with known type parameters. `ctx` is the namer context in which the
   * `owner` is defined.
   *
   * Constructing a PolyTypeCompleter for a DefDef creates type skolems for the type parameters and
   * assigns them to the `tparams` trees.
   */
  class PolyTypeCompleter(tparams: List[TypeDef], restp: TypeCompleter, ctx: Context) extends LockingTypeCompleter with FlagAgnosticCompleter {
    // @M. If `owner` is an abstract type member, `typeParams` are all NoSymbol (see comment in `completerOf`),
    // otherwise, the non-skolemized (external) type parameter symbols
    override val typeParams = tparams map (_.symbol)

    /* The definition tree (poly ClassDef, poly DefDef or HK TypeDef) */
    override val tree = restp.tree

    private val defnSym = tree.symbol

    if (defnSym.isTerm) {
      // for polymorphic DefDefs, create type skolems and assign them to the tparam trees.
      val skolems = deriveFreshSkolems(tparams map (_.symbol))
      map2(tparams, skolems)(_ setSymbol _)
    }

    def completeImpl(sym: Symbol) = {
      // @M an abstract type's type parameters are entered.
      // TODO: change to isTypeMember ?
      if (defnSym.isAbstractType)
        newNamerFor(ctx, tree) enterSyms tparams //@M
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
    original.companionSymbol orElse {
      ctx.lookup(original.name.companionName, original.owner).suchThat(sym =>
        (original.isTerm || sym.hasModuleFlag) &&
        (sym isCoDefinedWith original)
      )
    }
  }
}
