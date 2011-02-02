/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable.{HashMap, WeakHashMap}
import scala.ref.WeakReference
import symtab.Flags
import symtab.Flags._

/** This trait declares methods to create symbols and to enter them into scopes.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Namers { self: Analyzer =>
  import global._
  import definitions._

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

  private class NormalNamer(context : Context) extends Namer(context)
  def newNamer(context : Context) : Namer = new NormalNamer(context)

  // In the typeCompleter (templateSig) of a case class (resp it's module),
  // synthetic `copy' (reps `apply', `unapply') methods are added. To compute
  // their signatures, the corresponding ClassDef is needed.
  // During naming, for each case class module symbol, the corresponding ClassDef
  // is stored in this map. The map is cleared lazily, i.e. when the new symbol
  // is created with the same name, the old one (if present) is wiped out, or the
  // entry is deleted when it is used and no longer needed.
  private val caseClassOfModuleClass = new WeakHashMap[Symbol, WeakReference[ClassDef]]

  // Default getters of constructors are added to the companion object in the
  // typeCompleter of the constructor (methodSig). To compute the signature,
  // we need the ClassDef. To create and enter the symbols into the companion
  // object, we need the templateNamer of that module class.
  // This map is extended during naming of classes, the Namer is added in when
  // it's available, i.e. in the type completer (templateSig) of the module class.
  private[typechecker] val classAndNamerOfModule = new HashMap[Symbol, (ClassDef, Namer)]

  def resetNamer() {
    classAndNamerOfModule.clear
  }

  abstract class Namer(val context: Context) {

    val typer = newTyper(context)

    def setPrivateWithin[Sym <: Symbol](tree: Tree, sym: Sym, mods: Modifiers): Sym = {
      if (mods.hasAccessBoundary)
        sym.privateWithin = typer.qualifyingClass(tree, mods.privateWithin, true)
      sym
    }

    def inConstructorFlag: Long =
      if (context.owner.isConstructor && !context.inConstructorSuffix || context.owner.isEarlyInitialized) INCONSTRUCTOR
      else 0l

    def moduleClassFlags(moduleFlags: Long) =
      (moduleFlags & ModuleToClassFlags) | FINAL | inConstructorFlag

    def updatePosFlags(sym: Symbol, pos: Position, flags: Long): Symbol = {
      if (settings.debug.value) log("overwriting " + sym)
      val lockedFlag = sym.flags & LOCKED
      sym.reset(NoType)
      sym setPos pos
      sym.flags = flags | lockedFlag
      if (sym.isModule && sym.moduleClass != NoSymbol)
        updatePosFlags(sym.moduleClass, pos, moduleClassFlags(flags))
      var companion: Symbol = NoSymbol
      if (sym.owner.isPackageClass && {companion = companionSymbolOf(sym, context); companion != NoSymbol} &&
        (companion.rawInfo.isInstanceOf[loaders.SymbolLoader] ||
         companion.rawInfo.isComplete && runId(sym.validTo) != currentRunId))
        // pre-set linked symbol to NoType, in case it is not loaded together with this symbol.
        companion.setInfo(NoType)
      sym
    }

    private def isCopyGetter(meth: Symbol) = {
      meth.name startsWith (nme.copy + nme.DEFAULT_GETTER_STRING)
    }
    private def isTemplateContext(context: Context): Boolean = context.tree match {
      case Template(_, _, _) => true
      case Import(_, _) => isTemplateContext(context.outer)
      case _ => false
    }

    private var innerNamerCache: Namer = null
    protected def makeConstructorScope(classContext : Context) : Context = {
      val outerContext = classContext.outer.outer
      outerContext.makeNewScope(outerContext.tree, outerContext.owner)
    }

    def namerOf(sym: Symbol): Namer = {

      def innerNamer: Namer = {
        if (innerNamerCache eq null)
          innerNamerCache =
            if (!isTemplateContext(context)) this
            else newNamer(context.make(context.tree, context.owner, new Scope))
        innerNamerCache
      }

      def primaryConstructorParamNamer: Namer = { //todo: can we merge this with SCCmode?
        val classContext = context.enclClass
        val paramContext = makeConstructorScope(classContext)
        val unsafeTypeParams = context.owner.unsafeTypeParams
        unsafeTypeParams foreach(sym => paramContext.scope.enter(sym))
        newNamer(paramContext)
      }

      def usePrimary = sym.isTerm && (
        (sym.isParamAccessor) ||
        (sym.isParameter && sym.owner.isPrimaryConstructor)
      )

      if (usePrimary) primaryConstructorParamNamer
      else innerNamer
    }

    protected def conflict(newS : Symbol, oldS : Symbol) : Boolean = {
      (!oldS.isSourceMethod ||
        nme.isSetterName(newS.name) ||
        newS.owner.isPackageClass) &&
        !((newS.owner.isTypeParameter || newS.owner.isAbstractType) &&
          newS.name.length==1 && newS.name(0)=='_') //@M: allow repeated use of `_' for higher-order type params
    }

    private def setInfo[Sym <: Symbol](sym : Sym)(tpe : LazyType) : Sym = sym.setInfo(tpe)

    private def doubleDefError(pos: Position, sym: Symbol) {
      context.error(pos,
        sym.name.toString() + " is already defined as " +
        (if (sym.isSynthetic)
          "(compiler-generated) "+ (if (sym.isModule) "case class companion " else "")
         else "") +
        (if (sym.isCase) "case class " + sym.name else sym.toString()))
    }

    private def inCurrentScope(m: Symbol): Boolean = {
      if (context.owner.isClass) context.owner == m.owner
      else m.owner.isClass && context.scope == m.owner.info.decls
    }

    /** Enter symbol into context's scope and return symbol itself */
    def enterInScope(sym: Symbol): Symbol = enterInScope(sym, context.scope)

    /** Enter symbol into given scope and return symbol itself */
    def enterInScope(sym: Symbol, scope: Scope): Symbol = {
      // allow for overloaded methods
      if (!(sym.isSourceMethod && sym.owner.isClass && !sym.owner.isPackageClass)) {
        var prev = scope.lookupEntry(sym.name)
        if ((prev ne null) && prev.owner == scope && conflict(sym, prev.sym)) {
          doubleDefError(sym.pos, prev.sym)
          sym setInfo ErrorType
          scope unlink prev.sym // let them co-exist...
          scope enter sym
        } else scope enter sym
      } else scope enter sym
    }

    def enterPackageSymbol(pos: Position, pid: RefTree, pkgOwner: Symbol): Symbol = {
      val owner = pid match {
        case Ident(name) =>
          pkgOwner
        case Select(qual: RefTree, name) =>
          enterPackageSymbol(pos, qual, pkgOwner).moduleClass
      }
      var pkg = owner.info.decls.lookup(pid.name)
      if (!pkg.isPackage || owner != pkg.owner) {
        pkg = owner.newPackage(pos, pid.name.toTermName)
        pkg.moduleClass.setInfo(new PackageClassInfoType(new Scope, pkg.moduleClass))
        pkg.setInfo(pkg.moduleClass.tpe)
        enterInScope(pkg, owner.info.decls)
      }
      pkg
    }

    def enterClassSymbol(tree : ClassDef): Symbol = {
      var c: Symbol = context.scope.lookup(tree.name)
      if (c.isType && c.owner.isPackageClass && context.scope == c.owner.info.decls && currentRun.canRedefine(c)) {
        updatePosFlags(c, tree.pos, tree.mods.flags)
        setPrivateWithin(tree, c, tree.mods)
      } else {
        var sym = context.owner.newClass(tree.pos, tree.name)
        sym = sym.setFlag(tree.mods.flags | inConstructorFlag)
        sym = setPrivateWithin(tree, sym, tree.mods)
        c = enterInScope(sym)
      }
      if (c.owner.isPackageClass) {
        val file = context.unit.source.file
        val clazz = c.asInstanceOf[ClassSymbol]
        if (settings.debug.value && (clazz.sourceFile ne null) && !clazz.sourceFile.equals(file)) {
          Console.err.println("SOURCE MISMATCH: " + clazz.sourceFile + " vs. " + file + " SYM=" + c);
        }
        clazz.sourceFile = file
        if (clazz.sourceFile ne null) {
          assert(currentRun.canRedefine(clazz) || clazz.sourceFile == currentRun.symSource(c));
          currentRun.symSource(c) = clazz.sourceFile
        }
        registerTopLevelSym(clazz)
      }
      assert(c.name.toString.indexOf('(') == -1)
      c
    }

    /** Enter a module symbol. The tree parameter can be either a module definition
     *  or a class definition */
    def enterModuleSymbol(tree : ModuleDef): Symbol = {
      // .pos, mods.flags | MODULE | FINAL, name
      var m: Symbol = context.scope.lookup(tree.name)
      val moduleFlags = tree.mods.flags | MODULE | FINAL
      if (m.isModule && !m.isPackage && inCurrentScope(m) &&
          (currentRun.canRedefine(m) || m.isSynthetic)) {
        updatePosFlags(m, tree.pos, moduleFlags)
        setPrivateWithin(tree, m, tree.mods)
        if (m.moduleClass != NoSymbol)
          setPrivateWithin(tree, m.moduleClass, tree.mods)

        context.unit.synthetics -= m
      } else {
        m = context.owner.newModule(tree.pos, tree.name)
        m.setFlag(moduleFlags)
        m = setPrivateWithin(tree, m, tree.mods)
        m = enterInScope(m)

        m.moduleClass.setFlag(moduleClassFlags(moduleFlags))
        setPrivateWithin(tree, m.moduleClass, tree.mods)
      }
      if (m.owner.isPackageClass && !m.isPackage) {
        m.moduleClass.sourceFile = context.unit.source.file
        currentRun.symSource(m) = m.moduleClass.sourceFile
        registerTopLevelSym(m)
      }
      m
    }

    def enterSyms(trees: List[Tree]): Namer = {
      var namer : Namer = this
      for (tree <- trees) {
        val txt = namer.enterSym(tree)
        if (txt ne namer.context) namer = newNamer(txt)
      }
      namer
    }

    def newTypeSkolems(tparams: List[Symbol]): List[Symbol] = {
      val tskolems = tparams map (_.newTypeSkolem)
      val ltp = new LazyType {
        override def complete(sym: Symbol) {
          sym setInfo sym.deSkolemize.info.substSym(tparams, tskolems) //@M the info of a skolem is the skolemized info of the actual type parameter of the skolem
        }
      }
      tskolems foreach (_.setInfo(ltp))
      tskolems
    }

    /** Replace type parameters with their TypeSkolems, which can later be deskolemized to the original type param
     * (a skolem is a representation of a bound variable when viewed inside its scope)
     *  !!!Adriaan: this does not work for hk types.
     */
    def skolemize(tparams: List[TypeDef]) {
      val tskolems = newTypeSkolems(tparams map (_.symbol))
      for ((tparam, tskolem) <- tparams zip tskolems) tparam.symbol = tskolem
    }

    def applicableTypeParams(owner: Symbol): List[Symbol] =
      if (owner.isTerm || owner.isPackageClass) List()
      else applicableTypeParams(owner.owner) ::: owner.typeParams

    /** If no companion object for clazz exists yet, create one by applying `creator` to
     *  class definition tree.
     *  @return the companion object symbol.
     */
     def ensureCompanionObject(tree: ClassDef, creator: => Tree): Symbol = {
       val m = companionModuleOf(tree.symbol, context)
       // @luc: not sure why "currentRun.compiles(m)" is needed, things breaks
       // otherwise. documentation welcome.
       if (m != NoSymbol && currentRun.compiles(m)) m
       else enterSyntheticSym(creator)
     }

    private def enterSymFinishWith(tree: Tree, tparams: List[TypeDef]) {
      val sym = tree.symbol
      if (settings.debug.value) log("entered " + sym + " in " + context.owner + ", scope-id = " + context.scope.## )
      var ltype = namerOf(sym).typeCompleter(tree)
      if (tparams nonEmpty) {
        //@M! TypeDef's type params are handled differently
        //@M e.g., in [A[x <: B], B], A and B are entered first as both are in scope in the definition of x
        //@M x is only in scope in `A[x <: B]'
        if(!sym.isAbstractType) //@M TODO: change to isTypeMember ?
          newNamer(context.makeNewScope(tree, sym)).enterSyms(tparams)

        ltype = new PolyTypeCompleter(tparams, ltype, tree, sym, context) //@M
        if (sym.isTerm) skolemize(tparams)
      }

      if (sym.name == nme.copy || isCopyGetter(sym)) {
        // it could be a compiler-generated copy method or one of its default getters
        setInfo(sym)(mkTypeCompleter(tree)(copySym => {
          def copyIsSynthetic() = sym.owner.info.member(nme.copy).isSynthetic
          if (sym.isSynthetic && (!sym.hasDefaultFlag || copyIsSynthetic())) {
            // the 'copy' method of case classes needs a special type completer to make bug0054.scala (and others)
            // work. the copy method has to take exactly the same parameter types as the primary constructor.
            val constrType = copySym.owner.primaryConstructor.tpe
            val subst = new SubstSymMap(copySym.owner.typeParams, tparams map (_.symbol))
            for ((params, cparams) <- tree.asInstanceOf[DefDef].vparamss.zip(constrType.paramss);
                 (param, cparam) <- params.zip(cparams)) {
              // need to clone the type cparam.tpe??? problem is: we don't have the new owner yet (the new param symbol)
              param.tpt.setType(subst(cparam.tpe))
            }
          }
          ltype.complete(sym)
        }))
      } else setInfo(sym)(ltype)
    }

    def enterIfNotThere(sym: Symbol) {
      val scope = context.scope
      var e = scope.lookupEntry(sym.name)
      while ((e ne null) && (e.owner eq scope) && (e.sym ne sym)) e = e.tail
      if (!((e ne null) && (e.owner eq scope))) context.scope.enter(sym)
    }

    def enterSym(tree: Tree): Context = {
      def finishWith(tparams: List[TypeDef]) { enterSymFinishWith(tree, tparams) }
      def finish = finishWith(Nil)
      def sym = tree.symbol
      if (sym != NoSymbol) {
        if (forInteractive && sym != null && sym.owner.isTerm) {
          // this logic is needed in case typer was interrupted half way through and then comes
          // back to do the tree again. In that case the definitions that were already
          // attributed as well as any default parameters of such methods need to be
          // re-entered in the current scope.
          enterIfNotThere(sym)
          if (sym.isLazy) {
            val acc = sym.lazyAccessor
            if (acc != NoSymbol) enterIfNotThere(acc)
          }
          defaultParametersOfMethod(sym) foreach enterIfNotThere
        }
        return this.context
      }
      try {
        val owner = context.owner
        tree match {
          case PackageDef(pid, stats) =>
            tree.symbol = enterPackageSymbol(tree.pos, pid,
              if (context.owner == EmptyPackageClass) RootClass else context.owner)
            val namer = newNamer(context.make(tree, sym.moduleClass, sym.info.decls))
            namer enterSyms stats

          case tree @ ClassDef(mods, name, tparams, impl) =>
            tree.symbol = enterClassSymbol(tree)
            finishWith(tparams)
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
          case tree @ ModuleDef(mods, name, _) =>
            tree.symbol = enterModuleSymbol(tree)
            sym.moduleClass setInfo namerOf(sym).moduleClassTypeCompleter(tree)
            finish

          case vd @ ValDef(mods, name, tp, rhs) =>
            if ((!context.owner.isClass ||
                 (mods.isPrivateLocal && !mods.isCaseAccessor) ||
                 name.startsWith(nme.OUTER) ||
                 context.unit.isJava) &&
                 !mods.isLazy) {
              val vsym = owner.newValue(tree.pos, name).setFlag(mods.flags);
              if(context.unit.isJava) setPrivateWithin(tree, vsym, mods) // #3663 -- for Scala fields we assume private[this]
              tree.symbol = enterInScope(vsym)
              finish
            } else {
              val mods1 =
            	  if (mods.isPrivateLocal && !mods.isLazy) {
                    context.error(tree.pos, "private[this] not allowed for case class parameters")
                    mods &~ LOCAL
                  } else mods
              // add getter and possibly also setter
              if (nme.isSetterName(name))
                context.error(tree.pos, "Names of vals or vars may not end in `_='")
              // .isInstanceOf[..]: probably for (old) IDE hook. is this obsolete?
              val getter = enterAccessorMethod(tree, name, getterFlags(mods1.flags), mods1)
              setInfo(getter)(namerOf(getter).getterTypeCompleter(vd))
              if (mods1.isMutable) {
                val setter = enterAccessorMethod(tree, nme.getterToSetter(name), setterFlags(mods1.flags), mods1)
                setInfo(setter)(namerOf(setter).setterTypeCompleter(vd))
              }

              tree.symbol =
                if (mods1.isDeferred) {
                  getter setPos tree.pos // unfocus getter position, because there won't be a separate value
                } else {
                  val vsym =
                    if (!context.owner.isClass) {
                      assert(mods1.isLazy)   // if not a field, it has to be a lazy val
                      owner.newValue(tree.pos, name + "$lzy" ).setFlag((mods1.flags | MUTABLE) & ~IMPLICIT)
                    } else {
                      val mFlag = if (mods1.isLazy) MUTABLE else 0
                      val lFlag = if (mods.isPrivateLocal) 0 else LOCAL
                      val newflags = mods1.flags & FieldFlags | PRIVATE | lFlag | mFlag
                      owner.newValue(tree.pos, nme.getterToLocal(name)) setFlag newflags
                    }
                  enterInScope(vsym)
                  setInfo(vsym)(namerOf(vsym).typeCompleter(tree))
                  if (mods1.isLazy)
                    vsym.setLazyAccessor(getter)

                  vsym
                }
              addBeanGetterSetter(vd, getter)
            }
          case DefDef(mods, nme.CONSTRUCTOR, tparams, _, _, _) =>
            val sym = owner.newConstructor(tree.pos).setFlag(mods.flags | owner.getFlag(ConstrFlags))
            setPrivateWithin(tree, sym, mods)
            tree.symbol = enterInScope(sym)
            finishWith(tparams)
          case DefDef(mods, name, tparams, _, _, _) =>
            tree.symbol = enterNewMethod(tree, name, mods.flags, mods, tree.pos)
            finishWith(tparams)
          case TypeDef(mods, name, tparams, _) =>
            var flags: Long = mods.flags
            if ((flags & PARAM) != 0L) flags |= DEFERRED
            val sym = new TypeSymbol(owner, tree.pos, name).setFlag(flags)
            setPrivateWithin(tree, sym, mods)
            tree.symbol = enterInScope(sym)
            finishWith(tparams)
          case DocDef(_, defn) =>
            enterSym(defn)
          case imp @ Import(_, _) =>
            tree.symbol = NoSymbol.newImport(tree.pos)
            setInfo(sym)(namerOf(sym).typeCompleter(tree))
            return context.makeNewImport(imp)
          case _ =>
        }
      }
      catch {
        case ex: TypeError =>
          //Console.println("caught " + ex + " in enterSym")//DEBUG
          typer.reportTypeError(tree.pos, ex)
          this.context
      }
      this.context
    }

    def enterSyntheticSym(tree: Tree): Symbol = {
      enterSym(tree)
      context.unit.synthetics(tree.symbol) = tree
      tree.symbol
    }

    def enterNewMethod(tree: Tree, name: Name, flags: Long, mods: Modifiers, pos: Position): TermSymbol = {
      val sym = context.owner.newMethod(pos, name.toTermName).setFlag(flags)
      setPrivateWithin(tree, sym, mods)
      enterInScope(sym)
      sym
    }

    def enterAccessorMethod(tree: Tree, name: Name, flags: Long, mods: Modifiers): TermSymbol =
      enterNewMethod(tree, name, flags, mods, tree.pos.focus)

    private def addBeanGetterSetter(vd: ValDef, getter: Symbol) {
      def isAnn(ann: Tree, demand: String) = ann match {
        case Apply(Select(New(Ident(name)), _), _) =>
          name.toString == demand
        case Apply(Select(New(Select(pre, name)), _), _) =>
          name.toString == demand
        case _ => false
      }
      val ValDef(mods, name, tpt, _) = vd
      val hasBP = mods.annotations.exists(isAnn(_, "BeanProperty"))
      val hasBoolBP = mods.annotations.exists(isAnn(_, "BooleanBeanProperty"))
      if ((hasBP || hasBoolBP) && !forMSIL) {
        if (!name(0).isLetter)
          context.error(vd.pos, "`BeanProperty' annotation can be applied "+
                                "only to fields that start with a letter")
        else if (mods.isPrivate)
          // avoids name clashes with private fields in traits
          context.error(vd.pos, "`BeanProperty' annotation can only be applied "+
                                "to non-private fields")
        else {
          val flags = mods.flags & (DEFERRED | OVERRIDE | STATIC)
          val beanName = name.toString.capitalize

          val getterName = if (hasBoolBP) "is" + beanName
                           else "get" + beanName
          val getterMods = Modifiers(flags, mods.privateWithin, Nil, mods.positions)
          val beanGetterDef = atPos(vd.pos.focus) {
            DefDef(getterMods, getterName, Nil, List(Nil), tpt.duplicate,
                   if (mods.isDeferred) EmptyTree
                   else Select(This(getter.owner.name.toTypeName), name)) }
          enterSyntheticSym(beanGetterDef)

          if (mods.isMutable) {
            // can't use "enterSyntheticSym", because the parameter type is not yet
            // known. instead, uses the same machinery as for the non-bean setter:
            // create and enter the symbol here, add the tree in Typer.addGettterSetter.
            val setterName = "set" + beanName
            val setter = enterAccessorMethod(vd, setterName, flags, mods)
              .setPos(vd.pos.focus)
            setInfo(setter)(namerOf(setter).setterTypeCompleter(vd))
          }
        }
      }
    }

// --- Lazy Type Assignment --------------------------------------------------

    def typeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      if (settings.debug.value) log("defining " + sym + Flags.flagsToString(sym.flags)+sym.locationString)
      val tp = typeSig(tree)
      tp match {
        case TypeBounds(lo, hi) =>
          // check that lower bound is not an F-bound
          for (t <- lo) {
            t match {
              case TypeRef(_, sym, _) => sym.initialize
              case _ =>
            }
          }
        case _ =>
      }
      sym.setInfo(tp)
      if ((sym.isAliasType || sym.isAbstractType) && !sym.isParameter &&
          !typer.checkNonCyclic(tree.pos, tp))
        sym.setInfo(ErrorType) // this early test is there to avoid infinite baseTypes when
                               // adding setters and getters --> bug798
      if (settings.debug.value) log("defined " + sym);
      validate(sym)
    }

    def moduleClassTypeCompleter(tree: Tree) = {
      mkTypeCompleter(tree) { sym =>
        val moduleSymbol = tree.symbol
        assert(moduleSymbol.moduleClass == sym)
        moduleSymbol.info // sets moduleClass info as a side effect.
        //assert(sym.rawInfo.isComplete)
      }
    }

    def getterTypeCompleter(vd: ValDef) = mkTypeCompleter(vd) { sym =>
      if (settings.debug.value) log("defining " + sym)
      val tp = typeSig(vd)
      sym.setInfo(NullaryMethodType(tp))
      if (settings.debug.value) log("defined " + sym)
      validate(sym)
    }

    def setterTypeCompleter(vd: ValDef) = mkTypeCompleter(vd) { sym =>
      if (settings.debug.value) log("defining " + sym)
      val param = sym.newSyntheticValueParam(typeSig(vd))
      sym.setInfo(MethodType(List(param), UnitClass.tpe))
      if (settings.debug.value) log("defined " + sym)
      validate(sym)
    }

    def selfTypeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      var selftpe = typer.typedType(tree).tpe
      if (!(selftpe.typeSymbol isNonBottomSubClass sym.owner))
        selftpe = intersectionType(List(sym.owner.tpe, selftpe))
//    println("completing self of "+sym.owner+": "+selftpe)
      sym.setInfo(selftpe)
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
     *  Locally defined symbols are also excluded from widening.
     *
     *  PP:is there a useful difference in practice between widen
     *  and deconst which wouldn't be achieved by simply calling widen
     *  in both situations? If so, could an example of this be added?
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
      val tpe1 = tpe.deconst
      val tpe2 = tpe1.widen
      if ((sym.isVariable || sym.isMethod && !sym.hasAccessorFlag))
        if (tpe2 <:< pt) tpe2 else tpe1
      else if (isHidden(tpe)) tpe2
      else if (sym.isFinal || sym.isLocal) tpe
      else tpe1
    }

    // sets each ValDef's symbol
    def enterValueParams(owner: Symbol, vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      def enterValueParam(param: ValDef): Symbol = {
        param.symbol = setInfo(
          enterInScope{
            val sym = owner.newValueParameter(param.pos, param.name).
              setFlag(param.mods.flags & (BYNAMEPARAM | IMPLICIT | DEFAULTPARAM))
            setPrivateWithin(param, sym, param.mods)
          })(typeCompleter(param))
        param.symbol
      }
      vparamss.map(_.map(enterValueParam))
    }

    private def templateSig(templ: Template): Type = {
      val clazz = context.owner
      def checkParent(tpt: Tree): Type = {
        val tp = tpt.tpe
        if (tp.typeSymbol == context.owner) {
          context.error(tpt.pos, ""+tp.typeSymbol+" inherits itself")
          AnyRefClass.tpe
        } else if (tp.isError) {
          AnyRefClass.tpe
        } else {
          tp
        }
      }
      def enterSelf(self: ValDef) {
        if (!self.tpt.isEmpty) {
          clazz.typeOfThis = selfTypeCompleter(self.tpt)
          self.symbol = clazz.thisSym.setPos(self.pos)
        } else {
          self.tpt defineType NoType
          if (self.name != nme.WILDCARD) {
            clazz.typeOfThis = clazz.tpe
            self.symbol = clazz.thisSym
          } else if (self ne emptyValDef) {
            self.symbol = clazz.newThisSym(self.pos) setInfo clazz.tpe
          }
        }
        if (self.name != nme.WILDCARD) {
          self.symbol.name = self.name
          self.symbol = context.scope enter self.symbol
        }
      }

      /* experimental code for allowiong early types as type parameters
      val earlyTypes = templ.body filter (treeInfo.isEarlyTypeDef)

      val parentTyper =
        if (earlyTypes.isEmpty) typer
        else {
          val earlyContext = context.outer.makeNewScope(context.tree, context.outer.owner.newLocalDummy(templ.pos))
          newNamer(earlyContext).enterSyms(earlyTypes)
          newTyper(earlyContext).typedStats(earlyTypes, context.owner)

          val parentContext = context.makeNewScope(context.tree, context.owner)
          for (etdef <- earlyTypes) parentContext.scope enter etdef.symbol
          newTyper(parentContext)
        }
      var parents = parentTyper.parentTypes(templ) map checkParent
      if (!earlyTypes.isEmpty) {
        val earlyMap = new EarlyMap(context.owner)
        for (etdef <- earlyTypes) {
          val esym = etdef.symbol
          esym.owner = context.owner
          esym.asInstanceOf[TypeSymbol].refreshType()
          esym setInfo earlyMap(esym.info)
        }

/*
        println("earlies: "+(earlyTypes map (_.symbol)))
        println("earlies: "+(earlyTypes map (_.symbol.tpe)))
        println("earlies: "+(earlyTypes map (_.symbol.info)))
        println("parents: "+parents)
        println(templ)

*/

      }
*/
      var parents = typer.parentTypes(templ) map checkParent
      enterSelf(templ.self)
      val decls = new Scope
//      for (etdef <- earlyTypes) decls enter etdef.symbol
      val templateNamer = newNamer(context.make(templ, clazz, decls))
        .enterSyms(templ.body)

      /* add overridden virtuals to parents
      val overridden = clazz.overriddenVirtuals
      if (!overridden.isEmpty)
        parents = parents ::: ( overridden map (
          sym => TypeRef(clazz.owner.thisType, sym, clazz.typeParams map (_.tpe))))
      println("Parents of "+clazz+":"+parents)

      // check that virtual classes are only defined as members of templates
      if (clazz.isVirtualClass && !clazz.owner.isClass)
        context.error(
          clazz.pos,
          "virtual traits and their subclasses must be defined as members of some other class")

      // make subclasses of virtual classes virtual as well; check that
      // they are defined in same scope.
      val virtualParents = parents map (_.typeSymbol) filter (_.isVirtualClass)
      virtualParents find {
        vp => !(clazz.owner.isClass && (clazz.owner isSubClass vp.owner))
      } match {
        case Some(vp) =>
          context.error(
            clazz.pos,
            "subclass of virtual "+vp+
            " needs to be defined at same level,\nas member of "+vp.owner)
        case None =>
          if (!virtualParents.isEmpty) clazz setFlag DEFERRED // make it virtual
      }
	  */

      // add apply and unapply methods to companion objects of case classes,
      // unless they exist already; here, "clazz" is the module class
      if (clazz.isModuleClass) {
        Namers.this.caseClassOfModuleClass get clazz map { cdefRef =>
          val cdef = cdefRef()
          addApplyUnapply(cdef, templateNamer)
          caseClassOfModuleClass -= clazz
        }
      }

      // add the copy method to case classes; this needs to be done here, not in SyntheticMethods, because
      // the namer phase must traverse this copy method to create default getters for its parameters.
      // here, clazz is the ClassSymbol of the case class (not the module).
      // @check: this seems to work only if the type completer of the class runs before the one of the
      // module class: the one from the module class removes the entry form caseClassOfModuleClass (see above).
      if (clazz.isClass && !clazz.hasModuleFlag) {
        Namers.this.caseClassOfModuleClass get companionModuleOf(clazz, context).moduleClass map { cdefRef =>
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
      if (classAndNamerOfModule contains module) {
        val (cdef, _) = classAndNamerOfModule(module)
        classAndNamerOfModule(module) = (cdef, templateNamer)
      }

      if (opt.verbose) {
        log(
          "ClassInfoType(\n%s,\n%s,\n%s)".format(
            "  " + (parents map (_.typeSymbol) mkString ", "),
            if (global.opt.debug) decls.toList map (">> " + _) mkString("\n", "\n", "") else "  <decls>",
            "  " + clazz)
        )
      }
      ClassInfoType(parents, decls, clazz)
    }

    private def classSig(tparams: List[TypeDef], impl: Template): Type =
      polyType(typer.reenterTypeParams(tparams), templateSig(impl))

    private def methodSig(mods: Modifiers, tparams: List[TypeDef],
                          vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): Type = {
      val meth = context.owner

      // enters the skolemized version into scope, returns the deSkolemized symbols
      val tparamSyms = typer.reenterTypeParams(tparams)
      // since the skolemized tparams are in scope, the TypeRefs in vparamSymss refer to skolemized tparams
      var vparamSymss = enterValueParams(meth, vparamss)
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
          if (meth hasFlag JAVA) vparam.setInfo(objToAny(vparam.tpe)) else vparam)
         // TODODEPMET necessary?? new dependent types: replace symbols in restpe with the ones in vparams
        if (meth hasFlag JAVA) JavaMethodType(params, restpe)
        else MethodType(params, restpe)
      }

      def thisMethodType(restpe: Type) =  {
        import scala.collection.mutable.ListBuffer
        val okParams = ListBuffer[Symbol]()
        // can we relax these restrictions? see test/files/pos/depmet_implicit_oopsla_session_2.scala and neg/depmet_try_implicit.scala for motivation
        // should allow forward references since type selections on implicit args are like type parameters:
        // def foo[T](a: T, x: w.T2)(implicit w: ComputeT2[T])
        // is more compact than: def foo[T, T2](a: T, x: T2)(implicit w: ComputeT2[T, T2])
        // moreover, the latter is not an encoding of the former, which hides type inference of T2, so you can specify T while T2 is purely computed
        val checkDependencies: TypeTraverser = new TypeTraverser {
          def traverse(tp: Type) = {
            tp match {
              case SingleType(_, sym) =>
                if (sym.owner == meth && sym.isValueParameter && !(okParams contains sym))
                  context.error(
                    sym.pos,
                    "illegal dependent method type"+
                    (if (settings.YdepMethTpes.value)
                       ": parameter appears in the type of another parameter in the same section or an earlier one"
                     else ""))
              case _ =>
                mapOver(tp)
            }
            this
          }
        }
        for(vps <- vparamSymss) {
          for(p <- vps) checkDependencies(p.info)
          if(settings.YdepMethTpes.value) okParams ++= vps // can only refer to symbols in earlier parameter sections (if the extension is enabled)
        }
        checkDependencies(restpe) // DEPMETTODO: check not needed when they become on by default

        polyType(
          tparamSyms, // deSkolemized symbols  -- TODO: check that their infos don't refer to method args?
          if (vparamSymss.isEmpty) NullaryMethodType(restpe)
          // vparamss refer (if they do) to skolemized tparams
          else (vparamSymss :\ restpe) (makeMethodType))
      }

      var resultPt = if (tpt.isEmpty) WildcardType else typer.typedType(tpt).tpe
      val site = meth.owner.thisType

      def overriddenSymbol = intersectionType(meth.owner.info.parents).nonPrivateMember(meth.name).filter(sym => {
        // luc: added .substSym from skolemized to deSkolemized
        // site.memberType(sym): PolyType(tparams, MethodType(..., ...)) ==> all references to tparams are deSkolemized
        // thisMethodType: tparams in PolyType are deSkolemized, the references in the MethodTypes are skolemized. ==> the two didn't match
        // for instance, B.foo would not override A.foo, and the default on parameter b would not be inherited
        //   class A { def foo[T](a: T)(b: T = a) = a }
        //   class B extends A { override def foo[U](a: U)(b: U) = b }
        sym != NoSymbol && (site.memberType(sym) matches thisMethodType(resultPt).substSym(tparams map (_.symbol), tparamSyms))
      })

      // fill in result type and parameter types from overridden symbol if there is a unique one.
      if (meth.owner.isClass && (tpt.isEmpty || vparamss.exists(_.exists(_.tpt.isEmpty)))) {
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
            // provisionally assign `meth' a method type with inherited result type
            // that way, we can leave out the result type even if method is recursive.
            meth setInfo thisMethodType(resultPt)
          }
        }
      }
      // Add a () parameter section if this overrides some method with () parameters.
      if (meth.owner.isClass && vparamss.isEmpty && overriddenSymbol.alternatives.exists(
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
          // replace deSkolemized symbols with skolemized ones (for resultPt computed by looking at overridden symbol, right?)
          val pt = resultPt.substSym(tparamSyms, tparams map (_.symbol))
          // compute result type from rhs
          tpt defineType widenIfNecessary(meth, typer.computeType(rhs, pt), pt)
          tpt setPos meth.pos.focus
          tpt.tpe
        } else typer.typedType(tpt).tpe
        // #2382: return type of default getters are always @uncheckedVariance
        if (meth.hasDefaultFlag)
          rt.withAnnotation(AnnotationInfo(definitions.uncheckedVarianceClass.tpe, List(), List()))
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

      val isConstr = meth.isConstructor
      val overridden = if (isConstr || !meth.owner.isClass) NoSymbol
                       else overriddenSymbol
      val overrides = overridden != NoSymbol && !overridden.isOverloaded
      // value parameters of the base class (whose defaults might be overridden)
      var baseParamss = overridden.tpe.paramss
        // match empty and missing parameter list
        if (vparamss.isEmpty && baseParamss == List(Nil)) baseParamss = Nil
        if (vparamss == List(Nil) && baseParamss.isEmpty) baseParamss = List(Nil)
        assert(!overrides || vparamss.length == baseParamss.length, ""+ meth.fullName + ", "+ overridden.fullName)

      // cache the namer used for entering the default getter symbols
      var ownerNamer: Option[Namer] = None
      var moduleNamer: Option[(ClassDef, Namer)] = None

      var posCounter = 1

      // for each value parameter, create the getter method if it has a default argument. previous
      // denotes the parameter lists which are on the left side of the current one. these get added
      // to the default getter. Example: "def foo(a: Int)(b: Int = a)" gives "foo$default$1(a: Int) = a"
      (List[List[ValDef]]() /: (vparamss))((previous: List[List[ValDef]], vparams: List[ValDef]) => {
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
                val module = companionModuleOf(meth.owner, context)
                module.initialize // call type completer (typedTemplate), adds the
                                  // module's templateNamer to classAndNamerOfModule
                if (!classAndNamerOfModule.contains(module))
                  return // fix #3649 (prevent crash in erroneous source code)
                val (cdef, nmr) = classAndNamerOfModule(module)
                moduleNamer = Some(cdef, nmr)
                (cdef, nmr)
              }
              deftParams = cdef.tparams map copyUntypedInvariant
              nmr
            } else {
              ownerNamer.getOrElse {
                val ctx = context.nextEnclosing(c => c.scope.toList.contains(meth))
                assert(ctx != NoContext)
                val nmr = newNamer(ctx)
                ownerNamer = Some(nmr)
                nmr
              }
            }

            // If the parameter type mentions any type parameter of the method, let the compiler infer the
            // return type of the default getter => allow "def foo[T](x: T = 1)" to compile.
            // This is better than always using Wildcard for inferring the result type, for example in
            //    def f(i: Int, m: Int => Int = identity _) = m(i)
            // if we use Wildcard as expected, we get "Nothing => Nothing", and the default is not usable.
            val names = deftParams map { case TypeDef(_, name, _, _) => name }
            object subst extends Transformer {
              override def transform(tree: Tree): Tree = tree match {
                case Ident(name) if (names contains name) =>
                  TypeTree()
                case _ =>
                  super.transform(tree)
              }
              def apply(tree: Tree) = {
                val r = transform(tree)
                if (r.exists(_.isEmpty)) TypeTree()
                else r
              }
            }

            val defTpt = subst(copyUntyped(vparam.tpt match {
              // default getter for by-name params
              case AppliedTypeTree(_, List(arg)) if sym.hasFlag(BYNAMEPARAM) => arg
              case t => t
            }))
            val defRhs = copyUntyped(vparam.rhs)

            val defaultTree = atPos(vparam.pos.focus) {
              DefDef(
                Modifiers(meth.flags & (PRIVATE | PROTECTED | FINAL)) | SYNTHETIC | DEFAULTPARAM | oflag,
                name, deftParams, defvParamss, defTpt, defRhs)
            }
            if (!isConstr)
              meth.owner.resetFlag(INTERFACE) // there's a concrete member now
            val default = parentNamer.enterSyntheticSym(defaultTree)
            if (forInteractive && default.owner.isTerm) {
              // enter into map from method symbols to default arguments.
              // if compiling the same local block several times (which can happen in interactive mode)
              // we might otherwise not find the default symbol, because the second time it the
              // method symbol will be re-entered in the scope but the default parameter will not.
              defaultParametersOfMethod(meth) += default
            }
          } else if (baseHasDefault) {
            // the parameter does not have a default itself, but the corresponding parameter
            // in the base class does.
            sym.setFlag(DEFAULTPARAM)
          }
          posCounter += 1
          if (overrides) baseParams = baseParams.tail
        }
        if (overrides) baseParamss = baseParamss.tail
        previous ::: List(vparams)
      })
    }

    //@M! an abstract type definition (abstract type member/type parameter) may take type parameters, which are in scope in its bounds
    private def typeDefSig(tpsym: Symbol, tparams: List[TypeDef], rhs: Tree) = {
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
      // doesn't change a type member's arity (number of type parameters),
      // e.g. trait T { type X[A] }; type S = T{type X}; val x: S
      // X in x.X[A] will get rebound to the X in the refinement, which does not take any type parameters
      // this mismatch does not crash the compiler (anymore), but leads to weird type errors,
      // as x.X[A] will become NoType internally
      // it's not obvious the errror refers to the X in the refinement and not the original X
      // however, separate compilation requires the symbol info to be loaded to do this check,
      // but loading the info will probably lead to spurious cyclic errors --> omit the check
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
      if (!(cdef.symbol hasFlag ABSTRACT)) {
        val applyMethod = caseModuleApplyMeth(cdef)
        if (applyMethod.vparamss.size > 2)
          context.error(cdef.symbol.pos, "case classes limited by implementation: maximum of 2 constructor parameter lists.")

        namer.enterSyntheticSym(applyMethod)
      }
      namer.enterSyntheticSym(caseModuleUnapplyMeth(cdef))
    }

    def addCopyMethod(cdef: ClassDef, namer: Namer) {
      caseClassCopyMeth(cdef) foreach (namer.enterSyntheticSym(_))
    }


    def typeSig(tree: Tree): Type = {

      /** For definitions, transform Annotation trees to AnnotationInfos, assign
       *  them to the sym's annotations. Type annotations: see Typer.typedAnnotated
       *  We have to parse definition annotations here (not in the typer when traversing
       *  the MemberDef tree): the typer looks at annotations of certain symbols; if
       *  they were added only in typer, depending on the compilation order, they would
       *  be visible or not
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
              annotated.setAnnotations(ainfos)
            if (annotated.isTypeSkolem)
              annotated.deSkolemize.setAnnotations(ainfos)
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

      val result =
        try {
          tree match {
            case ClassDef(_, _, tparams, impl) =>
              newNamer(context.makeNewScope(tree, sym)).classSig(tparams, impl)

            case ModuleDef(_, _, impl) =>
              val clazz = sym.moduleClass
              clazz.setInfo(newNamer(context.makeNewScope(tree, clazz)).templateSig(impl))
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
              val base = expr1.tpe
              typer.checkStable(expr1)
              if ((expr1.symbol ne null) && expr1.symbol.isRootPackage) context.error(tree.pos, "_root_ cannot be imported")
              def checkNotRedundant(pos: Position, from: Name, to: Name): Boolean = {
                if (!tree.symbol.isSynthetic &&
                    !((expr1.symbol ne null) && expr1.symbol.isInterpreterWrapper) &&
                    base.member(from) != NoSymbol) {
                  val e = context.scope.lookupEntry(to)
                  def warnRedundant(sym: Symbol) =
                    context.unit.warning(pos, "imported `"+to+
                                         "' is permanently hidden by definition of "+sym+
                                         sym.locationString)
                  if ((e ne null) && e.owner == context.scope && e.sym.exists) {
                    warnRedundant(e.sym); return false
                  } else if (context eq context.enclClass) {
                    val defSym = context.prefix.member(to) filter (
                      sym => sym.exists && context.isAccessible(sym, context.prefix, false))
                    if (defSym != NoSymbol) { warnRedundant(defSym); return false }
                  }
                }
                true
              }

              def isValidSelector(from: Name)(fun : => Unit) {
                if (from.bothNames forall (x => (base nonLocalMember x) == NoSymbol))
                  fun
              }

              def checkSelectors(selectors: List[ImportSelector]): Unit = selectors match {
                case ImportSelector(from, _, to, _) :: rest =>
                  if (from != nme.WILDCARD && base != ErrorType) {
                    isValidSelector(from) {
                      if (currentRun.compileSourceFor(expr, from))
                        return typeSig(tree)
                      // for Java code importing Scala objects
                      if (from.endsWith(nme.raw.DOLLAR))
                        isValidSelector(from.subName(0, from.length -1)) {
                          context.error(tree.pos, from.decode + " is not a member of " + expr)
                        }
                      else
                        context.error(tree.pos, from.decode + " is not a member of " + expr)
                    }

                    if (checkNotRedundant(tree.pos, from, to))
                      checkNotRedundant(tree.pos, from.toTypeName, to.toTypeName)
                  }
                  if (from != nme.WILDCARD && (rest.exists (sel => sel.name == from)))
                    context.error(tree.pos, from.decode + " is renamed twice")
                  if ((to ne null) && to != nme.WILDCARD && (rest exists (sel => sel.rename == to)))
                    context.error(tree.pos, to.decode + " appears twice as a target of a renaming")
                  checkSelectors(rest)
                case Nil =>
              }
              checkSelectors(selectors)
              transformed(tree) = treeCopy.Import(tree, expr1, selectors)
              ImportType(expr1)
          }
        } catch {
          case ex: TypeError =>
            //Console.println("caught " + ex + " in typeSig")
            typer.reportTypeError(tree.pos, ex)
            ErrorType
        }
      result match {
        case PolyType(tparams @ (tp :: _), _) if tp.owner.isTerm =>
          new DeSkolemizeMap(tparams) mapOver result
        case _ =>
          result
      }
    }

    /** Check that symbol's definition is well-formed. This means:
     *   - no conflicting modifiers
     *   - `abstract' modifier only for classes
     *   - `override' modifier never for classes
     *   - `def' modifier never for parameters of case classes
     *   - declarations only in mixins or abstract classes (when not @native)
     */
    def validate(sym: Symbol) {
      def checkNoConflict(flag1: Int, flag2: Int) {
        if (sym.hasFlag(flag1) && sym.hasFlag(flag2))
          context.error(sym.pos,
            if (flag1 == DEFERRED)
              "abstract member may not have " + Flags.flagsToString(flag2) + " modifier";
            else
              "illegal combination of modifiers: " +
              Flags.flagsToString(flag1) + " and " + Flags.flagsToString(flag2) +
              " for: " + sym);
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
      if (sym.hasFlag(OVERRIDE | ABSOVERRIDE) && !sym.hasFlag(TRAIT) && sym.isClass)
        context.error(sym.pos, "`override' modifier not allowed for classes")
      if (sym.hasFlag(OVERRIDE | ABSOVERRIDE) && sym.isConstructor)
        context.error(sym.pos, "`override' modifier not allowed for constructors")
      if (sym.hasFlag(ABSOVERRIDE) && !sym.owner.isTrait)
        context.error(sym.pos, "`abstract override' modifier only allowed for members of traits")
      if (sym.isLazy && sym.hasFlag(PRESUPER))
        context.error(sym.pos, "`lazy' definitions may not be initialized early")
      if (sym.info.typeSymbol == FunctionClass(0) &&
          sym.isValueParameter && sym.owner.isCaseClass)
        context.error(sym.pos, "pass-by-name arguments not allowed for case class parameters")
      if (sym hasFlag DEFERRED) { // virtual classes count, too
        if (sym.hasAnnotation(definitions.NativeAttr))
          sym.resetFlag(DEFERRED)
        else if (!sym.isValueParameter && !sym.isTypeParameterOrSkolem &&
          !context.tree.isInstanceOf[ExistentialTypeTree] &&
          (!sym.owner.isClass || sym.owner.isModuleClass || sym.owner.isAnonymousClass)) {
            context.error(sym.pos,
              "only classes can have declared but undefined members" + varNotice(sym))
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

  var lockedCount = 0

  def mkTypeCompleter(t: Tree)(c: Symbol => Unit) = new TypeCompleter {
    val tree = t
    override def complete(sym: Symbol) = try {
      lockedCount += 1
      c(sym)
    } finally {
      lockedCount -= 1
    }
  }

  /** A class representing a lazy type with known type parameters.
   */
  class PolyTypeCompleter(tparams: List[Tree], restp: TypeCompleter, owner: Tree, ownerSym: Symbol, ctx: Context) extends TypeCompleter {
    override val typeParams: List[Symbol]= tparams map (_.symbol) //@M
    override val tree = restp.tree
    override def complete(sym: Symbol) = try {
      lockedCount += 1
      if(ownerSym.isAbstractType) //@M an abstract type's type parameters are entered -- TODO: change to isTypeMember ?
        newNamer(ctx.makeNewScope(owner, ownerSym)).enterSyms(tparams) //@M
      restp.complete(sym)
    } finally {
      lockedCount -= 1
    }
  }

  /** The symbol that which this accessor represents (possibly in part).
   *  This is used for error messages, where we want to speak in terms
   *  of the actual declaration or definition, not in terms of the generated setters
   *  and getters */
  def underlying(member: Symbol): Symbol =
    if (member.hasAccessorFlag) {
      if (member.isDeferred) {
        val getter = if (member.isSetter) member.getter(member.owner) else member
        val result = getter.owner.newValue(getter.pos, getter.name.toTermName)
          .setInfo(getter.tpe.resultType)
          .setFlag(DEFERRED)
        if (getter.setter(member.owner) != NoSymbol) result.setFlag(MUTABLE)
        result
      } else member.accessed
    } else member

  /**
   * Finds the companion module of a class symbol. Calling .companionModule
   * does not work for classes defined inside methods.
   */
  def companionModuleOf(clazz: Symbol, context: Context): Symbol = {
    try {
      var res = clazz.companionModule
      if (res == NoSymbol)
        res = context.lookup(clazz.name.toTermName, clazz.owner).suchThat(sym =>
          sym.hasModuleFlag && sym.isCoDefinedWith(clazz))
        res
    } catch {
      case e: InvalidCompanions =>
        context.error(clazz.pos, e.getMessage)
        NoSymbol
    }
  }

  def companionClassOf(module: Symbol, context: Context): Symbol = {
    try {
      var res = module.companionClass
      if (res == NoSymbol)
        res = context.lookup(module.name.toTypeName, module.owner).suchThat(_.isCoDefinedWith(module))
      res
    } catch {
      case e: InvalidCompanions =>
        context.error(module.pos, e.getMessage)
        NoSymbol
    }
  }

  def companionSymbolOf(sym: Symbol, context: Context) =
    if (sym.isTerm) companionClassOf(sym, context)
    else if (sym.isClass) companionModuleOf(sym, context)
    else NoSymbol

  /** An explanatory note to be added to error messages
   *  when there's a problem with abstract var defs */
  def varNotice(sym: Symbol): String =
    if (underlying(sym).isVariable)
      "\n(Note that variables need to be initialized to be defined)"
    else ""
}

