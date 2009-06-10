/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import scala.collection.mutable.HashMap
import scala.tools.nsc.util.Position
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
        mapOver(rawTypeRef(NoPrefix, sym.deSkolemize, args))
/*
      case PolyType(tparams1, restpe) =>
        new DeSkolemizeMap(tparams1 ::: tparams).mapOver(tp)
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = List.mapConserve(parents)(this)
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
  // is stored in this map.
  private[typechecker] val caseClassOfModuleClass = new HashMap[Symbol, ClassDef]

  // Default getters of constructors are added to the companion object in the
  // typeCompleter of the constructor (methodSig). To compute the signature,
  // we need the ClassDef. To create and enter the symbols into the companion
  // object, we need the templateNamer of that module class.
  // This map is extended during naming of classes, the Namer is added in when
  // it's available, i.e. in the type completer (templateSig) of the module class.
  private[typechecker] val classAndNamerOfModule = new HashMap[Symbol, (ClassDef, Namer)]

  def resetNamer() {
    caseClassOfModuleClass.clear
    classAndNamerOfModule.clear
  }

  abstract class Namer(val context: Context) {

    val typer = newTyper(context)

    def setPrivateWithin[Sym <: Symbol](tree: Tree, sym: Sym, mods: Modifiers): Sym = {
      if (!mods.privateWithin.isEmpty)
        sym.privateWithin = typer.qualifyingClassContext(tree, mods.privateWithin, true).owner
      sym
    }

    def inConstructorFlag: Long =
      if (context.owner.isConstructor && !context.inConstructorSuffix || context.owner.isEarly) INCONSTRUCTOR
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
      if (sym.owner.isPackageClass &&
          (sym.linkedSym.rawInfo.isInstanceOf[loaders.SymbolLoader] ||
           sym.linkedSym.rawInfo.isComplete && runId(sym.validTo) != currentRunId))
        // pre-set linked symbol to NoType, in case it is not loaded together with this symbol.
        sym.linkedSym.setInfo(NoType)
      sym
    }

    private def isTemplateContext(context: Context): Boolean = context.tree match {
      case Template(_, _, _) => true
      case Import(_, _) => isTemplateContext(context.outer)
      case _ => false
    }

    private var innerNamerCache: Namer = null
    protected def makeConstructorScope(classContext : Context) : Context = {
      val outerContext = classContext.outer.outer
      outerContext.makeNewScope(outerContext.tree, outerContext.owner)(Constructor1ScopeKind)
    }

    def namerOf(sym: Symbol): Namer = {

      def innerNamer: Namer = {
        if (innerNamerCache eq null)
          innerNamerCache =
            if (!isTemplateContext(context)) this
            else newNamer(context.make(context.tree, context.owner, scopeFor(context.tree, InnerScopeKind)))
        innerNamerCache
      }

      def primaryConstructorParamNamer: Namer = { //todo: can we merge this with SCCmode?
        val classContext = context.enclClass
        val paramContext = makeConstructorScope(classContext)
        val unsafeTypeParams = context.owner.unsafeTypeParams
        unsafeTypeParams foreach(sym => paramContext.scope.enter(sym))
        newNamer(paramContext)
      }
      if (sym.isTerm) {
        if (sym.hasFlag(PARAM) && sym.owner.isPrimaryConstructor)
          primaryConstructorParamNamer
        else if (sym.hasFlag(PARAMACCESSOR))
          primaryConstructorParamNamer
        else innerNamer
      } else innerNamer
    }

    protected def conflict(newS : Symbol, oldS : Symbol) : Boolean = {
      (!oldS.isSourceMethod ||
        nme.isSetterName(newS.name) ||
        newS.owner.isPackageClass) &&
        !((newS.owner.isTypeParameter || newS.owner.isAbstractType) &&
          newS.name.length==1 && newS.name(0)=='_') //@M: allow repeated use of `_' for higher-order type params
    }

    // IDE hook
    protected def setInfo[Sym <: Symbol](sym : Sym)(tpe : LazyType) : Sym = sym.setInfo(tpe)

    private def doubleDefError(pos: Position, sym: Symbol) {
      context.error(pos,
        sym.name.toString() + " is already defined as " +
        (if (sym.hasFlag(SYNTHETIC))
          "(compiler-generated) "+ (if (sym.isModule) "case class companion " else "")
         else "") +
        (if (sym.hasFlag(CASE)) "case class " + sym.name else sym.toString()))
    }

    private def inCurrentScope(m: Symbol): Boolean = {
      if (context.owner.isClass) context.owner == m.owner
      else m.owner.isClass && context.scope == m.owner.info.decls
    }

    def enterInScope(sym: Symbol): Symbol = enterInScope(sym, context.scope)

    def enterInScope(sym: Symbol, scope: Scope): Symbol = {
      // allow for overloaded methods
      if (!(sym.isSourceMethod && sym.owner.isClass && !sym.owner.isPackageClass)) {
        var prev = scope.lookupEntryWithContext(sym.name)(context.owner);
        if ((prev ne null) && prev.owner == scope && conflict(sym, prev.sym)) {
           doubleDefError(sym.pos, prev.sym)
           sym setInfo ErrorType
           scope unlink prev.sym // let them co-exist...
           scope enter sym
        } else scope enter sym
      } else scope enter sym
    }

    def enterPackageSymbol(pos: Position, name: Name): Symbol = {
      val (cscope, cowner) =
        if (context.owner == EmptyPackageClass) (RootClass.info.decls, RootClass)
        else (context.scope, context.owner)
      val p: Symbol = cscope.lookupWithContext(name)(context.owner)
      if (p.isPackage && cscope == p.owner.info.decls) {
        p
      } else {
        val pkg = cowner.newPackage(pos, name)
        pkg.moduleClass.setInfo(new PackageClassInfoType(newScope, pkg.moduleClass, null))
        pkg.setInfo(pkg.moduleClass.tpe)
        enterInScope(pkg, cscope)
      }
    }

    def enterClassSymbol(tree : ClassDef): Symbol = {
      var c: Symbol = context.scope.lookupWithContext(tree.name)(context.owner);
      if (c.isType && c.owner.isPackageClass && context.scope == c.owner.info.decls && !currentRun.compiles(c)) {
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
          assert(!currentRun.compiles(clazz) || clazz.sourceFile == currentRun.symSource(c));
          currentRun.symSource(c) = clazz.sourceFile
        }
      }
      assert(c.name.toString.indexOf('(') == -1)
      c
    }

    /** Enter a module symbol. The tree parameter can be either a module definition
     *  or a class definition */
    def enterModuleSymbol(tree : ModuleDef): Symbol = {
      // .pos, mods.flags | MODULE | FINAL, name
      var m: Symbol = context.scope.lookupWithContext(tree.name)(context.owner)
      val moduleFlags = tree.mods.flags | MODULE | FINAL
      if (m.isModule && !m.isPackage && inCurrentScope(m) &&
          (!currentRun.compiles(m) || (m hasFlag SYNTHETIC))) {
        updatePosFlags(m, tree.pos, moduleFlags)
        setPrivateWithin(tree, m, tree.mods)
        context.unit.synthetics -= m
      } else {
        m = context.owner.newModule(tree.pos, tree.name)
        m.setFlag(moduleFlags)
        m = setPrivateWithin(tree, m, tree.mods)
        m = enterInScope(m)

        m.moduleClass.setFlag(moduleClassFlags(moduleFlags))
        setPrivateWithin(tree, m.moduleClass, tree.mods)
      }
      if (m.owner.isPackageClass) {
        m.moduleClass.sourceFile = context.unit.source.file
        currentRun.symSource(m) = m.moduleClass.sourceFile
      }
      m
    }

    def enterSyms(trees: List[Tree]): Namer = {
      var namer : Namer = this
      for (tree <- trees) {
        val txt = namer.enterSym(tree)
        if (!(txt eq namer.context)) namer = newNamer(txt)
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
      val m: Symbol = context.scope.lookupWithContext(tree.name.toTermName)(context.owner).filter(! _.isSourceMethod)
      if (m.isModule && inCurrentScope(m) && currentRun.compiles(m)) m
      else enterSyntheticSym(creator)
    }

    def enterSym(tree: Tree): Context = try {

      def finishWith(tparams: List[TypeDef]) {
        val sym = tree.symbol
        if (settings.debug.value) log("entered " + sym + " in " + context.owner + ", scope-id = " + context.scope.hashCode());
        var ltype = namerOf(sym).typeCompleter(tree)
        if (!tparams.isEmpty) {
          //@M! TypeDef's type params are handled differently
          //@M e.g., in [A[x <: B], B], A and B are entered first as both are in scope in the definition of x
          //@M x is only in scope in `A[x <: B]'
          if(!sym.isAbstractType) //@M TODO: change to isTypeMember ?
            newNamer(context.makeNewScope(tree, sym)(FinishWithScopeKind)).enterSyms(tparams)

          ltype = new PolyTypeCompleter(tparams, ltype, tree, sym, context) //@M
          if (sym.isTerm) skolemize(tparams)
        }
        if ((sym.name == nme.copy || sym.name.startsWith(nme.copy + "$default$")) &&
            sym.hasFlag(SYNTHETIC)) {
          // the 'copy' method of case classes needs a special type completer to make bug0054.scala (and others)
          // work. the copy method has to take exactly the same parameter types as the primary constructor.
          setInfo(sym)(mkTypeCompleter(tree)(copySym => {
            val constrType = copySym.owner.primaryConstructor.tpe
            val subst = new SubstSymMap(copySym.owner.typeParams, tparams map (_.symbol))
            for ((params, cparams) <- tree.asInstanceOf[DefDef].vparamss.zip(constrType.paramss);
                 (param, cparam) <- params.zip(cparams)) {
              // need to clone the type cparam.tpe??? problem is: we don't have the new owner yet (the new param symbol)
              param.tpt.setType(subst(cparam.tpe))
              () // @LUC TODO workaround for #1996
            }
            ltype.complete(sym)
          }))
        } else setInfo(sym)(ltype)
      }
      def finish = finishWith(List())

      if (tree.symbol == NoSymbol) {
        val owner = context.owner
        tree match {
          case PackageDef(name, stats) =>
            tree.symbol = enterPackageSymbol(tree.pos, name)
            val namer = newNamer(
                context.make(tree, tree.symbol.moduleClass, tree.symbol.info.decls))
            namer.enterSyms(stats)
          case tree @ ClassDef(mods, name, tparams, impl) =>
            tree.symbol = enterClassSymbol(tree)
            finishWith(tparams)
            if ((mods.flags & CASE) != 0) {
              val m = ensureCompanionObject(tree, caseModuleDef(tree))
              caseClassOfModuleClass(m.moduleClass) = tree
            }
            val constrs = impl.body filter {
              case DefDef(_, name, _, _, _, _) => name == nme.CONSTRUCTOR
              case _ => false
            }
            val hasDefault = constrs.exists(c => {
              val DefDef(_, _, _, vparamss, _, _) = c
              vparamss.exists(_.exists(_.mods hasFlag DEFAULTPARAM))
            })
            if (hasDefault) {
              val m = ensureCompanionObject(tree, companionModuleDef(tree, List(gen.scalaScalaObjectConstr)))
              classAndNamerOfModule(m) = (tree, null)
            }
          case tree @ ModuleDef(mods, name, _) =>
            tree.symbol = enterModuleSymbol(tree)
            tree.symbol.moduleClass.setInfo(namerOf(tree.symbol).moduleClassTypeCompleter((tree)))
            finish
            if (tree.symbol.name == nme.PACKAGEkw) {
              loaders.openPackageModule(tree.symbol)
            }

          case vd @ ValDef(mods, name, tp, rhs) =>
            if ((!context.owner.isClass ||
                 (mods.flags & (PRIVATE | LOCAL)) == (PRIVATE | LOCAL) ||
                 name.endsWith(nme.OUTER, nme.OUTER.length) ||
                 context.unit.isJava) &&
                (mods.flags & LAZY) == 0) {
              tree.symbol = enterInScope(owner.newValue(tree.pos, name)
                .setFlag(mods.flags))
              finish
            } else {
              // add getter and possibly also setter
              val accflags: Long = ACCESSOR |
                (if ((mods.flags & MUTABLE) != 0) mods.flags & ~MUTABLE & ~PRESUPER
                 else mods.flags & ~PRESUPER | STABLE)
              if (nme.isSetterName(name))
                context.error(tree.pos, "Names of vals or vars may not end in `_='")
              // .isInstanceOf[..]: probably for (old) IDE hook. is this obsolete?
              val getter = enterNewMethod(tree, name, accflags, mods).asInstanceOf[TermSymbol]
              setInfo(getter)(namerOf(getter).getterTypeCompleter(vd))
              if ((mods.flags & MUTABLE) != 0) {
                val setter = enterNewMethod(tree, nme.getterToSetter(name),
                                            accflags & ~STABLE & ~CASEACCESSOR,
                                            mods).asInstanceOf[TermSymbol]
                setInfo(setter)(namerOf(setter).setterTypeCompleter(vd))
              }
              tree.symbol =
                if (!mods.hasFlag(DEFERRED)) {
                  var vsym =
                    if (!context.owner.isClass) {
                      assert((mods.flags & LAZY) != 0) // if not a field, it has to be a lazy val
                      owner.newValue(tree.pos, name + "$lzy" ).setFlag(mods.flags | MUTABLE)
                    } else {
                      owner.newValue(tree.pos, nme.getterToLocal(name))
                        .setFlag(mods.flags & FieldFlags | PRIVATE | LOCAL |
                                 (if (mods.hasFlag(LAZY)) MUTABLE else 0))
                    }
                  vsym = enterInScope(vsym).asInstanceOf[TermSymbol]
                  setInfo(vsym)(namerOf(vsym).typeCompleter(tree))
                  if ((mods.flags & LAZY) != 0)
                    vsym.setLazyAccessor(getter)
                  vsym
                } else getter
              addBeanGetterSetter(vd, getter)
            }
          case DefDef(mods, nme.CONSTRUCTOR, tparams, _, _, _) =>
            var sym = owner.newConstructor(tree.pos).setFlag(mods.flags | owner.getFlag(ConstrFlags))
            setPrivateWithin(tree, sym, mods)
            tree.symbol = enterInScope(sym)
            finishWith(tparams)
          case DefDef(mods, name, tparams, _, _, _) =>
            tree.symbol = enterNewMethod(tree, name, mods.flags, mods)
            finishWith(tparams)
          case TypeDef(mods, name, tparams, _) =>
            var flags: Long = mods.flags
            if ((flags & PARAM) != 0) flags |= DEFERRED
            var sym = new TypeSymbol(owner, tree.pos, name).setFlag(flags)
            setPrivateWithin(tree, sym, mods)
            tree.symbol = enterInScope(sym)
            finishWith(tparams)
          case DocDef(_, defn) =>
            enterSym(defn)
          case imp @ Import(_, _) =>
            tree.symbol = NoSymbol.newImport(tree.pos)
            setInfo(tree.symbol)(namerOf(tree.symbol).typeCompleter(tree))
            return (context.makeNewImport(imp))
          case _ =>
        }
      }
      this.context
    } catch {
      case ex: TypeError =>
        //Console.println("caught " + ex + " in enterSym")//DEBUG
        typer.reportTypeError(tree.pos, ex)
        this.context
    }

    def enterSyntheticSym(tree: Tree): Symbol = {
      enterSym(tree)
      context.unit.synthetics(tree.symbol) = tree
      tree.symbol
    }

    def enterNewMethod(tree: Tree, name: Name, flags: Long, mods: Modifiers) = {
      val sym = context.owner.newMethod(tree.pos, name).setFlag(flags)
      setPrivateWithin(tree, sym, mods)
      enterInScope(sym)
    }

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
        else if (mods hasFlag PRIVATE)
          // avoids name clashes with private fields in traits
          context.error(vd.pos, "`BeanProperty' annotation can only be applied "+
                                "to non-private fields")
        else {
          val flags = mods.flags & (DEFERRED | OVERRIDE | STATIC)
          val beanName = name(0).toString.toUpperCase + name.subName(1, name.length)

          val getterName = if (hasBoolBP) "is" + beanName
                           else "get" + beanName
          val getterMods = Modifiers(flags, mods.privateWithin,
                                     mods.annotations map (_.duplicate))
          val beanGetterDef = atPos(vd.pos) {
            DefDef(getterMods, getterName, Nil, List(Nil), tpt.duplicate,
                   if (mods hasFlag DEFERRED) EmptyTree
                   else Select(This(getter.owner.name), name)) }
          enterSyntheticSym(beanGetterDef)

          if (mods hasFlag MUTABLE) {
            // can't use "enterSyntheticSym", because the parameter type is not yet
            // known. instead, uses the same machinery as for the non-bean setter:
            // create and enter the symbol here, add the tree in Typer.addGettterSetter.
            val setterName = "set" + beanName
            val setter = enterNewMethod(vd, setterName, flags, mods).asInstanceOf[TermSymbol]
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
      if ((sym.isAliasType || sym.isAbstractType) && !(sym hasFlag PARAM) &&
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
      sym.setInfo(PolyType(List(), tp))
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

    private def widenIfNotFinal(sym: Symbol, tpe: Type, pt: Type): Type = {
      val getter =
        if (sym.isValue && sym.owner.isClass && (sym hasFlag PRIVATE))
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
      if ((sym.isVariable || sym.isMethod && !(sym hasFlag ACCESSOR)))
        if (tpe2 <:< pt) tpe2 else tpe1
      else if (isHidden(tpe)) tpe2
      else if (!(sym hasFlag FINAL)) tpe1
      else tpe
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
          self.tpt.tpe = NoType
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
          val earlyContext = context.outer.makeNewScope(context.tree, context.outer.owner.newLocalDummy(templ.pos))(InnerScopeKind)
          newNamer(earlyContext).enterSyms(earlyTypes)
          newTyper(earlyContext).typedStats(earlyTypes, context.owner)

          val parentContext = context.makeNewScope(context.tree, context.owner)(InnerScopeKind)
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
      val decls = newClassScope(clazz)
//      for (etdef <- earlyTypes) decls enter etdef.symbol
      val templateNamer = newNamer(context.make(templ, clazz, decls))
        .enterSyms(templ.body)

      /* add overridden virtuals to parents
      val overridden = clazz.overriddenVirtuals
      if (!overridden.isEmpty)
        parents = parents ::: ( overridden map (
          sym => TypeRef(clazz.owner.thisType, sym, clazz.typeParams map (_.tpe))))
      println("Parents of "+clazz+":"+parents)

      // check that virtual classses are only defined as members of templates
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
      Namers.this.caseClassOfModuleClass get clazz match {
        case Some(cdef) =>
          addApplyUnapply(cdef, templateNamer)
          caseClassOfModuleClass -= clazz
        case None =>
      }

      // add the copy method to case classes; this needs to be done here, not in SyntheticMethods, because
      // the namer phase must traverse this copy method to create default getters for its parameters.
      Namers.this.caseClassOfModuleClass get clazz.linkedModuleOfClass.moduleClass match {
        case Some(cdef) =>
          def hasCopy(decls: Scope) = {
            decls.elements exists (_.name == nme.copy)
          }
          if (!hasCopy(decls) &&
              !parents.exists(p => hasCopy(p.typeSymbol.info.decls)) &&
              !parents.flatMap(_.baseClasses).removeDuplicates.exists(bc => hasCopy(bc.info.decls)))
            addCopyMethod(cdef, templateNamer)
        case None =>
      }

      // if default getters (for constructor defaults) need to be added to that module,
      // here's the namer to use
      val module = clazz.sourceModule
      if (classAndNamerOfModule contains module) {
        val (cdef, _) = classAndNamerOfModule(module)
        classAndNamerOfModule(module) = (cdef, templateNamer)
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

      if (tpt.isEmpty && meth.name == nme.CONSTRUCTOR) {
        tpt.tpe = context.enclClass.owner.tpe
        tpt setPos meth.pos
      }

      if (onlyPresentation && methodArgumentNames != null)
        methodArgumentNames(meth) = vparamss.map(_.map(_.symbol));

      def convertToDeBruijn(vparams: List[Symbol], level: Int): TypeMap = new TypeMap {
        def debruijnFor(param: Symbol) =
          DeBruijnIndex(level, vparams indexOf param)
        def apply(tp: Type) = {
          tp match {
            case SingleType(_, sym) =>
              if (settings.Xexperimental.value && sym.owner == meth && (vparams contains sym)) {
/*
                if (sym hasFlag IMPLICIT) {
                  context.error(sym.pos, "illegal type dependence on implicit parameter")
                  ErrorType
                } else
*/
                debruijnFor(sym)
              } else tp
            case MethodType(params, restpe) =>
              val params1 = this.mapOver(params)
              val restpe1 = convertToDeBruijn(vparams, level + 1)(restpe)
              if ((params1 eq params) && (restpe1 eq restpe)) tp
              else copyMethodType(tp, params1, restpe1)
            case _ =>
              mapOver(tp)
          }
        }

        // AnnotatedTypes can contain trees in the AnnotationArguments. When accessing a
        // parameter in an annotation, set the type of the Ident to the DeBruijnIndex
        object treeTrans extends TypeMapTransformer {
          override def transform(tree: Tree): Tree =
            tree match {
              case Ident(name) if (vparams contains tree.symbol) =>
                val dtpe = debruijnFor(tree.symbol)
                val dsym =
                  newLocalDummy(context.owner, tree.symbol.pos)
                  .newValue(tree.symbol.pos, name)

                dsym.setFlag(PARAM)
                dsym.setInfo(dtpe)
                Ident(name).setSymbol(dsym).copyAttrs(tree).setType(dtpe)
              case tree => super.transform(tree)
            }
        }

        // for type annotations (which may contain trees)
        override def mapOver(arg: Tree) = Some(treeTrans.transform(arg))
      }

      val checkDependencies: TypeTraverser = new TypeTraverser {
        def traverse(tp: Type) = {
          tp match {
            case SingleType(_, sym) =>
              if (sym.owner == meth && (vparamSymss exists (_ contains sym)))
                context.error(
                  sym.pos,
                  "illegal dependent method type"+
                  (if (settings.Xexperimental.value)
                     ": parameter appears in the type of another parameter in the same section or an earlier one"
                   else ""))
            case _ =>
              mapOver(tp)
          }
          this
        }
      }

      /** Called for all value parameter lists, right to left
       *  @param vparams the symbols of one parameter list
       *  @param restpe  the result type (possibly a MethodType)
       */
      def makeMethodType(vparams: List[Symbol], restpe: Type) = {
        // new dependent method types: probably OK already, since 'enterValueParams' above
        // enters them in scope, and all have a lazy type. so they may depend on other params. but: need to
        // check that params only depend on ones in earlier sections, not the same. (done by checkDependencies,
        // so re-use / adapt that)
        val params = vparams map (vparam =>
          if (meth hasFlag JAVA) vparam.setInfo(objToAny(vparam.tpe)) else vparam)
        val restpe1 = convertToDeBruijn(vparams, 1)(restpe) // new dependent types: replace symbols in restpe with the ones in vparams
        if (!vparams.isEmpty && vparams.head.hasFlag(IMPLICIT))
          ImplicitMethodType(params, restpe1)
        else if (meth hasFlag JAVA) JavaMethodType(params, restpe1)
        else MethodType(params, restpe1)
      }

      def thisMethodType(restpe: Type) =
        polyType(
          tparamSyms, // deSkolemized symbols
          if (vparamSymss.isEmpty) PolyType(List(), restpe)
          // vparamss refer (if they do) to skolemized tparams
          else checkDependencies((vparamSymss :\ restpe) (makeMethodType)))

      var resultPt = if (tpt.isEmpty) WildcardType else typer.typedType(tpt).tpe
      val site = meth.owner.thisType

      def overriddenSymbol = intersectionType(meth.owner.info.parents).member(meth.name).filter(sym => {
        // luc: added .syubstSym from skolemized to deSkolemized
        // site.memberType(sym): PolyType(tparams, MethodType(..., ...)) ==> all references to tparams are deSkolemized
        // thisMethodType: tparams in PolyType are deSkolemized, the references in the MethodTypes are skolemized. ==> the two didn't match
        // for instance, B.foo would not override A.foo, and the default on parameter b would not be inherited
        //   class A { def foo[T](a: T)(b: T = a) = a }
        //   class B extends A { override def foo[U](a: U)(b: U) = b }
        //   (new B).foo(1)()
        sym != NoSymbol && (site.memberType(sym) matches thisMethodType(resultPt).substSym(tparams map (_.symbol), tparamSyms))
      })

      // fill in result type and parameter types from overridden symbol if there is a unique one.
      if (meth.owner.isClass && (tpt.isEmpty || vparamss.exists(_.exists(_.tpt.isEmpty)))) {
        // try to complete from matching definition in base type
        for (vparams <- vparamss; vparam <- vparams)
          if (vparam.tpt.isEmpty) vparam.symbol setInfo WildcardType
        val overridden = overriddenSymbol
        if (overridden != NoSymbol && !(overridden hasFlag OVERLOADED)) {
          resultPt = site.memberType(overridden) match {
            case PolyType(tparams, rt) => rt.substSym(tparams, tparamSyms)
            case mt => mt
          }

          for (vparams <- vparamss) {
            var pfs = resultPt.paramTypes
            for (vparam <- vparams) {
              if (vparam.tpt.isEmpty) {
                vparam.tpt.tpe = pfs.head
                vparam.tpt setPos vparam.pos
                vparam.symbol setInfo pfs.head
              }
              pfs = pfs.tail
            }
            resultPt = resultPt.resultType
          }
          resultPt match {
            case PolyType(List(), rtpe) => resultPt = rtpe
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
        vparam.tpt.tpe = ErrorType
      }


      // add the getter methods for default arguments.

      /** Adding the "override" and "defaultparam" (for inherited defaults) flags
       *  has to be done here. Typer is too late, if an inherited default is used
       *  before the method is typechecked, the corresponding param would not yet
       *  have the "defaultparam" flag.
       */
      val isConstr = meth.isConstructor
      val overridden = if (isConstr || !meth.owner.isClass) NoSymbol
                       else overriddenSymbol
      val overrides = overridden != NoSymbol && !(overridden hasFlag OVERLOADED)
      // value parameters of the base class (whose defaults might be overridden)
      var baseParamss = overridden.tpe.paramss
        // match empty and missing parameter list
        if (vparamss.isEmpty && baseParamss == List(Nil)) baseParamss = Nil
        if (vparamss == List(Nil) && baseParamss.isEmpty) baseParamss = List(Nil)
        assert(!overrides || vparamss.length == baseParamss.length, ""+ meth.fullNameString + ", "+ overridden.fullNameString)

      var ownerNamer: Option[Namer] = None
      var moduleNamer: Option[(ClassDef, Namer)] = None

      var posCounter = 1

      // for each value parameter, create the getter method if it has a default argument. previous
      // denotes the parameter lists which are on the left side of the current one. these get added
      // to the default getter. Example: "def foo(a: Int)(b: Int = a)" gives "foo$default$1(a: Int) = a"
      (List[List[ValDef]]() /: (vparamss))((previous: List[List[ValDef]], vparams: List[ValDef]) => {
        assert(!overrides || vparams.length == baseParamss.head.length, ""+ meth.fullNameString + ", "+ overridden.fullNameString)
        var baseParams = if (overrides) baseParamss.head else Nil
        for (vparam <- vparams) {
          val sym = vparam.symbol
          // true if the corresponding parameter of the base class has a default argument
          val baseHasDefault = overrides && (baseParams.head hasFlag DEFAULTPARAM)
          if (sym hasFlag DEFAULTPARAM) {
        	// generate a default getter for that argument
            val oflag = if (baseHasDefault) OVERRIDE else 0
            val name = (if (isConstr) "init" else meth.name) +"$default$"+ posCounter

            // Create trees for the defaultGetter. Uses tools from Unapplies.scala
            var deftParams = tparams map copyUntyped[TypeDef]
            val defvParamss = previous map (_.map(p => {
              // in the default getter, remove the default parameter
              val p1 = atPos(p.pos) { ValDef(p.mods &~ DEFAULTPARAM, p.name, p.tpt.duplicate, EmptyTree) }
              UnTyper.traverse(p1)
              p1
            }))
            // let the compiler infer the return type of the defaultGetter. needed in "foo[T](a: T = 1)"
            val defTpt = TypeTree()
            val defRhs = copyUntyped(vparam.rhs)

            val parentNamer = if (isConstr) {
              val (cdef, nmr) = moduleNamer.getOrElse {
                val module = meth.owner.linkedModuleOfClass
                module.initialize // call type completer (typedTemplate), adds the
                                  // module's templateNamer to classAndNamerOfModule
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

            val defaultTree = atPos(vparam.pos) {
              DefDef(
                Modifiers(meth.flags & (PRIVATE | PROTECTED | FINAL)) | SYNTHETIC | DEFAULTPARAM | oflag,
                name, deftParams, defvParamss, defTpt, defRhs)
            }
            meth.owner.resetFlag(INTERFACE)
            val default = parentNamer.enterSyntheticSym(defaultTree)
            sym.defaultGetter = default
          } else if (baseHasDefault) {
            // the parameter does not have a default itself, but the corresponding parameter
            // in the base class does.
            sym.setFlag(DEFAULTPARAM)
            sym.defaultGetter = baseParams.head.defaultGetter
          }
          posCounter += 1
          if (overrides) baseParams = baseParams.tail
        }
        if (overrides) baseParamss = baseParamss.tail
        previous ::: List(vparams)
      })

      thisMethodType(
        if (tpt.isEmpty) {
          // replace deSkolemized symbols with skolemized ones (for resultPt computed by looking at overridden symbol, right?)
          val pt = resultPt.substSym(tparamSyms, tparams map (_.symbol))
          // compute result type from rhs
          tpt.tpe = widenIfNotFinal(meth, typer.computeType(rhs, pt), pt)
          tpt setPos meth.pos
          tpt.tpe
        } else typer.typedType(tpt).tpe)
    }

    //@M! an abstract type definition (abstract type member/type parameter) may take type parameters, which are in scope in its bounds
    private def typeDefSig(tpsym: Symbol, tparams: List[TypeDef], rhs: Tree) = {
      val tparamSyms = typer.reenterTypeParams(tparams) //@M make tparams available in scope (just for this abstypedef)
      val tp = typer.typedType(rhs).tpe match {
        case TypeBounds(lt, rt) if (lt.isError || rt.isError) =>
          TypeBounds(NothingClass.tpe, AnyClass.tpe)
        case tp @ TypeBounds(lt, rt) if (tpsym hasFlag JAVA) =>
          TypeBounds(lt, objToAny(rt))
        case tp =>
          tp
      }

      def verifyOverriding(other: Symbol): Boolean = {
        if(other.unsafeTypeParams.length != tparamSyms.length) {
          context.error(tpsym.pos,
              "The kind of "+tpsym.keyString+" "+tpsym.varianceString + tpsym.nameString+
              " does not conform to the expected kind of " + other.defString + other.locationString + ".")
          false
        } else true
      }

      // @M: make sure overriding in refinements respects rudimentary kinding
      // have to do this early, as otherwise we might get crashes: (see neg/bug1275.scala)
      //   suppose some parameterized type member is overridden by a type member w/o params,
      //   then appliedType will be called on a type that does not expect type args --> crash
      if (tpsym.owner.isRefinementClass &&  // only needed in refinements
          !tpsym.allOverriddenSymbols.forall{verifyOverriding(_)})
	      ErrorType
      else polyType(tparamSyms, tp)
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
      if (!(cdef.symbol hasFlag ABSTRACT))
        namer.enterSyntheticSym(caseModuleApplyMeth(cdef))
      namer.enterSyntheticSym(caseModuleUnapplyMeth(cdef))
    }

    def addCopyMethod(cdef: ClassDef, namer: Namer) {
      caseClassCopyMeth(cdef) foreach (namer.enterSyntheticSym(_))
    }

    def typeSig(tree: Tree): Type = {
      val sym: Symbol = tree.symbol
      // For definitions, transform Annotation trees to AnnotationInfos, assign
      // them to the sym's annotations. Type annotations: see Typer.typedAnnotated

      // We have to parse definition annotatinos here (not in the typer when traversing
      // the MemberDef tree): the typer looks at annotations of certain symbols; if
      // they were added only in typer, depending on the compilation order, they would
      // be visible or not
      val annotated = if (sym.isModule) sym.moduleClass else sym
      if (annotated.annotations.isEmpty) tree match {
        case defn: MemberDef =>
          val ainfos = defn.mods.annotations filter { _ != null } map { ann =>
            // need to be lazy, #1782
            LazyAnnotationInfo(() => typer.typedAnnotation(ann))
          }
          if (!ainfos.isEmpty)
            annotated.setAnnotations(ainfos)
        case _ =>
      }
      implicit val scopeKind = TypeSigScopeKind
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
              val typer1 = typer.constrTyperIf(sym.hasFlag(PARAM | PRESUPER) && sym.owner.isConstructor)
              if (tpt.isEmpty) {
                if (rhs.isEmpty) {
                  context.error(tpt.pos, "missing parameter type");
                  ErrorType
                } else {
                  tpt.tpe = widenIfNotFinal(
                    sym,
                    newTyper(typer1.context.make(vdef, sym)).computeType(rhs, WildcardType),
                    WildcardType)
                  tpt setPos vdef.pos
                  tpt.tpe
                }
              } else typer1.typedType(tpt).tpe

            case TypeDef(_, _, tparams, rhs) =>
              newNamer(context.makeNewScope(tree, sym)).typeDefSig(sym, tparams, rhs) //@M!

            case Import(expr, selectors) =>
              val expr1 = typer.typedQualifier(expr)
              val base = expr1.tpe
              typer.checkStable(expr1)
              if (expr1.symbol.isRootPackage) context.error(tree.pos, "_root_ cannot be imported")
              def checkNotRedundant(pos: Position, from: Name, to: Name): Boolean = {
                if (!tree.symbol.hasFlag(SYNTHETIC) &&
                    !((expr1.symbol ne null) && expr1.symbol.isInterpreterWrapper) &&
                    base.member(from) != NoSymbol) {
                  val e = context.scope.lookupEntryWithContext(to)(context.owner)
                  def warnRedundant(sym: Symbol) =
                    context.unit.warning(pos, "imported `"+to+
                                         "' is permanently hidden by definition of "+sym+
                                         sym.locationString)
                  if ((e ne null) && e.owner == context.scope) {
                    warnRedundant(e.sym); return false
                  } else if (context eq context.enclClass) {
                    val defSym = context.prefix.member(to) filter (
                      sym => sym.exists && context.isAccessible(sym, context.prefix, false))
                    if (defSym != NoSymbol) { warnRedundant(defSym); return false }
                  }
                }
                true
              }
              def checkSelectors(selectors: List[(Name, Name)]): Unit = selectors match {
                case (from, to) :: rest =>
                  if (from != nme.WILDCARD && base != ErrorType) {
                    if (base.member(from) == NoSymbol && base.member(from.toTypeName) == NoSymbol)
                      context.error(tree.pos, from.decode + " is not a member of " + expr);
                    if (checkNotRedundant(tree.pos, from, to))
                      checkNotRedundant(tree.pos, from.toTypeName, to.toTypeName)
                  }
                  if (from != nme.WILDCARD && (rest.exists (sel => sel._1 == from)))
                    context.error(tree.pos, from.decode + " is renamed twice");
                  if ((to ne null) && to != nme.WILDCARD && (rest exists (sel => sel._2 == to)))
                    context.error(tree.pos, to.decode + " appears twice as a target of a renaming");
                  checkSelectors(rest)
                case Nil =>
              }
              checkSelectors(selectors)
              ImportType(expr1)
          }
        } catch {
          case ex: TypeError =>
            //Console.println("caught " + ex + " in typeSig")//DEBUG
            typer.reportTypeError(tree.pos, ex)
            ErrorType
        }
      result match {
        case PolyType(tparams, restpe)
        if (!tparams.isEmpty && tparams.head.owner.isTerm ||
            // Adriaan: The added conditon below is quite a hack. It seems that HK type parameters is relying
            // on a pass that forces all infos in the type to get everything right.
            // The problem is that the same pass causes cyclic reference errors in
            // test pos/cyclics.scala. It turned out that deSkolemize is run way more often than necessary,
            // ruinning it only when needed fixes the cuclic reference errors.
            // But correcting deSkolemize broke HK types, because we don't do the traversal anymore.
            // For the moment I made a special hack to do the traversal if we have HK type parameters.
            // Maybe it's not a hack, then we need to document it better. But ideally, we should find
            // a way to deal with HK types that's not dependent on accidental side
            // effects like this.
            tparams.exists(!_.typeParams.isEmpty)) =>
          new DeSkolemizeMap(tparams) mapOver result
        case _ =>
//          println("not skolemizing "+result+" in "+context.owner)
//          new DeSkolemizeMap(List()) mapOver result
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
      if (sym.hasFlag(LAZY) && sym.hasFlag(PRESUPER))
        context.error(sym.pos, "`lazy' definitions may not be initialized early")
      if (sym.info.typeSymbol == FunctionClass(0) &&
          sym.isValueParameter && sym.owner.isClass && sym.owner.hasFlag(CASE))
        context.error(sym.pos, "pass-by-name arguments not allowed for case class parameters");
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
      checkNoConflict(PRIVATE, OVERRIDE)
      /* checkNoConflict(PRIVATE, FINAL) // can't do this because FINAL also means compile-time constant */
      checkNoConflict(ABSTRACT, FINAL)  // bug #1833
      checkNoConflict(DEFERRED, FINAL)
    }
  }

  abstract class TypeCompleter extends LazyType {
    val tree: Tree
  }

  def mkTypeCompleter(t: Tree)(c: Symbol => Unit) = new TypeCompleter {
    val tree = t
    override def complete(sym: Symbol) = c(sym)
  }

  /** A class representing a lazy type with known type parameters.
   */
  class PolyTypeCompleter(tparams: List[Tree], restp: TypeCompleter, owner: Tree, ownerSym: Symbol, ctx: Context) extends TypeCompleter {
    override val typeParams: List[Symbol]= tparams map (_.symbol) //@M
    override val tree = restp.tree
    override def complete(sym: Symbol) {
      if(ownerSym.isAbstractType) //@M an abstract type's type parameters are entered -- TODO: change to isTypeMember ?
        newNamer(ctx.makeNewScope(owner, ownerSym)(PolyTypeCompleterScopeKind)).enterSyms(tparams) //@M
      restp.complete(sym)
    }
  }

  /** The symbol that which this accessor represents (possibly in part).
   *  This is used for error messages, where we want to speak in terms
   *  of the actual declaration or definition, not in terms of the generated setters
   *  and getters */
  def underlying(member: Symbol): Symbol =
    if (member hasFlag ACCESSOR) {
      if (member.isDeferred) {
        val getter = if (member.isSetter) member.getter(member.owner) else member
        val result = getter.owner.newValue(getter.pos, getter.name)
          .setInfo(getter.tpe.resultType)
          .setFlag(DEFERRED)
        if (getter.setter(member.owner) != NoSymbol) result.setFlag(MUTABLE)
        result
      } else member.accessed
    } else member

  /** An explanatory note to be added to error messages
   *  when there's a problem with abstract var defs */
  def varNotice(sym: Symbol): String =
    if (underlying(sym).isVariable)
      "\n(Note that variables need to be initialized to be defined)"
    else ""
}

