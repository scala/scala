/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

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

  /** Convert to corresponding type parameters all skolems which satisfy one
   *  of the following two conditions:
   *  1. The skolem is a parameter of a class or alias type
   *  2. The skolem is a method parameter which appears in parameter `tparams'
   */
  class DeSkolemizeMap(tparams: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args)
      if (sym.isTypeSkolem && (tparams contains sym.deSkolemize)) =>
        mapOver(rawTypeRef(NoPrefix, sym.deSkolemize, args))
      case PolyType(tparams1, restpe) =>
        new DeSkolemizeMap(tparams1 ::: tparams).mapOver(tp)
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = List.mapConserve(parents)(this)
        if (parents1 eq parents) tp else ClassInfoType(parents1, decls, clazz)
      case _ =>
        mapOver(tp)
    }
  }
  /** overridden by IDE to not manually enter value parameters */
  protected final def doEnterValueParams = true // !inIDE;
  // can't use the other ones.
  protected def newDecls(classSym : Symbol) : Scope = newClassScope

  private class NormalNamer(context : Context) extends Namer(context)
  def newNamer(context : Context) : Namer = new NormalNamer(context)

  abstract class Namer(val context: Context) {

    val typer = newTyper(context)

    def setPrivateWithin[Sym <: Symbol](tree: Tree, sym: Sym, mods: Modifiers): Sym = {
      if (!mods.privateWithin.isEmpty)
        sym.privateWithin = typer.qualifyingClassContext(tree, mods.privateWithin).owner
      sym
    }

    def updatePosFlags(sym: Symbol, pos: Position, flags: Long): Symbol = {
      if (settings.debug.value) log("overwriting " + sym)
      val lockedFlag = sym.flags & LOCKED
      sym.reset(NoType)
      sym setPos pos
      sym.flags = flags | lockedFlag
      if (sym.isModule && sym.moduleClass != NoSymbol)
        updatePosFlags(sym.moduleClass, pos, (flags & ModuleToClassFlags) | MODULE | FINAL)
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
      outerContext.makeNewScope(outerContext.tree, outerContext.owner)
    }

    def namerOf(sym: Symbol): Namer = {

      def innerNamer: Namer = {
        if (innerNamerCache eq null)
          innerNamerCache =
            if (!isTemplateContext(context)) this
            else newNamer(context.make(context.tree, context.owner, scopeFor(context.tree)))
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
        else if (sym.hasFlag(PARAMACCESSOR) && !inIDE)
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
    protected def setInfo[Sym <: Symbol](sym : Sym)(tpe : LazyType) : Sym = sym.setInfo(tpe)
    private def doubleDefError(pos: Position, sym: Symbol) {
      context.error(pos,
        sym.name.toString() + " is already defined as " +
        (if (sym.hasFlag(CASE)) "case class " + sym.name else sym.toString()))
    }

    def enterInScope(sym: Symbol): Symbol = {
      // allow for overloaded methods
      if (!(sym.isSourceMethod && sym.owner.isClass && !sym.owner.isPackageClass)) {
        val prev = context.scope.lookupEntry(sym.name);
        if ((prev ne null) && prev.owner == context.scope && conflict(sym, prev.sym)) {
           doubleDefError(sym.pos, prev.sym)
           if (!inIDE) sym setInfo ErrorType // don't do this in IDE for stability
           if (!inIDE) context.scope unlink prev.sym // let them co-exist...
           context.scope enter sym
        } else context.scope enter sym
      } else context.scope enter sym
    }

    def enterPackageSymbol(pos: Position, name: Name): Symbol = {
      val cscope = if (context.owner == EmptyPackageClass) RootClass.info.decls
                   else context.scope
      val p: Symbol = cscope.lookup(name)
      if (p.isPackage && cscope == p.owner.info.decls) {
        p
      } else {
        val cowner = if (context.owner == EmptyPackageClass) RootClass else context.owner
        val pkg = cowner.newPackage(pos, name)
        // IDE: newScope should be ok because packages are never destroyed.
        if (inIDE) assert(pkg.moduleClass.rawInfoSafe.isEmpty  || !pkg.moduleClass.rawInfo.isComplete)
        pkg.moduleClass.setInfo(new PackageClassInfoType(newScope, pkg.moduleClass, null))
        pkg.setInfo(pkg.moduleClass.tpe)
        enterInScope(pkg)
      }
    }

    def inConstructorFlag: Long =
      if (context.owner.isConstructor && !context.inConstructorSuffix || context.owner.isEarly) INCONSTRUCTOR
      else 0l

    def enterClassSymbol(tree : ClassDef): Symbol = {
      var c: Symbol = context.scope.lookup(tree.name);
      if (!inIDE && c.isType && context.scope == c.owner.info.decls && !currentRun.compiles(c)) {
        updatePosFlags(c, tree.pos, tree.mods.flags)
        setPrivateWithin(tree, c, tree.mods)
      } else {
        //if (inIDE && c != NoSymbol) currentRun.compiles(c) // in IDE, will unload/save non-source symbol
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
          assert(inIDE || !currentRun.compiles(clazz) || clazz.sourceFile == currentRun.symSource(c));
          currentRun.symSource(c) = clazz.sourceFile
        }
      }
      assert(c.name.toString.indexOf('(') == -1)
      c
    }
    def enterModuleSymbol(tree : ModuleDef): Symbol = {
      // .pos, mods.flags | MODULE | FINAL, name
      assert(tree.name.isTermName)
      var m: Symbol = context.scope.lookup(tree.name)
      if (!inIDE && m.isModule && !m.isPackage && (context.scope == m.owner.info.decls) &&
          !currentRun.compiles(m)) {
        updatePosFlags(m, tree.pos, tree.mods.flags|MODULE|FINAL)
        setPrivateWithin(tree, m, tree.mods)
      } else {
        //if (m.isTerm && !m.isPackage && currentRun.compiles(m) && (context.scope == m.owner.info.decls))
        //  context.scope.unlink(m)

        m = context.owner.newModule(tree.pos, tree.name)
        m.setFlag(tree.mods.flags)
        m = setPrivateWithin(tree, m, tree.mods)
        m = enterInScope(m)

        m.moduleClass.setFlag(tree.mods.flags|MODULE|FINAL| inConstructorFlag)
        setPrivateWithin(tree, m.moduleClass, tree.mods)
      }
      if (m.owner.isPackageClass) {
        m.moduleClass.sourceFile = context.unit.source.file
        currentRun.symSource(m) = m.moduleClass.sourceFile
      }
      m
    }

    def enterCaseFactorySymbol(tree : ClassDef): Symbol = {
      val pos = tree.pos
      val flags = tree.mods.flags & AccessFlags | METHOD | CASE
      val name = tree.name.toTermName
      var m: Symbol = context.scope.lookup(name)
      if (!inIDE && m.isTerm && !m.isPackage && !currentRun.compiles(m) && context.scope == m.owner.info.decls) {
        updatePosFlags(m, pos, flags)
        setPrivateWithin(tree, m, tree.mods)
      } else {
        // recycle the old fashion way.
        m = enterInScope{
          var sym = context.owner.newMethod(pos, name).setFlag(flags)
          sym = setPrivateWithin(tree, sym, tree.mods)
          sym
        }
      }
      if (m.owner.isPackageClass)
        currentRun.symSource(m) = context.unit.source.file
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
     * (a skolem is a representation of a bound variable when viewed outside its scope)
     */
    def skolemize(tparams: List[TypeDef]) {
      val tskolems = newTypeSkolems(tparams map (_.symbol))
      for ((tparam, tskolem) <- tparams zip tskolems) tparam.symbol = tskolem
    }

    def applicableTypeParams(owner: Symbol): List[Symbol] =
      if (owner.isTerm || owner.isPackageClass) List()
      else applicableTypeParams(owner.owner) ::: owner.typeParams

    def deSkolemize: TypeMap = new DeSkolemizeMap(applicableTypeParams(context.owner))

    def reuse[T <: Tree](tree: T) = if (!inIDE) tree else {
      val tree0 = tree.duplicate
	    //tree0.symbol = tree.symbol
      tree0
    }

    def enterSym(tree: Tree): Context = {

      def finishWith(tparams: List[TypeDef]) {
        val sym = tree.symbol
        if (settings.debug.value) log("entered " + sym + " in " + context.owner + ", scope-id = " + context.scope.hashCode());
        var ltype = namerOf(sym).typeCompleter(tree)
        if (!tparams.isEmpty) {
          //@M! TypeDef's type params are handled differently
          //@M e.g., in [A[x <: B], B], A and B are entered first as both are in scope in the definition of x
          //@M x is only in scope in `A[x <: B]'
          if(!sym.isAbstractType) //@M TODO: change to isTypeMember ?
            newNamer(context.makeNewScope(tree, sym)).enterSyms(tparams)
          ltype = new PolyTypeCompleter(tparams, ltype, tree, sym, context) //@M
          if (sym.isTerm) skolemize(tparams)
        }
        setInfo(sym)(ltype)
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
            if ((mods.flags & CASE) != 0) { // enter case factory method.
              val sym = enterCaseFactorySymbol(tree)
              setInfo(sym)(namerOf(sym).caseFactoryCompleter(reuse(tree)))
            }
          case tree @ ModuleDef(mods, name, _) =>
            tree.symbol = enterModuleSymbol(tree)
            // IDE: do not use the setInfo call for the module class as it is initialized
            //      through module symbol
            tree.symbol.moduleClass.setInfo(namerOf(tree.symbol).moduleClassTypeCompleter(reuse(tree)))
            finish

          case ValDef(mods, name, tp, rhs) =>
            if (context.owner.isClass && (mods.flags & (PRIVATE | LOCAL)) != (PRIVATE | LOCAL)
                || (mods.flags & LAZY) != 0) {
              val accflags: Long = ACCESSOR |
                (if ((mods.flags & MUTABLE) != 0) mods.flags & ~MUTABLE & ~PRESUPER
                 else mods.flags & ~PRESUPER | STABLE)
              var getter = owner.newMethod(tree.pos, name).setFlag(accflags)
              setPrivateWithin(tree, getter, mods)
              getter = enterInScope(getter).asInstanceOf[TermSymbol]
              setInfo(getter)(namerOf(getter).getterTypeCompleter(tree))
              if ((mods.flags & MUTABLE) != 0) {
                var setter = owner.newMethod(tree.pos, nme.getterToSetter(name))
                                .setFlag(accflags & ~STABLE & ~CASEACCESSOR)
                setPrivateWithin(tree, setter, mods)
                setter = enterInScope(setter).asInstanceOf[TermSymbol]
                setInfo(setter)(namerOf(setter).setterTypeCompleter(tree))
              }
              tree.symbol =
                if ((mods.flags & DEFERRED) == 0) { // not deferred
                  var vsym =
                    if (!context.owner.isClass) {
                      assert((mods.flags & LAZY) != 0) // if not a field, it has to be a lazy val
                      owner.newValue(tree.pos, name + "$lzy" ).setFlag(mods.flags | MUTABLE)
                    } else {
                      owner.newValue(tree.pos, nme.getterToLocal(name))
                        .setFlag(mods.flags & FieldFlags | PRIVATE | LOCAL | (if ((mods.flags & LAZY) != 0) MUTABLE else 0))
                    }
                  vsym = enterInScope(vsym).asInstanceOf[TermSymbol]
                  setInfo(vsym)(namerOf(vsym).typeCompleter(tree))
                  if ((mods.flags & LAZY) != 0)
                    vsym.setLazyAccessor(getter)
                  vsym
                } else getter;
            } else {
              tree.symbol = enterInScope(owner.newValue(tree.pos, name)
                .setFlag(mods.flags))
              finish
            }
          case DefDef(mods, nme.CONSTRUCTOR, tparams, _, _, _) =>
            var sym = owner.newConstructor(tree.pos).setFlag(mods.flags | owner.getFlag(ConstrFlags))
            setPrivateWithin(tree, sym, mods)
            tree.symbol = enterInScope(sym)
            finishWith(tparams)
          case DefDef(mods, name, tparams, _, _, _) =>
            var sym = (owner.newMethod(tree.pos, name)).setFlag(mods.flags)
            setPrivateWithin(tree, sym, mods)
            tree.symbol = enterInScope(sym)
            finishWith(tparams)
          case TypeDef(mods, name, tparams, _) =>
            var flags: Long = mods.flags
            if ((flags & PARAM) != 0) flags |= DEFERRED
            var sym =new TypeSymbol(owner, tree.pos, name).setFlag(flags)
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
    }

// --- Lazy Type Assignment --------------------------------------------------

    def typeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      if (settings.debug.value) log("defining " + sym + Flags.flagsToString(sym.flags));
      val tp = typeSig(tree)
      tp match {
        case TypeBounds(lo, hi) =>
          // check that lower bound is not an F-bound
          for (val t <- lo) {
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

    def moduleClassTypeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      tree.symbol.info // sets moduleClass info as a side effect.
    }

    def getterTypeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      if (settings.debug.value) log("defining " + sym)
      sym.setInfo(PolyType(List(), typeSig(tree)))
      if (settings.debug.value) log("defined " + sym)
      validate(sym)
    }

    def setterTypeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      if (settings.debug.value) log("defining " + sym);
      sym.setInfo(MethodType(List(typeSig(tree)), UnitClass.tpe))
      if (settings.debug.value) log("defined " + sym);
      validate(sym)
    }

    def selfTypeCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      var selftpe = typer.typedType(tree).tpe
      if (!(selftpe.typeSymbol isNonBottomSubClass sym.owner))
        selftpe = intersectionType(List(sym.owner.tpe, selftpe))
//    println("completing self of "+sym.owner+": "+selftpe)
      sym.setInfo(selftpe)
    }

    def caseFactoryCompleter(tree: Tree) = mkTypeCompleter(tree) { sym =>
      val clazz = tree.symbol
      var tpe = clazz.primaryConstructor.tpe
      val tparams = clazz.typeParams
      if (!tparams.isEmpty) tpe = PolyType(tparams, tpe).cloneInfo(sym);
      sym.setInfo(tpe)
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

    def enterValueParams(owner: Symbol, vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      def enterValueParam(param: ValDef): Symbol = if (doEnterValueParams) {
        if (inIDE) param.symbol = {
          var sym = owner.newValueParameter(param.pos, param.name).
            setFlag(param.mods.flags & (BYNAMEPARAM | IMPLICIT))
          setPrivateWithin(param, sym, param.mods)
          sym = enterInScope(sym).asInstanceOf[TermSymbol]
          if (!sym.rawInfoSafe.isDefined || sym.rawInfo.isComplete)
            setInfo(sym)(typeCompleter(param))
          sym
        } else param.symbol = setInfo(
          enterInScope{
            val sym = owner.newValueParameter(param.pos, param.name).
              setFlag(param.mods.flags & (BYNAMEPARAM | IMPLICIT))
            setPrivateWithin(param, sym, param.mods)
          })(typeCompleter(param))
        param.symbol
      } else param.symbol
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
          } else {
            self.symbol = clazz.newThisSym(self.pos) setInfo clazz.tpe
          }
        }
        if (self.name != nme.WILDCARD) {
          self.symbol.name = self.name
          self.symbol = context.scope enter self.symbol
        }
      }
      val parents = typer.parentTypes(templ) map checkParent
      enterSelf(templ.self)
      val decls = newDecls(clazz)
      newNamer(context.make(templ, clazz, decls)).enterSyms(templ.body)
      ClassInfoType(parents, decls, clazz)
    }

    private def classSig(tparams: List[TypeDef], impl: Template): Type =
      parameterizedType(typer.reenterTypeParams(tparams), templateSig(impl))

    private def methodSig(tparams: List[TypeDef], vparamss: List[List[ValDef]],
                          tpt: Tree, rhs: Tree): Type = {
      val meth = context.owner

      val tparamSyms = typer.reenterTypeParams(tparams)
      var vparamSymss =
        if (inIDE && meth.isPrimaryConstructor) {
          // @S: because they have already been entered this way....
          assert(true)
          enterValueParams(meth.owner.owner, vparamss)
        } else {
          enterValueParams(meth, vparamss)
        }
      if (tpt.isEmpty && meth.name == nme.CONSTRUCTOR) tpt.tpe = context.enclClass.owner.tpe

      if (onlyPresentation)
        methodArgumentNames(meth) = vparamss.map(_.map(_.symbol));

      def convertToDeBruijn(vparams: List[Symbol], level: Int): TypeMap = new TypeMap {
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
                DeBruijnIndex(level, vparams indexOf sym)
              } else tp
            case MethodType(formals, restpe) =>
              val formals1 = List.mapConserve(formals)(this)
              val restpe1 = convertToDeBruijn(vparams, level + 1)(restpe)
              if ((formals1 eq formals) && (restpe1 eq restpe)) tp
              else copyMethodType(tp, formals1, restpe1)
            case _ =>
              mapOver(tp)
          }
        }
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

      def makeMethodType(vparams: List[Symbol], restpe: Type) = {
        val formals = vparams map (_.tpe)
        val restpe1 = convertToDeBruijn(vparams, 1)(restpe)
        if (!vparams.isEmpty && vparams.head.hasFlag(IMPLICIT))
          ImplicitMethodType(formals, restpe1)
        else MethodType(formals, restpe1)
      }

      def thisMethodType(restpe: Type) =
        parameterizedType(
          tparamSyms,
          if (vparamSymss.isEmpty) PolyType(List(), restpe)
          else checkDependencies((vparamSymss :\ restpe) (makeMethodType)))

      var resultPt = if (tpt.isEmpty) WildcardType else typer.typedType(tpt).tpe
      val site = meth.owner.thisType

      def overriddenSymbol = intersectionType(meth.owner.info.parents).member(meth.name).filter(sym =>
          sym != NoSymbol && (site.memberType(sym) matches thisMethodType(resultPt)))

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
      // Add a () parameter section if this overrides dome method with () parameters.
      if (meth.owner.isClass && vparamss.isEmpty && overriddenSymbol.alternatives.exists(
        _.info.isInstanceOf[MethodType])) {
        vparamSymss = List(List())
      }
      for (vparams <- vparamss; vparam <- vparams if vparam.tpt.isEmpty) {
        context.error(vparam.pos, "missing parameter type")
        vparam.tpt.tpe = ErrorType
      }

      thisMethodType(
        if (tpt.isEmpty) {
          val pt = resultPt.substSym(tparamSyms, tparams map (_.symbol))
          tpt.tpe = widenIfNotFinal(meth, typer.computeType(rhs, pt), pt)
          tpt.tpe
        } else typer.typedType(tpt).tpe)
     }

    /** If `sym' is an implicit value, check that its type signature `tp' is contractive.
     *  This means: The type of every implicit parameter is properly contained
     *  in the type that is obtained by removing all implicit parameters and converting
     *  the rest to a function type.
     *  If the check succeeds return `tp' itself, otherwise `ErrorType'.
     */
    private def checkContractive(sym: Symbol, tp: Type): Type = {
      /* The type signature without implicit parameters converted to function type */
      def provided(tp: Type): Type = tp match {
        case PolyType(_, restpe) => provided(restpe)
        case mt: ImplicitMethodType => mt.resultType
        case MethodType(formals, restpe) => functionType(formals, provided(restpe))
        case _ => tp
      }
      /* The types of all implicit parameters */
      def required(tp: Type): List[Type] = tp match {
        case PolyType(_, restpe) => required(restpe)
        case mt: ImplicitMethodType => mt.paramTypes
        case MethodType(formals, restpe) => required(restpe)
        case _ => List()
      }
      var result = tp;
      if (sym hasFlag IMPLICIT) {
        val p = provided(tp);
        //Console.println("check contractive: "+sym+" "+p+"/"+required(tp))
        for (r <- required(tp)) {
          if (!isContainedIn(r, p) || (r =:= p)) {
            context.error(sym.pos, "implicit " + sym + " is not contractive," +
                          "\n because the implicit parameter type " + r +
                          "\n is not strictly contained in the signature " + p);
            result = ErrorType;
          }
        }
      }
      result
    }

    //@M! an abstract type definition (abstract type member/type parameter) may take type parameters, which are in scope in its bounds
    private def typeDefSig(tpsym: Symbol, tparams: List[TypeDef], rhs: Tree) = {
      val tparamSyms = typer.reenterTypeParams(tparams) //@M make tparams available in scope (just for this abstypedef)
      val tp = typer.typedType(rhs).tpe match {
        case TypeBounds(lt, rt) if (lt.isError || rt.isError) =>
          TypeBounds(AllClass.tpe, AnyClass.tpe)
        case tp => tp
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
      else parameterizedType(tparamSyms, tp)
    }

    def typeSig(tree: Tree): Type = {
      val sym: Symbol = tree.symbol
      tree match {
        case defn: MemberDef =>
          val ainfos = for {
            annot <- defn.mods.annotations
            val ainfo = typer.typedAnnotation(annot)
            if !ainfo.atp.isError && annot != null
          } yield ainfo
          if (!ainfos.isEmpty) {
            val annotated = if (sym.isModule) sym.moduleClass else sym
            annotated.attributes = ainfos
          }
        case _ =>
      }
      val result =
        try {
          tree match {
            case ClassDef(_, _, tparams, impl) =>
              newNamer(context.makeNewScope(tree, sym)).classSig(tparams, impl)

            case ModuleDef(_, _, impl) =>
              val clazz = sym.moduleClass
              clazz.setInfo(newNamer(context.makeNewScope(tree, clazz)).templateSig(impl))
              //clazz.typeOfThis = singleType(sym.owner.thisType, sym);
              clazz.tpe;

            case DefDef(_, _, tparams, vparamss, tpt, rhs) =>
              val result =
                newNamer(context.makeNewScope(tree, sym)).methodSig(tparams, vparamss, tpt, rhs);
              checkContractive(sym, result)

            case vdef @ ValDef(mods, _, tpt, rhs) =>
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
                  tpt.tpe
                }
              } else typer1.typedType(tpt).tpe

            case TypeDef(_, _, tparams, rhs) =>
              newNamer(context.makeNewScope(tree, sym)).typeDefSig(sym, tparams, rhs) //@M!

            case Import(expr, selectors) =>
              val expr1 = typer.typedQualifier(expr)
              val base = expr1.tpe
              typer.checkStable(expr1)
              def checkNotRedundant(pos: Position, from: Name, to: Name): Boolean = {
                if (!tree.symbol.hasFlag(SYNTHETIC) &&
                    !((expr1.symbol ne null) && expr1.symbol.isInterpreterWrapper) &&
                    base.member(from) != NoSymbol) {
                  val e = context.scope.lookupEntry(to)
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
      deSkolemize(result)
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
              " for: " + sym + Flags.flagsToString(sym.rawflags));
      }
      if (sym.hasFlag(IMPLICIT) && !sym.isTerm)
        context.error(sym.pos, "`implicit' modifier can be used only for values, variables and methods")
      if (sym.hasFlag(IMPLICIT) && sym.owner.isPackageClass && !inIDE)
        context.error(sym.pos, "`implicit' modifier cannot be used for top-level objects")
      if (sym.hasFlag(ABSTRACT) && !sym.isClass)
        context.error(sym.pos, "`abstract' modifier can be used only for classes; " +
          "\nit should be omitted for abstract members")
      if (sym.hasFlag(OVERRIDE | ABSOVERRIDE) && sym.isClass)
        context.error(sym.pos, "`override' modifier not allowed for classes")
      if (sym.hasFlag(OVERRIDE | ABSOVERRIDE) && sym.isConstructor)
        context.error(sym.pos, "`override' modifier not allowed for constructors")
      if (sym.hasFlag(ABSOVERRIDE) && !sym.owner.isTrait)
        context.error(sym.pos, "`abstract override' modifier only allowed for members of traits")
      if (sym.info.typeSymbol == FunctionClass(0) &&
          sym.isValueParameter && sym.owner.isClass && sym.owner.hasFlag(CASE))
        context.error(sym.pos, "pass-by-name arguments not allowed for case class parameters");
      if ((sym.flags & DEFERRED) != 0) {
        if (sym.hasAttribute(definitions.NativeAttr))
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
      checkNoConflict(DEFERRED, FINAL)
      checkNoConflict(ABSTRACT, CASE)
    }
  }

  /* Is type `tp1' properly contained in type `tp2'? */
  def isContainedIn(tp1: Type, tp2: Type) = {
    //Console.println("is " + tp1 + " contained in " + tp2 + "?");//DEBUG
    new ContainsTraverser(tp1).traverse(tp2).result
  }

  /* Type `elemtp' is contained in type `tp' is one of the following holds:
   *  - elemtp is the same as some proper part of tp
   *  - tp is a function type and elemtp is not
   *  - tp and elemtp are function types, and arity of tp is greater than arity of elemtp
   *  - tp and elemtp are both parameterized types with same type constructor and prefix,
   *    and each type argument of elemtp is contained in the corresponding type argument of tp.
   */
  private class ContainsTraverser(elemtp: Type) extends TypeTraverser {
    var nested = false
    var result = false
    def traverse(tp: Type): ContainsTraverser = {
      if (!result) {
        if (elemtp =:= tp)
          result = nested
        else if (isFunctionType(tp) &&
                 (!isFunctionType(elemtp) || tp.normalize.typeArgs.length > elemtp.normalize.typeArgs.length))
          result = true
        else (tp, elemtp) match {
          case (TypeRef(pre, sym, args), TypeRef(elempre, elemsym, elemargs)) =>
            if ((sym == elemsym) && (pre =:= elempre) && (args.length == elemargs.length))
              result = List.forall2(elemargs, args) (isContainedIn)
          case _ =>
        }
      }
      if (!result) {
        tp match {
          case SingleType(_, _) => nested = true
          case TypeRef(_, _, _) => nested = true
          case _ =>
        }
        mapOver(tp)
      }
      this
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
        newNamer(ctx.makeNewScope(owner, ownerSym)).enterSyms(tparams) //@M
      restp.complete(sym)
    }
  }

  /** The symbol that which this accessor represents (possibly in part).
   *  This is used for error messages, where we want to speak in terms
   *  of the actual declaration or definition, not in terms of the generated setters
   *  and getters */
  def underlying(member: Symbol): Symbol =
    if (member hasFlag ACCESSOR) {
      if (member hasFlag DEFERRED) {
        val getter = if (member.isSetter) member.getter(member.owner) else member
        if (inIDE && getter == NoSymbol) return NoSymbol;
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

