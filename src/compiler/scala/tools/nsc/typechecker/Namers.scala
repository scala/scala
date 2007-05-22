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
trait Namers requires Analyzer {
  import global._
  import definitions._

  /** Convert to corresponding type parameters all skolems which satisfy one
   *  of the following two conditions:
   *  1. The skolem is a parameter of a class or alias type
   *  2. The skolem is a method parameter which appears in parameter `tparams'
   */
  class DeSkolemizeMap(tparams: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) =>
        val tparam = sym.deSkolemize
        mapOver(
          if (tparam == sym || !(tparams contains tparam)) tp
          else rawTypeRef(NoPrefix, tparam, args))
      case SingleType(pre, sym) if (sym.isThisSkolem) =>
        mkThisType(sym.deSkolemize)
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
  protected final def doEnterValueParams = !inIDE;
  protected def inIDE = false;

  class Namer(val context: Context) {

    val typer = newTyper(context)

    def setPrivateWithin(tree: Tree, sym: Symbol, mods: Modifiers): Symbol = {
      if (!mods.privateWithin.isEmpty)
        sym.privateWithin = typer.qualifyingClassContext(tree, mods.privateWithin).owner
      sym
    }

    def updatePosFlags(sym: Symbol, pos: Position, flags: long): Symbol = {
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

    private def isTemplateContext(context: Context): boolean = context.tree match {
      case Template(_, _, _) => true
      case Import(_, _) => isTemplateContext(context.outer)
      case _ => false
    }

    private var innerNamerCache: Namer = null

    protected def newNamer(context : Context) : Namer = new Namer(context)

    def namerOf(sym: Symbol): Namer = {
      def innerNamer: Namer = {
        if (innerNamerCache eq null)
          innerNamerCache =
            if (!isTemplateContext(context)) this
            else newNamer(context.make(context.tree, context.owner, newScope))
        innerNamerCache
      }

      def primaryConstructorParamNamer: Namer = { //todo: can we merge this with SCCmode?
        val classContext = context.enclClass
        val outerContext = classContext.outer.outer
        val paramContext = makeNewScope(outerContext, outerContext.tree, outerContext.owner)
        classContext.owner.unsafeTypeParams foreach paramContext.scope.enter
        newNamer(paramContext)
      }

      if (sym.isTerm &&
          (sym.hasFlag(PARAM) && sym.owner.isPrimaryConstructor || sym.hasFlag(PARAMACCESSOR)))
        primaryConstructorParamNamer
      else
        innerNamer
    }

    private def doubleDefError(pos: Position, sym: Symbol): unit =
      context.error(pos,
        sym.name.toString() + " is already defined as " +
        (if (sym.hasFlag(CASE)) "case class " + sym.name else sym.toString()))

    def enterInScope[Sym <: Symbol](sym: Sym): Sym = {
      // allow for overloaded methods
      if (!(sym.isSourceMethod && sym.owner.isClass && !sym.owner.isPackageClass)) {
        val prev = context.scope.lookupEntry(sym.name);
        if ((prev ne null) && (prev.sym eq sym)) return sym
        if ((prev ne null) && prev.owner == context.scope &&
            (!prev.sym.isSourceMethod ||
             nme.isSetterName(sym.name) ||
             sym.owner.isPackageClass) &&
             !(sym.owner.isTypeParameter && sym.name.length==1 && sym.name(0)=='_')) { //@M: allow repeated use of `_' for higher-order type params
           doubleDefError(sym.pos, prev.sym)
           sym setInfo ErrorType
        } else context.scope enter sym
      } else context.scope enter sym
      sym
    }


    def enterPackageSymbol(pos: Position, name: Name): Symbol = {
      val cscope = if (context.owner == EmptyPackageClass) RootClass.info.decls
                   else context.scope
      val p: Symbol = cscope.lookup(name)
      if (p.isPackage && cscope == p.owner.info.decls) {
        p
      } else {
        val cowner = if (context.owner == EmptyPackageClass) RootClass else context.owner
        val pkg = newPackage(cowner, pos, name)
        pkg.moduleClass.setInfo(new PackageClassInfoType(newScope, pkg.moduleClass))
        //pkg.setInfo(pkg.moduleClass.tpe)
        enterInScope(pkg) setInfo (pkg.moduleClass.tpe)
      }
    }

    def inConstructorFlag: long =
      if (context.owner.isConstructor && !context.inConstructorSuffix || context.owner.isEarly) INCONSTRUCTOR
      else 0l


    private def enterClassSymbol(tree : ClassDef, flags: long, name: Name): Symbol = {
      var c: Symbol = context.scope.lookup(name)
      if (c.isType && !currentRun.compiles(c) && context.scope == c.owner.info.decls) {
        updatePosFlags(setPrivateWithin(tree, c, tree.mods), tree.pos, flags)
      } else {
        c = enterAndSet(tree)
        {newClass(tree.pos, name, flags | inConstructorFlag)}
      }
      if (c.owner.isPackageClass) {
        val file = context.unit.source.getFile()
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
      c
    }

    private def enterModuleSymbol(tree: ModuleDef, flags: long, name: Name): Symbol = {
      var m: Symbol = context.scope.lookup(name)
      if (m.isModule && !m.isPackage && !currentRun.compiles(m) &&
         (context.scope == m.owner.info.decls)) {
        updatePosFlags(setPrivateWithin(tree, m, tree.mods), tree.pos, flags)
      } else {
        if (m.isTerm && !m.isPackage && !currentRun.compiles(m) && (context.scope == m.owner.info.decls))
          context.scope.unlink(m)
        m = enterAndSet(tree)
        {newModule(tree.pos, name, flags)}
        m.moduleClass.setFlag(flags | inConstructorFlag)
      }
      if (m.owner.isPackageClass) {
        m.moduleClass.sourceFile = context.unit.source.getFile()
        currentRun.symSource(m) = m.moduleClass.sourceFile
      }
      m
    }
    def newMethod(pos : Position, name : Name, flags : Long) : TermSymbol = context.owner.newMethod(pos, name).setFlag(flags)
    def newValue(pos : Position, name : Name, flags : Long) : TermSymbol = context.owner.newValue(pos, name).setFlag(flags)
    def newConstructor(pos : Position, flags : Long) : TermSymbol = context.owner.newConstructor(pos).setFlag(flags)
    def newModule(pos : Position, name : Name, flags : Long) : TermSymbol = context.owner.newModule(pos, name).setFlag(flags)
    def newClass(pos : Position, name : Name, flags : Long) : ClassSymbol = context.owner.newClass(pos, name).setFlag(flags)
    def newPackage(owner : Symbol, pos : Position, name : Name) : TermSymbol = owner.newPackage(pos, name)
    def newImport(pos : Position) : TermSymbol = NoSymbol.newImport(pos)

    def newAbstractType(pos : Position, name : Name, flags : Long) : TypeSymbol = context.owner.newAbstractType(pos, name).setFlag(flags)
    def newAliasType(pos : Position, name : Name, flags : Long) : TypeSymbol = context.owner.newAliasType(pos, name).setFlag(flags)
    def newValueParameter(owner : Symbol, pos : Position, name : Name, flags : Long) : TermSymbol = owner.newValueParameter(pos, name).setFlag(flags)
    def newThisSym(owner : Symbol, pos : Position) : TermSymbol = owner.newThisSym(pos)
    def newTypeSkolem(sym : Symbol) = sym.newTypeSkolem
    def newLabel(owner : Symbol, pos : Position, name : Name) = owner.newLabel(pos, name)

    protected def setInfo(t : LazyType)(sym : Symbol) : Symbol =
      sym setInfo t

    private def enterCaseFactorySymbol(tree : ClassDef, flags: long, name: Name): Symbol = {
      var m: Symbol = context.scope.lookup(name)
      if (m.isTerm && !m.isPackage && !currentRun.compiles(m) && context.scope == m.owner.info.decls) {
        updatePosFlags(setPrivateWithin(tree, m, tree.mods), tree.pos, flags)
      } else {
        m = enterAndSet(tree)
        {newMethod(tree.pos, name, flags)}
      }
      if (m.owner.isPackageClass)
        currentRun.symSource(m) = context.unit.source.getFile()
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
      val tskolems = tparams map (newTypeSkolem)
      val ltp = new LazyType {
        override def complete(sym: Symbol): unit =
          sym setInfo sym.deSkolemize.info.substSym(tparams, tskolems) //@M the info of a skolem is the skolemized info of the actual type parameter of the skolem
      }
      tskolems foreach (setInfo(ltp))
      tskolems
    }

    /** Replace type parameters with their TypeSkolems, which can later be deskolemized to the original type param
     * (a skolem is a representation of a bound variable when viewed outside its scope)
     */
    def skolemize(tparams: List[AbsTypeDef]): unit = {
      val tskolems = newTypeSkolems(tparams map (.symbol))
      for ((tparam, tskolem) <- tparams zip tskolems) tparam.symbol = tskolem
    }

    def applicableTypeParams(owner: Symbol): List[Symbol] =
      if (owner.isTerm || owner.isPackageClass) List()
      else applicableTypeParams(owner.owner) ::: owner.typeParams

    def deSkolemize: TypeMap = new DeSkolemizeMap(applicableTypeParams(context.owner))

    /** A class representing a lazy type with known type parameters.
     */
    class LazyPolyType(tparams: List[Tree], restp: Type, owner: Tree, ownerSym: Symbol, ctx: Context) extends LazyType { //@M
      override val typeParams: List[Symbol]= tparams map (.symbol) //@M
      override def complete(sym: Symbol): unit = {
        if(ownerSym.isAbstractType) //@M an abstract type's type parameters are entered
          newNamer(makeNewScope(ctx, owner, ownerSym)).enterSyms(tparams) //@M
        restp.complete(sym)
      }
    }

    protected def enterAndSet(tree : MemberDef)(sym : Symbol) : Symbol =
      enterInScope(setPrivateWithin(tree, sym, tree.mods))

    def enterSym(tree: Tree): Context = {
      def finishWith(tparams: List[AbsTypeDef]): unit = {
        val sym = tree.symbol
        if (settings.debug.value) log("entered " + sym + " in " + context.owner + ", scope-id = " + context.scope.hashCode());
        var ltype: LazyType = namerOf(sym).typeCompleter(tree)
        if (!tparams.isEmpty) {
          //@M! AbsTypeDef's type params are handled differently
          //@M e.g., in [A[x <: B], B], A and B are entered first as both are in scope in the definition of x
          //@M x is only in scope in `A[x <: B]'
          if(!sym.isAbstractType) //@M
            new Namer(makeNewScope(context, tree, sym)).enterSyms(tparams)
          ltype = new LazyPolyType(tparams, ltype, tree, sym, context) //@M
          if (sym.isTerm) skolemize(tparams)
        }
        setInfo(ltype)(sym)
      }
      def finish = finishWith(List())

      if (tree.symbol == NoSymbol) {
        //val owner = context.owner
        tree match {
          case PackageDef(name, stats) =>
            tree.symbol = enterPackageSymbol(tree.pos, name)
            val namer = newNamer{
              context.make(tree, tree.symbol.moduleClass, tree.symbol.info.decls)
            }
            namer.enterSyms(stats)
          case tree @ ClassDef(mods, name, tparams, impl) =>
            if ((mods.flags & CASE) != 0) { // enter case factory method.
              tree.symbol = enterCaseFactorySymbol(
                  tree, mods.flags & AccessFlags | METHOD | CASE, name.toTermName)
              setInfo(namerOf(tree.symbol).caseFactoryCompleter(tree))(tree.symbol)
            }
            tree.symbol = enterClassSymbol(tree, mods.flags, name)
            finishWith(tparams)
          case tree @ ModuleDef(mods, name, _) =>
            tree.symbol = enterModuleSymbol(tree, mods.flags | MODULE | FINAL, name)
            // additional stuff with moduleClass, note that module class doesn't go into the symbol table, so we don't care about it
            setPrivateWithin(tree, tree.symbol.moduleClass, mods)
            setInfo(namerOf(tree.symbol).moduleClassTypeCompleter(tree))(tree.symbol.moduleClass)
            finish
          case tree @ ValDef(mods, name, tp, rhs) =>
            if (context.owner.isClass && (mods.flags & (PRIVATE | LOCAL)) != (PRIVATE | LOCAL)) {
              val accflags = ACCESSOR |
                (if ((mods.flags & MUTABLE) != 0) mods.flags & ~MUTABLE & ~PRESUPER
                 else mods.flags & ~PRESUPER | STABLE)
              val getter = enterAndSet(tree)
              {newMethod(tree.pos, name, accflags)}
              setInfo(namerOf(getter).getterTypeCompleter(tree))(getter)
              if ((mods.flags & MUTABLE) != 0) {
                val setter = enterAndSet(tree)
                {newMethod(tree.pos, nme.getterToSetter(name),
                    accflags & ~STABLE & ~CASEACCESSOR)}
                setInfo(namerOf(setter).setterTypeCompleter(tree))(setter)
              }
              tree.symbol =
                if ((mods.flags & DEFERRED) == 0) {
                  val value =
                    enterInScope(newValue(tree.pos, nme.getterToLocal(name), mods.flags & FieldFlags | PRIVATE | LOCAL))
                  setInfo(namerOf(value).typeCompleter(tree))(value)
                  value
                } else getter;
            } else {
              tree.symbol = enterInScope(newValue(tree.pos, name, mods.flags))
              finish
            }
          case tree @ DefDef(mods, nme.CONSTRUCTOR, tparams, _, _, _) =>
            tree.symbol = enterAndSet(tree)
            {newConstructor(tree.pos, mods.flags | context.owner.getFlag(ConstrFlags))}
            finishWith(tparams)
          case tree @ DefDef(mods, name, tparams, _, _, _) =>
            tree.symbol = enterAndSet(tree)
            {newMethod(tree.pos, name, mods.flags)}
            finishWith(tparams)
          case tree @ AbsTypeDef(mods, name, tparams, _, _) =>
            tree.symbol = enterAndSet(tree)
            {newAbstractType(tree.pos, name, mods.flags)}
            finishWith(tparams)
          case tree @ AliasTypeDef(mods, name, tparams, _) =>
            tree.symbol = enterAndSet(tree)
            {newAliasType(tree.pos, name, mods.flags)}
            finishWith(tparams)
          case DocDef(_, defn) =>
            enterSym(defn)
          case imp @ Import(_, _) =>
            tree.symbol = newImport(tree.pos)
            setInfo(namerOf(tree.symbol).typeCompleter(tree))(tree.symbol)
            return (context.makeNewImport(imp))
          case _ =>
        }
      }
      this.context
    }

// --- Lazy Type Assignment --------------------------------------------------

    def typeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        if (settings.debug.value) log("defining " + sym);
        val tp = typeSig(tree)
        sym.setInfo(tp)
        if ((sym.isAliasType || sym.isAbstractType) && !(sym hasFlag PARAM) &&
            !typer.checkNonCyclic(tree.pos, tp))
          sym.setInfo(ErrorType) // this early test is there to avoid infinite baseTypes when
                                 // adding setters and getters --> bug798
        if (settings.debug.value) log("defined " + sym);
        validate(sym)
      }
    }

    def moduleClassTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        tree.symbol.info // sets moduleClass info as a side effect.
      }
    }

    def getterTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        if (settings.debug.value) log("defining " + sym);
        sym.setInfo(PolyType(List(), typeSig(tree)))
        if (settings.debug.value) log("defined " + sym);
        validate(sym)
      }
    }

    def setterTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        if (settings.debug.value) log("defining " + sym);
        sym.setInfo(MethodType(List(typeSig(tree)), UnitClass.tpe))
        if (settings.debug.value) log("defined " + sym);
        validate(sym)
      }
    }

    def selfTypeCompleter(tree: Tree) : Type = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        sym.setInfo(typer.typedType(tree).tpe)
      }
    }

    def caseFactoryCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        val clazz = tree.symbol
        var tpe = clazz.primaryConstructor.tpe
        val tparams = clazz.typeParams
        if (!tparams.isEmpty) tpe = PolyType(tparams, tpe).cloneInfo(sym);
        sym.setInfo(tpe)
      }
    }

    private def deconstIfNotFinal(sym: Symbol, tpe: Type): Type =
      if (sym.isVariable ||
          !(sym hasFlag FINAL) ||
          sym.isMethod && !(sym hasFlag ACCESSOR)) tpe.deconst
      else tpe;


    def enterValueParams(owner: Symbol, vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      def enterValueParam(param: ValDef): Symbol = if (doEnterValueParams) {
        param.symbol = setInfo(typeCompleter(param))(
            enterAndSet(param)
            {newValueParameter(owner, param.pos, param.name,
                param.mods.flags & (BYNAMEPARAM | IMPLICIT))})

        param.symbol
      } else param.symbol
      vparamss.map(.map(enterValueParam))
    }

    private def templateSig(templ: Template): Type = {
      val clazz = context.owner
      def checkParent(tpt: Tree): Type = {
        val tp = tpt.tpe
        if (tp.symbol == context.owner) {
          context.error(tpt.pos, ""+tp.symbol+" inherits itself")
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
          self.symbol = clazz.thisSym
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
          context.scope enter self.symbol
        }
      }
      val parents = typer.parentTypes(templ) map checkParent
      enterSelf(templ.self)
      val decls = newDecls(templ, clazz)
      new Namer(context.make(templ, clazz, decls)).enterSyms(templ.body)
      ClassInfoType(parents, decls, clazz)
    }

    private def classSig(tparams: List[AbsTypeDef], impl: Template): Type =
      parameterizedType(typer.reenterTypeParams(tparams), templateSig(impl))

    private def methodSig(tparams: List[AbsTypeDef], vparamss: List[List[ValDef]],
                          tpt: Tree, rhs: Tree): Type = {
      val meth = context.owner

      val tparamSyms = typer.reenterTypeParams(tparams)
      var vparamSymss = enterValueParams(meth, vparamss)
      if (tpt.isEmpty && meth.name == nme.CONSTRUCTOR) tpt.tpe = context.enclClass.owner.tpe

      if (onlyPresentation)
        methodArgumentNames(meth) = vparamss.map(.map(.symbol));

      def convertToDeBruijn(vparams: List[Symbol], level: int): TypeMap = new TypeMap {
        def apply(tp: Type) = {
          tp match {
            case SingleType(_, sym) =>
              if (settings.Xexperimental.value && sym.owner == meth && (vparams contains sym)) {
                if (sym hasFlag IMPLICIT) {
                  context.error(sym.pos, "illegal type dependence on implicit parameter")
                  ErrorType
                } else DeBruijnIndex(level, vparams indexOf sym)
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
              if (settings.Xexperimental.value && sym.owner == meth &&
                  (vparamSymss exists (_ contains sym)))
                context.error(
                  sym.pos,
                  "illegal dependent method type: parameter appears in the type "+
                  "of another parameter in the same section or an earlier one")
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
      if (meth.owner.isClass && (tpt.isEmpty || vparamss.exists(.exists(.tpt.isEmpty)))) {
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
        .info.isInstanceOf[MethodType])) {
        vparamSymss = List(List())
      }
      for (vparams <- vparamss; vparam <- vparams if vparam.tpt.isEmpty) {
        context.error(vparam.pos, "missing parameter type")
        vparam.tpt.tpe = ErrorType
      }

      thisMethodType(
        if (tpt.isEmpty) {
          val pt = resultPt.substSym(tparamSyms, tparams map (.symbol))
          tpt.tpe = deconstIfNotFinal(meth, typer.computeType(rhs, pt))
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
    private def abstractTypeSig(tree: Tree, tpsym: Symbol, tparams: List[AbsTypeDef], lo: Tree, hi: Tree) = {
      val tparamSyms = typer.reenterTypeParams(tparams) //@M make tparams available in scope (just for this abstypedef)

      var lt = typer.typedType(lo).tpe
      if (lt.isError) lt = AllClass.tpe

      var ht = typer.typedType(hi).tpe
      if (ht.isError) ht = AnyClass.tpe

      parameterizedType(tparamSyms,  mkTypeBounds(lt, ht)) //@M
    }

    private def aliasTypeSig(tpsym: Symbol, tparams: List[AbsTypeDef], rhs: Tree): Type =
      parameterizedType(typer.reenterTypeParams(tparams), typer.typedType(rhs).tpe);

    def typeSig(tree: Tree): Type = {
      val sym: Symbol = tree.symbol
      tree match {
        case defn: MemberDef =>
          val ainfos = for {
            annot <- defn.mods.annotations
            val ainfo = typer.typedAnnotation(annot)
            if !ainfo.atp.isError
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
              new Namer(makeNewScope(context, tree, sym)).classSig(tparams, impl)

            case ModuleDef(_, _, impl) =>
              val clazz = sym.moduleClass
              clazz.setInfo(new Namer(makeNewScope(context, tree, clazz)).templateSig(impl))
              //clazz.typeOfThis = singleType(sym.owner.thisType, sym);
              clazz.tpe;

            case DefDef(_, _, tparams, vparamss, tpt, rhs) =>
              val result =
                new Namer(makeNewScope(context, tree, sym)).methodSig(tparams, vparamss, tpt, rhs);
              checkContractive(sym, result)

            case vdef @ ValDef(mods, _, tpt, rhs) =>
              val typer1 = typer.constrTyperIf(sym.hasFlag(PARAM | PRESUPER) && sym.owner.isConstructor)
              if (tpt.isEmpty) {
                if (rhs.isEmpty) {
                  context.error(tpt.pos, "missing parameter type");
                  ErrorType
                } else {
                  tpt.tpe = deconstIfNotFinal(sym,
                    newTyper(typer1.context.make(vdef, sym)).computeType(rhs, WildcardType))
                  tpt.tpe
                }
              } else typer1.typedType(tpt).tpe

            case tree @ AliasTypeDef(_, _, tparams, rhs) =>
              new Namer(makeNewScope(context, tree, sym)).aliasTypeSig(sym, tparams, rhs)

            case AbsTypeDef(_, _, tparams, lo, hi) =>
              new Namer(makeNewScope(context, tree, sym)).abstractTypeSig(tree, sym, tparams, lo, hi) //@M!

            case Import(expr, selectors) =>
              val expr1 = typer.typedQualifier(expr)
              val base = expr1.tpe
              typer.checkStable(expr1)
              def checkNotRedundant(pos: Position, from: Name, to: Name): boolean = {
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
              def checkSelectors(selectors: List[(Name, Name)]): unit = selectors match {
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
     *   - declarations only in mixins or abstract classes
     */
    def validate(sym: Symbol): unit = {
      def checkNoConflict(flag1: int, flag2: int): unit =
        if (sym.hasFlag(flag1) && sym.hasFlag(flag2))
          context.error(sym.pos,
            if (flag1 == DEFERRED)
              "abstract member may not have " + Flags.flagsToString(flag2) + " modifier";
            else
              "illegal combination of modifiers: " +
              Flags.flagsToString(flag1) + " and " + Flags.flagsToString(flag2));
      if (sym.hasFlag(IMPLICIT) && !sym.isTerm)
        context.error(sym.pos, "`implicit' modifier can be used only for values, variables and methods")
      if (sym.hasFlag(IMPLICIT) && sym.owner.isPackageClass)
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
      if (sym.info.symbol == FunctionClass(0) &&
          sym.isValueParameter && sym.owner.isClass && sym.owner.hasFlag(CASE))
        context.error(sym.pos, "pass-by-name arguments not allowed for case class parameters");
      if ((sym.flags & DEFERRED) != 0) {
        if (!sym.isValueParameter && !sym.isTypeParameterOrSkolem &&
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

  abstract class TypeCompleter(val tree: Tree) extends LazyType

  /** The symbol that which this accessor represents (possibly in part).
   *  This is used for error messages, where we want to speak in terms
   *  of the actual declaration or definition, not in terms of the generated setters
   *  and getters */
  def underlying(member: Symbol) : Symbol =
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
  def varNotice(sym: Symbol) =
    if (underlying(sym).isVariable)
      "\n(Note that variables need to be initialized to be defined)"
    else ""
}

