package scala.reflect
package internal

import scala.collection.mutable.WeakHashMap
import scala.ref.WeakReference

// SI-6241: move importers to a mirror
trait Importers extends api.Importers { self: SymbolTable =>

  def mkImporter(from0: api.Universe): Importer { val from: from0.type } = (
    if (self eq from0) {
      new Importer {
        val from = from0
        val reverse = this.asInstanceOf[from.Importer{ val from: self.type }]
        def importSymbol(sym: from.Symbol) = sym.asInstanceOf[self.Symbol]
        def importType(tpe: from.Type) = tpe.asInstanceOf[self.Type]
        def importTree(tree: from.Tree) = tree.asInstanceOf[self.Tree]
        def importPosition(pos: from.Position) = pos.asInstanceOf[self.Position]
      }
    } else {
      // todo. fix this loophole
      assert(from0.isInstanceOf[SymbolTable], "`from` should be an instance of scala.reflect.internal.SymbolTable")
      new StandardImporter { val from = from0.asInstanceOf[SymbolTable] }
    }
  ).asInstanceOf[Importer { val from: from0.type }]

  abstract class StandardImporter extends Importer {

    val from: SymbolTable

    protected lazy val symMap = new Cache[from.Symbol, Symbol]()
    protected lazy val tpeMap = new Cache[from.Type, Type]()
    protected class Cache[K <: AnyRef, V <: AnyRef] extends WeakHashMap[K, WeakReference[V]] {
      def weakGet(key: K): Option[V] = this get key flatMap WeakReference.unapply
      def weakUpdate(key: K, value: V) = this.update(key, WeakReference(value))
    }

    // fixups and maps prevent stackoverflows in importer
    var pendingSyms = 0
    var pendingTpes = 0
    lazy val fixups = scala.collection.mutable.MutableList[Function0[Unit]]()
    def addFixup(fixup: => Unit): Unit = fixups += (() => fixup)
    def tryFixup(): Unit = {
      if (pendingSyms == 0 && pendingTpes == 0) {
        val fixups = this.fixups.toList
        this.fixups.clear()
        fixups foreach { _() }
      }
    }

    object reverse extends from.StandardImporter {
      val from: self.type = self
      // FIXME this and reverse should be constantly kept in sync
      // not just synced once upon the first usage of reverse
      for ((fromsym, WeakReference(mysym)) <- StandardImporter.this.symMap) symMap += ((mysym, WeakReference(fromsym)))
      for ((fromtpe, WeakReference(mytpe)) <- StandardImporter.this.tpeMap) tpeMap += ((mytpe, WeakReference(fromtpe)))
    }

    // todo. careful import of positions
    def importPosition(pos: from.Position): Position =
      pos.asInstanceOf[Position]

    def importSymbol(sym0: from.Symbol): Symbol = {
      def doImport(sym: from.Symbol): Symbol =
        symMap weakGet sym match {
          case Some(result) => result
          case _ =>
            val myowner = importSymbol(sym.owner)
            val mypos   = importPosition(sym.pos)
            val myname  = importName(sym.name).toTermName
            val myflags = sym.flags
            def linkReferenced(mysym: TermSymbol, x: from.TermSymbol, op: from.Symbol => Symbol): Symbol = {
              symMap.weakUpdate(x, mysym)
              mysym.referenced = op(x.referenced)
              mysym
            }
            val mysym = sym match {
              case x: from.MethodSymbol =>
                linkReferenced(myowner.newMethod(myname, mypos, myflags), x, importSymbol)
              case x: from.ModuleSymbol =>
                linkReferenced(myowner.newModuleSymbol(myname, mypos, myflags), x, importSymbol)
              case x: from.FreeTermSymbol =>
                newFreeTermSymbol(importName(x.name).toTermName, x.value, x.flags, x.origin) setInfo importType(x.info)
              case x: from.FreeTypeSymbol =>
                newFreeTypeSymbol(importName(x.name).toTypeName, x.flags, x.origin)
              case x: from.TermSymbol =>
                linkReferenced(myowner.newValue(myname, mypos, myflags), x, importSymbol)
              case x: from.TypeSkolem =>
                val origin = x.unpackLocation match {
                  case null           => null
                  case y: from.Tree   => importTree(y)
                  case y: from.Symbol => importSymbol(y)
                }
                myowner.newTypeSkolemSymbol(myname.toTypeName, origin, mypos, myflags)
              case x: from.ModuleClassSymbol =>
                val mysym = myowner.newModuleClass(myname.toTypeName, mypos, myflags)
                symMap.weakUpdate(x, mysym)
                mysym.sourceModule = importSymbol(x.sourceModule)
                mysym
              case x: from.ClassSymbol =>
                val mysym = myowner.newClassSymbol(myname.toTypeName, mypos, myflags)
                symMap.weakUpdate(x, mysym)
                if (sym.thisSym != sym) {
                  mysym.typeOfThis = importType(sym.typeOfThis)
                  mysym.thisSym setName importName(sym.thisSym.name)
                }
                mysym
              case x: from.TypeSymbol =>
                myowner.newTypeSymbol(myname.toTypeName, mypos, myflags)
            }
            symMap.weakUpdate(sym, mysym)
            mysym setFlag Flags.LOCKED
            mysym setInfo {
              val mytypeParams = sym.typeParams map importSymbol
              new LazyPolyType(mytypeParams) with FlagAgnosticCompleter {
                override def complete(s: Symbol) {
                  val result = sym.info match {
                    case from.PolyType(_, res) => res
                    case result => result
                  }
                  s setInfo GenPolyType(mytypeParams, importType(result))
                  s setAnnotations (sym.annotations map importAnnotationInfo)
                }
              }
            }
            mysym resetFlag Flags.LOCKED
        } // end doImport

      def importOrRelink: Symbol = {
        val sym = sym0 // makes sym visible in the debugger
        if (sym == null)
          null
        else if (sym == from.NoSymbol)
          NoSymbol
        else if (sym.isRoot)
          rootMirror.RootClass // !!! replace with actual mirror when we move importers to the mirror
        else {
          val name = sym.name
          val owner = sym.owner
          var scope = if (owner.isClass && !owner.isRefinementClass) owner.info else from.NoType
          var existing = scope.decl(name)
          if (sym.isModuleClass)
            existing = existing.moduleClass

          if (!existing.exists) scope = from.NoType

          val myname = importName(name)
          val myowner = importSymbol(owner)
          val myscope = if (scope != from.NoType && !(myowner hasFlag Flags.LOCKED)) myowner.info else NoType
          var myexisting = if (myscope != NoType) myowner.info.decl(myname) else NoSymbol // cannot load myexisting in general case, because it creates cycles for methods
          if (sym.isModuleClass)
            myexisting = importSymbol(sym.sourceModule).moduleClass

          if (!sym.isOverloaded && myexisting.isOverloaded) {
            myexisting =
              if (sym.isMethod) {
                val localCopy = doImport(sym)
                myexisting filter (_.tpe matches localCopy.tpe)
              } else {
                myexisting filter (!_.isMethod)
              }
            assert(!myexisting.isOverloaded,
                "import failure: cannot determine unique overloaded method alternative from\n "+
                (myexisting.alternatives map (_.defString) mkString "\n")+"\n that matches "+sym+":"+sym.tpe)
          }

          val mysym = {
            if (sym.isOverloaded) {
              myowner.newOverloaded(myowner.thisType, sym.alternatives map importSymbol)
            } else if (sym.isTypeParameter && sym.paramPos >= 0 && !(myowner hasFlag Flags.LOCKED)) {
              assert(myowner.typeParams.length > sym.paramPos,
                  "import failure: cannot determine parameter "+sym+" (#"+sym.paramPos+") in "+
                  myowner+typeParamsString(myowner.rawInfo)+"\n original symbol was: "+
                  sym.owner+from.typeParamsString(sym.owner.info))
              myowner.typeParams(sym.paramPos)
            } else {
              if (myexisting != NoSymbol) {
                myexisting
              } else {
                val mysym = doImport(sym)

                if (myscope != NoType) {
                  assert(myowner.info.decls.lookup(myname) == NoSymbol, myname+" "+myowner.info.decl(myname)+" "+myexisting)
                  myowner.info.decls enter mysym
                }

                mysym
              }
            }
          }

          mysym
        }
      } // end importOrRelink

      val sym = sym0
      symMap.weakGet(sym) match {
        case Some(result) => result
        case None =>
          pendingSyms += 1
          try {
            val result = importOrRelink
            symMap.weakUpdate(sym, result)
            result
          } finally {
            pendingSyms -= 1
            tryFixup()
          }
      }
    }

    def importType(tpe: from.Type): Type = {
      def doImport(tpe: from.Type): Type = tpe match {
        case from.TypeRef(pre, sym, args) =>
          TypeRef(importType(pre), importSymbol(sym), args map importType)
        case from.ThisType(clazz) =>
          ThisType(importSymbol(clazz))
        case from.SingleType(pre, sym) =>
          SingleType(importType(pre), importSymbol(sym))
        case from.MethodType(params, restpe) =>
          MethodType(params map importSymbol, importType(restpe))
        case from.PolyType(tparams, restpe) =>
          PolyType(tparams map importSymbol, importType(restpe))
        case from.NullaryMethodType(restpe) =>
          NullaryMethodType(importType(restpe))
        case from.ConstantType(constant @ from.Constant(_)) =>
          ConstantType(importConstant(constant))
        case from.SuperType(thistpe, supertpe) =>
          SuperType(importType(thistpe), importType(supertpe))
        case from.TypeBounds(lo, hi) =>
          TypeBounds(importType(lo), importType(hi))
        case from.BoundedWildcardType(bounds) =>
          BoundedWildcardType(importTypeBounds(bounds))
        case from.ClassInfoType(parents, decls, clazz) =>
          val myclazz = importSymbol(clazz)
          val myscope = if (myclazz.isPackageClass) newPackageScope(myclazz) else newScope
          val myclazzTpe = ClassInfoType(parents map importType, myscope, myclazz)
          myclazz setInfo GenPolyType(myclazz.typeParams, myclazzTpe) // needed so that newly created symbols find their scope
          decls foreach importSymbol // will enter itself into myclazz
          myclazzTpe
        case from.RefinedType(parents, decls) =>
          RefinedType(parents map importType, importScope(decls), importSymbol(tpe.typeSymbol))
        case from.ExistentialType(tparams, restpe) =>
          newExistentialType(tparams map importSymbol, importType(restpe))
        case from.OverloadedType(pre, alts) =>
          OverloadedType(importType(pre), alts map importSymbol)
        case from.AntiPolyType(pre, targs) =>
          AntiPolyType(importType(pre), targs map importType)
        case x: from.TypeVar =>
          TypeVar(importType(x.origin), importTypeConstraint(x.constr), x.typeArgs map importType, x.params map importSymbol)
        case from.NotNullType(tpe) =>
          NotNullType(importType(tpe))
        case from.AnnotatedType(annots, tpe, selfsym) =>
          AnnotatedType(annots map importAnnotationInfo, importType(tpe), importSymbol(selfsym))
        case from.ErrorType =>
          ErrorType
        case from.WildcardType =>
          WildcardType
        case from.NoType =>
          NoType
        case from.NoPrefix =>
          NoPrefix
        case null =>
          null
      } // end doImport

      def importOrRelink: Type =
        doImport(tpe)

      tpeMap.weakGet(tpe) match {
        case Some(result) => result
        case None =>
          pendingTpes += 1
          try {
            val result = importOrRelink
            tpeMap.weakUpdate(tpe, result)
            result
          } finally {
            pendingTpes -= 1
            tryFixup()
          }
      }
    }

    def importTypeBounds(bounds: from.TypeBounds) = importType(bounds).asInstanceOf[TypeBounds]

    def importAnnotationInfo(ann: from.AnnotationInfo): AnnotationInfo = {
      val atp1 = importType(ann.atp)
      val args1 = ann.args map importTree
      val assocs1 = ann.assocs map { case (name, arg) => (importName(name), importAnnotArg(arg)) }
      val original1 = importTree(ann.original)
      AnnotationInfo(atp1, args1, assocs1) setOriginal original1
    }

    def importAnnotArg(arg: from.ClassfileAnnotArg): ClassfileAnnotArg = arg match {
      case from.LiteralAnnotArg(constant @ from.Constant(_)) =>
        LiteralAnnotArg(importConstant(constant))
      case from.ArrayAnnotArg(args) =>
        ArrayAnnotArg(args map importAnnotArg)
      case from.ScalaSigBytes(bytes) =>
        ScalaSigBytes(bytes)
      case from.NestedAnnotArg(annInfo) =>
        NestedAnnotArg(importAnnotationInfo(annInfo))
    }

    def importTypeConstraint(constr: from.TypeConstraint): TypeConstraint = {
      val result = new TypeConstraint(constr.loBounds map importType, constr.hiBounds map importType)
      result.inst = importType(constr.inst)
      result
    }

    // !!! todo: override to cater for PackageScopes
    def importScope(decls: from.Scope): Scope =
      newScopeWith(decls.toList map importSymbol: _*)

    def importName(name: from.Name): Name =
      if (name.isTypeName) newTypeName(name.toString) else newTermName(name.toString)
    def importTypeName(name: from.TypeName): TypeName = importName(name).toTypeName
    def importTermName(name: from.TermName): TermName = importName(name).toTermName

    def importModifiers(mods: from.Modifiers): Modifiers =
      new Modifiers(mods.flags, importName(mods.privateWithin), mods.annotations map importTree)

    def importImportSelector(sel: from.ImportSelector): ImportSelector =
      new ImportSelector(importName(sel.name), sel.namePos, if (sel.rename != null) importName(sel.rename) else null, sel.renamePos)

    def importTree(tree: from.Tree): Tree = {
      val mytree = tree match {
        case from.ClassDef(mods, name, tparams, impl) =>
          new ClassDef(importModifiers(mods), importName(name).toTypeName, tparams map importTypeDef, importTemplate(impl))
        case from.PackageDef(pid, stats) =>
          new PackageDef(importRefTree(pid), stats map importTree)
        case from.ModuleDef(mods, name, impl) =>
          new ModuleDef(importModifiers(mods), importName(name).toTermName, importTemplate(impl))
        case from.emptyValDef =>
          emptyValDef
        case from.ValDef(mods, name, tpt, rhs) =>
          new ValDef(importModifiers(mods), importName(name).toTermName, importTree(tpt), importTree(rhs))
        case from.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          new DefDef(importModifiers(mods), importName(name).toTermName, tparams map importTypeDef, mmap(vparamss)(importValDef), importTree(tpt), importTree(rhs))
        case from.TypeDef(mods, name, tparams, rhs) =>
          new TypeDef(importModifiers(mods), importName(name).toTypeName, tparams map importTypeDef, importTree(rhs))
        case from.LabelDef(name, params, rhs) =>
          new LabelDef(importName(name).toTermName, params map importIdent, importTree(rhs))
        case from.Import(expr, selectors) =>
          new Import(importTree(expr), selectors map importImportSelector)
        case from.Template(parents, self, body) =>
          new Template(parents map importTree, importValDef(self), body map importTree)
        case from.Block(stats, expr) =>
          new Block(stats map importTree, importTree(expr))
        case from.CaseDef(pat, guard, body) =>
          new CaseDef(importTree(pat), importTree(guard), importTree(body))
        case from.Alternative(trees) =>
          new Alternative(trees map importTree)
        case from.Star(elem) =>
          new Star(importTree(elem))
        case from.Bind(name, body) =>
          new Bind(importName(name), importTree(body))
        case from.UnApply(fun, args) =>
          new UnApply(importTree(fun), args map importTree)
        case from.ArrayValue(elemtpt ,elems) =>
          new ArrayValue(importTree(elemtpt), elems map importTree)
        case from.Function(vparams, body) =>
          new Function(vparams map importValDef, importTree(body))
        case from.Assign(lhs, rhs) =>
          new Assign(importTree(lhs), importTree(rhs))
        case from.AssignOrNamedArg(lhs, rhs) =>
          new AssignOrNamedArg(importTree(lhs), importTree(rhs))
        case from.If(cond, thenp, elsep) =>
          new If(importTree(cond), importTree(thenp), importTree(elsep))
        case from.Match(selector, cases) =>
          new Match(importTree(selector), cases map importCaseDef)
        case from.Return(expr) =>
          new Return(importTree(expr))
        case from.Try(block, catches, finalizer) =>
          new Try(importTree(block), catches map importCaseDef, importTree(finalizer))
        case from.Throw(expr) =>
          new Throw(importTree(expr))
        case from.New(tpt) =>
          new New(importTree(tpt))
        case from.Typed(expr, tpt) =>
          new Typed(importTree(expr), importTree(tpt))
        case from.TypeApply(fun, args) =>
          new TypeApply(importTree(fun), args map importTree)
        case from.Apply(fun, args) => tree match {
          case _: from.ApplyToImplicitArgs =>
            new ApplyToImplicitArgs(importTree(fun), args map importTree)
          case _: from.ApplyImplicitView =>
            new ApplyImplicitView(importTree(fun), args map importTree)
          case _ =>
            new Apply(importTree(fun), args map importTree)
        }
        case from.ApplyDynamic(qual, args) =>
          new ApplyDynamic(importTree(qual), args map importTree)
        case from.Super(qual, mix) =>
          new Super(importTree(qual), importTypeName(mix))
        case from.This(qual) =>
          new This(importName(qual).toTypeName)
        case from.Select(qual, name) =>
          new Select(importTree(qual), importName(name))
        case from.Ident(name) =>
          new Ident(importName(name))
        case from.ReferenceToBoxed(ident) =>
          new ReferenceToBoxed(importTree(ident) match { case ident: Ident => ident })
        case from.Literal(constant @ from.Constant(_)) =>
          new Literal(importConstant(constant))
        case from.TypeTree() =>
          new TypeTree()
        case from.Annotated(annot, arg) =>
          new Annotated(importTree(annot), importTree(arg))
        case from.SingletonTypeTree(ref) =>
          new SingletonTypeTree(importTree(ref))
        case from.SelectFromTypeTree(qual, name) =>
          new SelectFromTypeTree(importTree(qual), importName(name).toTypeName)
        case from.CompoundTypeTree(templ) =>
          new CompoundTypeTree(importTemplate(templ))
        case from.AppliedTypeTree(tpt, args) =>
          new AppliedTypeTree(importTree(tpt), args map importTree)
        case from.TypeBoundsTree(lo, hi) =>
          new TypeBoundsTree(importTree(lo), importTree(hi))
        case from.ExistentialTypeTree(tpt, whereClauses) =>
          new ExistentialTypeTree(importTree(tpt), whereClauses map importTree)
        case from.EmptyTree =>
          EmptyTree
        case null =>
          null
      }
      addFixup({
        if (mytree != null) {
          val mysym = if (tree.hasSymbol) importSymbol(tree.symbol) else NoSymbol
          val mytpe = importType(tree.tpe)

          mytree match {
            case mytt: TypeTree =>
              val tt = tree.asInstanceOf[from.TypeTree]
              if (mytree.hasSymbol) mytt.symbol = mysym
              if (tt.wasEmpty) mytt.defineType(mytpe) else mytt.setType(mytpe)
              if (tt.original != null) mytt.setOriginal(importTree(tt.original))
            case _ =>
              if (mytree.hasSymbol) mytree.symbol = importSymbol(tree.symbol)
              mytree.tpe = importType(tree.tpe)
          }
        }
      })
      tryFixup()
      // we have to be careful with position import as some shared trees
      // like EmptyTree, emptyValDef don't support position assignment
      if (tree.pos != NoPosition)
        mytree.setPos(importPosition(tree.pos))
      else
        mytree
    }

    def importValDef(tree: from.ValDef): ValDef = importTree(tree).asInstanceOf[ValDef]
    def importTypeDef(tree: from.TypeDef): TypeDef = importTree(tree).asInstanceOf[TypeDef]
    def importTemplate(tree: from.Template): Template = importTree(tree).asInstanceOf[Template]
    def importRefTree(tree: from.RefTree): RefTree = importTree(tree).asInstanceOf[RefTree]
    def importIdent(tree: from.Ident): Ident = importTree(tree).asInstanceOf[Ident]
    def importCaseDef(tree: from.CaseDef): CaseDef = importTree(tree).asInstanceOf[CaseDef]
    def importConstant(constant: from.Constant): Constant = new Constant(constant.tag match {
      case ClazzTag => importType(constant.value.asInstanceOf[from.Type])
      case EnumTag => importSymbol(constant.value.asInstanceOf[from.Symbol])
      case _ => constant.value
    })
  }
}
