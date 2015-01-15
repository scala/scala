package scala
package reflect
package internal

import scala.collection.mutable.WeakHashMap
import scala.ref.WeakReference
import scala.reflect.internal.Flags._

// SI-6241: move importers to a mirror
trait Importers { to: SymbolTable =>

  override def mkImporter(from0: api.Universe): Importer { val from: from0.type } = (
    if (to eq from0) {
      new Importer {
        val from = from0
        val reverse = this.asInstanceOf[from.Importer{ val from: to.type }]
        def importSymbol(their: from.Symbol) = their.asInstanceOf[to.Symbol]
        def importType(their: from.Type) = their.asInstanceOf[to.Type]
        def importTree(their: from.Tree) = their.asInstanceOf[to.Tree]
        def importPosition(their: from.Position) = their.asInstanceOf[to.Position]
      }
    } else {
      // todo. fix this loophole
      assert(from0.isInstanceOf[SymbolTable], "`from` should be an instance of scala.reflect.internal.SymbolTable")
      new StandardImporter { val from = from0.asInstanceOf[SymbolTable] }
    }
  ).asInstanceOf[Importer { val from: from0.type }]

  abstract class StandardImporter extends Importer {

    val from: SymbolTable

    protected lazy val symMap = new Cache[from.Symbol, to.Symbol]()
    protected lazy val tpeMap = new Cache[from.Type, to.Type]()
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
      val from: to.type = to
      // FIXME this and reverse should be constantly kept in sync
      // not just synced once upon the first usage of reverse
      for ((theirsym, WeakReference(mysym)) <- StandardImporter.this.symMap) symMap += ((mysym, WeakReference(theirsym)))
      for ((theirtpe, WeakReference(mytpe)) <- StandardImporter.this.tpeMap) tpeMap += ((mytpe, WeakReference(theirtpe)))
    }

    // ============== SYMBOLS ==============

    protected def recreatedSymbolCompleter(my: to.Symbol, their: from.Symbol) = {
      // we lock the symbol that is imported for a very short period of time
      // i.e. only for when type parameters of the symbol are being imported
      // the lock is used to communicate to the recursive importSymbol calls
      // that type parameters need to be created from scratch
      // because otherwise type parameters are imported by looking into owner.typeParams
      // which is obviously unavailable while the completer is being created
      try {
        my setFlag Flags.LOCKED
        val mytypeParams = their.typeParams map importSymbol
        new LazyPolyType(mytypeParams) with FlagAgnosticCompleter {
          override def complete(my: to.Symbol): Unit = {
            val theirCore = their.info match {
              case from.PolyType(_, core) => core
              case core => core
            }
            my setInfo GenPolyType(mytypeParams, importType(theirCore))
            my setAnnotations (their.annotations map importAnnotationInfo)
            markAllCompleted(my)
          }
        }
      } finally {
        my resetFlag Flags.LOCKED
      }
    }

    protected def recreateSymbol(their: from.Symbol): to.Symbol = {
      val myowner = importSymbol(their.owner)
      val mypos   = importPosition(their.pos)
      val myname  = importName(their.name)
      val myflags = their.flags
      def linkReferenced(my: TermSymbol, their: from.TermSymbol, op: from.Symbol => Symbol): Symbol = {
        symMap.weakUpdate(their, my)
        my.referenced = op(their.referenced)
        my
      }
      val my = their match {
        case their: from.MethodSymbol =>
          linkReferenced(myowner.newMethod(myname.toTermName, mypos, myflags), their, importSymbol)
        case their: from.ModuleSymbol =>
          val ret = linkReferenced(myowner.newModuleSymbol(myname.toTermName, mypos, myflags), their, importSymbol)
          ret.associatedFile = their.associatedFile
          ret
        case their: from.FreeTermSymbol =>
          newFreeTermSymbol(myname.toTermName, their.value, their.flags, their.origin) setInfo importType(their.info)
        case their: from.FreeTypeSymbol =>
          newFreeTypeSymbol(myname.toTypeName, their.flags, their.origin)
        case their: from.TermSymbol =>
          linkReferenced(myowner.newValue(myname.toTermName, mypos, myflags), their, importSymbol)
        case their: from.TypeSkolem =>
          val origin = their.unpackLocation match {
            case null                  => null
            case theirloc: from.Tree   => importTree(theirloc)
            case theirloc: from.Symbol => importSymbol(theirloc)
          }
          myowner.newTypeSkolemSymbol(myname.toTypeName, origin, mypos, myflags)
        case their: from.ModuleClassSymbol =>
          val my = myowner.newModuleClass(myname.toTypeName, mypos, myflags)
          symMap.weakUpdate(their, my)
          my.sourceModule = importSymbol(their.sourceModule)
          my
        case their: from.ClassSymbol =>
          val my = myowner.newClassSymbol(myname.toTypeName, mypos, myflags)
          symMap.weakUpdate(their, my)
          if (their.thisSym != their) {
            my.typeOfThis = importType(their.typeOfThis)
            my.thisSym setName importName(their.thisSym.name)
          }
          my.associatedFile = their.associatedFile
          my
        case their: from.TypeSymbol =>
          myowner.newTypeSymbol(myname.toTypeName, mypos, myflags)
      }
      symMap.weakUpdate(their, my)
      markFlagsCompleted(my)(mask = AllFlags)
      my setInfo recreatedSymbolCompleter(my, their)
    }

    def importSymbol(their0: from.Symbol): Symbol = {
      def cachedRecreateSymbol(their: from.Symbol): Symbol =
        symMap weakGet their match {
          case Some(result) => result
          case _ => recreateSymbol(their)
        }

      def recreateOrRelink: Symbol = {
        val their = their0 // makes their visible in the debugger
        if (their == null)
          null
        else if (their == from.NoSymbol)
          NoSymbol
        else if (their.isRoot)
          rootMirror.RootClass // !!! replace with actual mirror when we move importers to the mirror
        else {
          val isModuleClass = their.isModuleClass
          val isTparam = their.isTypeParameter && their.paramPos >= 0
          val isOverloaded = their.isOverloaded

          var theirscope = if (their.owner.isClass && !their.owner.isRefinementClass) their.owner.info else from.NoType
          val theirexisting = if (isModuleClass) theirscope.decl(their.name).moduleClass else theirscope.decl(their.name)
          if (!theirexisting.exists) theirscope = from.NoType

          val myname = importName(their.name)
          val myowner = importSymbol(their.owner)
          val myscope = if (theirscope != from.NoType && !(myowner hasFlag Flags.LOCKED)) myowner.info else NoType
          val myexisting = {
            if (isModuleClass) importSymbol(their.sourceModule).moduleClass
            else if (isTparam) (if (myowner hasFlag Flags.LOCKED) NoSymbol else myowner.typeParams(their.paramPos))
            else if (isOverloaded) myowner.newOverloaded(myowner.thisType, their.alternatives map importSymbol)
            else {
              def disambiguate(my: Symbol) = {
                val result =
                  if (their.isMethod) {
                    val localCopy = cachedRecreateSymbol(their)
                    my filter (_.tpe matches localCopy.tpe)
                  } else {
                    my filter (!_.isMethod)
                  }
                assert(!result.isOverloaded,
                    "import failure: cannot determine unique overloaded method alternative from\n "+
                    (result.alternatives map (_.defString) mkString "\n")+"\n that matches "+their+":"+their.tpe)
                result
              }

              val myexisting = if (myscope != NoType) myscope.decl(myname) else NoSymbol
              if (myexisting.isOverloaded) disambiguate(myexisting)
              else myexisting
            }
          }

          myexisting.orElse {
            val my = cachedRecreateSymbol(their)
            if (myscope != NoType) {
              assert(myscope.decls.lookup(myname) == NoSymbol, myname+" "+myscope.decl(myname)+" "+myexisting)
              myscope.decls enter my
            }
            my
          }
        }
      } // end recreateOrRelink

      val their = their0
      symMap.weakGet(their) match {
        case Some(result) => result
        case None =>
          pendingSyms += 1
          try {
            val result = recreateOrRelink
            symMap.weakUpdate(their, result)
            result
          } finally {
            pendingSyms -= 1
            tryFixup()
          }
      }
    }

    // ============== TYPES ==============

    def recreateType(their: from.Type): Type = their match {
      case from.TypeRef(pre, sym, args) =>
        TypeRef(importType(pre), importSymbol(sym), args map importType)
      case from.ThisType(clazz) =>
        ThisType(importSymbol(clazz))
      case from.SingleType(pre, sym) =>
        SingleType(importType(pre), importSymbol(sym))
      case from.MethodType(params, result) =>
        MethodType(params map importSymbol, importType(result))
      case from.PolyType(tparams, result) =>
        PolyType(tparams map importSymbol, importType(result))
      case from.NullaryMethodType(result) =>
        NullaryMethodType(importType(result))
      case from.ConstantType(constant @ from.Constant(_)) =>
        ConstantType(importConstant(constant))
      case from.SuperType(thistpe, supertpe) =>
        SuperType(importType(thistpe), importType(supertpe))
      case from.TypeBounds(lo, hi) =>
        TypeBounds(importType(lo), importType(hi))
      case from.BoundedWildcardType(bounds) =>
        BoundedWildcardType(importType(bounds).asInstanceOf[TypeBounds])
      case from.ClassInfoType(parents, decls, clazz) =>
        val myclazz = importSymbol(clazz)
        val myscope = if (myclazz.isPackageClass) newPackageScope(myclazz) else newScope
        val myclazzTpe = ClassInfoType(parents map importType, myscope, myclazz)
        myclazz setInfo GenPolyType(myclazz.typeParams, myclazzTpe) // needed so that newly created symbols find their scope
        decls foreach importSymbol // will enter itself into myclazz
        myclazzTpe
      case from.RefinedType(parents, decls) =>
        RefinedType(parents map importType, importScope(decls), importSymbol(their.typeSymbol))
      case from.ExistentialType(tparams, result) =>
        newExistentialType(tparams map importSymbol, importType(result))
      case from.OverloadedType(pre, alts) =>
        OverloadedType(importType(pre), alts map importSymbol)
      case from.ImportType(qual) =>
        ImportType(importTree(qual))
      case from.AntiPolyType(pre, targs) =>
        AntiPolyType(importType(pre), targs map importType)
      case their: from.TypeVar =>
        val myconstr = new TypeConstraint(their.constr.loBounds map importType, their.constr.hiBounds map importType)
        myconstr.inst = importType(their.constr.inst)
        TypeVar(importType(their.origin), myconstr, their.typeArgs map importType, their.params map importSymbol)
      case from.AnnotatedType(annots, result) =>
        AnnotatedType(annots map importAnnotationInfo, importType(result))
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
    }

    def importType(their: from.Type): Type = {
      tpeMap.weakGet(their) match {
        case Some(result) => result
        case None =>
          pendingTpes += 1
          try {
            val result = recreateType(their)
            tpeMap.weakUpdate(their, result)
            result
          } finally {
            pendingTpes -= 1
            tryFixup()
          }
      }
    }

    // ============== TREES ==============

    def recreatedTreeCompleter(their: from.Tree, my: to.Tree): Unit = {
      if (their.canHaveAttrs) {
        if (my.hasSymbolField) my.symbol = importSymbol(their.symbol)
        my.pos = importPosition(their.pos)
        (their, my) match {
          case (their: from.TypeTree, my: to.TypeTree) =>
            if (their.wasEmpty) my.defineType(importType(their.tpe)) else my.setType(importType(their.tpe))
          case (_, _) =>
            my.setType(importType(their.tpe))
        }
      }
    }

    def recreateTree(their: from.Tree): to.Tree = their match {
      case from.ClassDef(mods, name, tparams, impl) =>
        new ClassDef(importModifiers(mods), importName(name).toTypeName, tparams map importTypeDef, importTemplate(impl))
      case from.PackageDef(pid, stats) =>
        new PackageDef(importRefTree(pid), stats map importTree)
      case from.ModuleDef(mods, name, impl) =>
        new ModuleDef(importModifiers(mods), importName(name).toTermName, importTemplate(impl))
      case from.noSelfType =>
        noSelfType
      case from.pendingSuperCall =>
        pendingSuperCall
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
      case from.Apply(fun, args) => their match {
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
        new Super(importTree(qual), importName(mix).toTypeName)
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
      case theirtt @ from.TypeTree() =>
        val mytt = TypeTree()
        if (theirtt.original != null) mytt.setOriginal(importTree(theirtt.original))
        mytt
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
        new ExistentialTypeTree(importTree(tpt), whereClauses map importMemberDef)
      case from.EmptyTree =>
        EmptyTree
      case null =>
        null
    }

    def importTree(their: from.Tree): Tree = {
      val my = recreateTree(their)
      if (my != null) {
        addFixup(recreatedTreeCompleter(their, my))
        tryFixup()
        // we have to be careful with position import as some shared trees
        // like EmptyTree, noSelfType don't support position assignment
        if (their.pos != NoPosition) {
          my.setPos(importPosition(their.pos))
        }
      }
      importAttachments(their.attachments.all).foreach { my.updateAttachment(_) }
      my
    }

    // ============== MISCELLANEOUS ==============

    def importAttachments(attachments: Set[Any]): Set[Any] =
      attachments.collect { case ia: ImportableAttachment => ia.importAttachment(this) }

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
      case from.UnmappableAnnotArg =>
        UnmappableAnnotArg
    }

    // todo. careful import of positions
    def importPosition(their: from.Position): to.Position =
      their.asInstanceOf[Position]

    // !!! todo: override to cater for PackageScopes
    def importScope(decls: from.Scope): Scope =
      newScopeWith(decls.toList map importSymbol: _*)

    def importName(name: from.Name): Name =
      if (name.isTypeName) newTypeName(name.toString) else newTermName(name.toString)

    def importModifiers(mods: from.Modifiers): Modifiers =
      new Modifiers(mods.flags, importName(mods.privateWithin), mods.annotations map importTree)

    def importImportSelector(sel: from.ImportSelector): ImportSelector =
      new ImportSelector(importName(sel.name), sel.namePos, if (sel.rename != null) importName(sel.rename) else null, sel.renamePos)
    def importValDef(tree: from.ValDef): ValDef = importTree(tree).asInstanceOf[ValDef]
    def importTypeDef(tree: from.TypeDef): TypeDef = importTree(tree).asInstanceOf[TypeDef]
    def importMemberDef(tree: from.MemberDef): MemberDef = importTree(tree).asInstanceOf[MemberDef]
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
