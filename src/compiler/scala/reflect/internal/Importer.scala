package scala.reflect
package internal
import com.sun.source.tree.ImportTree
import scala.collection.mutable.WeakHashMap

trait Importers { self: SymbolTable =>

  class Importer(val symMap: WeakHashMap[SymbolTable # Symbol, Symbol] = new WeakHashMap[SymbolTable # Symbol, Symbol]) {

    def importPosition(pos: SymbolTable # Position): Position = NoPosition

    def importSymbol(sym: SymbolTable#Symbol): Symbol = {
      def importOrRelink: Symbol =
        if (sym == null)
          null
        else if (sym.getClass == NoSymbol.getClass)
          NoSymbol
        else if (sym.isRoot)
          definitions.RootClass
        else {
          val myowner = importSymbol(sym.owner)
          val mypos = importPosition(sym.pos)
          val myname = importName(sym.name)

          def doImport: Symbol = {
            val mysym = sym match {
              case x: SymbolTable # MethodSymbol =>
                val mysym = new MethodSymbol(myowner, mypos, myname)
                mysym.referenced = importSymbol(x.referenced)
                mysym
              case x: SymbolTable # ModuleSymbol =>
                new ModuleSymbol(myowner, mypos, myname)
              case x: SymbolTable # TermSymbol =>
                new TermSymbol(myowner, mypos, myname)
              case x: SymbolTable # TypeSkolem =>
                new TypeSkolem(myowner, mypos, myname.toTypeName, x.unpackLocation match {
                  case null => null
                  case y: Tree => importTree(y)
                  case y: Symbol => importSymbol(y)
                })
/*
              case x: SymbolTable#ModuleClassSymbol =>
                val mysym = new ModuleClassSymbol(myowner, mypos, myname.toTypeName)
                mysym.sourceModule = importSymbol(x.sourceModule)
                mysym
*/
              case x: SymbolTable # ClassSymbol =>
                val mysym = new ClassSymbol(myowner, mypos, myname.toTypeName)
                if (sym.thisSym != sym) {
                  mysym.typeOfThis = importType(sym.typeOfThis)
                  mysym.thisSym.name = importName(sym.thisSym.name)
                }
                mysym
              case x: SymbolTable # TypeSymbol =>
                new TypeSymbol(myowner, mypos, myname.toTypeName)
            }
            symMap(sym) = mysym
            mysym setFlag sym.flags
            mysym setInfo {
              val mytypeParams = sym.typeParams map importSymbol
              new LazyPolyType(mytypeParams) {
                override def complete(s: Symbol) {
                  val result = sym.info match {
                    case PolyType(_, result) => result
                    case result => result
                  }
                  s setInfo polyType(mytypeParams, importType(result))
                  s setAnnotations (sym.annotations map importAnnotationInfo)
                }
              }
            }
          } // end doImport

          if (myowner.isClass && !myowner.isRefinementClass && sym.owner.lookup(sym.name).exists) {
            // symbol is in class scope, try to find equivalent one in local scope
            if (sym.isOverloaded)
              myowner.newOverloaded(myowner.thisType, sym.alternatives map importSymbol)
            else {
              var existing: Symbol = myowner.lookup(myname)
              if (existing.isOverloaded) {
                existing =
                  if (sym.isMethod) {
                    val localCopy = doImport
                    existing filter (_.tpe matchesType localCopy.tpe)
                  } else {
                    existing filter (!_.isMethod)
                  }
                assert(!existing.isOverloaded)
              }
              if (existing.exists) existing
              else {
                val mysym = doImport
                myowner.info.decls enter mysym
                mysym
              }
            }
          } else if (sym.isTypeParameter && sym.paramPos >= 0)
            myowner.typeParams(sym.paramPos)
          else
            doImport
        }
      symMap getOrElseUpdate (sym, importOrRelink)
    }

    def importType(tpe: SymbolTable#Type): Type = tpe match {
      case x: SymbolTable#TypeRef =>
        TypeRef(importType(x.pre), importSymbol(x.sym), x.args map importType)
      case x: SymbolTable#ThisType =>
        ThisType(importSymbol(x.typeSymbol))
      case x: SymbolTable#SingleType =>
        SingleType(importType(x.pre), importSymbol(x.sym))
      case x: SymbolTable#MethodType =>
        MethodType(x.params map importSymbol, importType(x.resultType))
      case x: SymbolTable#PolyType =>
        PolyType(x.typeParams map importSymbol, importType(x.resultType))
      case x: SymbolTable#NullaryMethodType =>
        NullaryMethodType(importType(x.resultType))
      case x: SymbolTable#ConstantType =>
        ConstantType(Constant(x.value.value))
      case x: SymbolTable#SuperType =>
        SuperType(importType(x.thistpe), importType(x.supertpe))
      case x: SymbolTable#TypeBounds =>
        TypeBounds(importType(x.lo), importType(x.hi))
      case x: SymbolTable#BoundedWildcardType =>
        BoundedWildcardType(importTypeBounds(x.bounds))
      case x: SymbolTable#ClassInfoType =>
        val myclazz = importSymbol(x.typeSymbol)
        val myscope = if (myclazz.isPackageClass) newPackageScope(myclazz) else newScope
        val myclazzTpe = ClassInfoType(x.parents map importType, myscope, myclazz)
        myclazz setInfo polyType(myclazz.typeParams, myclazzTpe) // needed so that newly created symbols find their scope
        for (sym <- x.decls) importSymbol(sym) // will enter itself into myclazz
        myclazzTpe
      case x: SymbolTable#RefinedType =>
        RefinedType(x.parents map importType, importScope(x.decls), importSymbol(x.typeSymbol))
      case x: SymbolTable#ExistentialType =>
        ExistentialType(x.typeParams map importSymbol, importType(x.resultType))
      case x: SymbolTable#OverloadedType =>
        OverloadedType(importType(x.pre), x.alternatives map importSymbol)
      case x: SymbolTable#AntiPolyType =>
        AntiPolyType(importType(x.pre), x.targs map importType)
      case x: SymbolTable#TypeVar =>
        new TypeVar(importType(x.origin), importTypeConstraint(x.constr0), x.typeArgs map importType, x.params map importSymbol)
      case x: SymbolTable#NotNullType =>
        NotNullType(importType(x.underlying))
      case x: SymbolTable#AnnotatedType =>
        AnnotatedType(x.annotations map importAnnotationInfo, importType(x.underlying), importSymbol(x.selfsym))
      case x =>
        val xclazz = x.getClass
        if (xclazz == ErrorType.getClass) ErrorType
        else if (xclazz == WildcardType.getClass) WildcardType
        else if (xclazz == NoType.getClass) NoType
        else if (xclazz == NoPrefix.getClass) NoPrefix
        else if (x == null) null
        else throw new MatchError(x)
    }

    def importTypeBounds(bounds: SymbolTable#TypeBounds) = importType(bounds).asInstanceOf[TypeBounds]

    def importAnnotationInfo(ann: SymbolTable#AnnotationInfo): AnnotationInfo =
      AnnotationInfo(importType(ann.atp), ann.args map importTree, ann.assocs map {
        case (name, arg) => (importName(name), importAnnotArg(arg))
      })

    def importAnnotArg(arg: SymbolTable#ClassfileAnnotArg): ClassfileAnnotArg = arg match {
      case x: SymbolTable#LiteralAnnotArg =>
        LiteralAnnotArg(Constant(x.const.value))
      case x: SymbolTable#ArrayAnnotArg =>
        ArrayAnnotArg(x.args map importAnnotArg)
      case x: SymbolTable#ScalaSigBytes =>
        ScalaSigBytes(x.bytes)
      case x: SymbolTable#NestedAnnotArg =>
        NestedAnnotArg(importAnnotationInfo(x.annInfo))
    }

    def importTypeConstraint(constr: SymbolTable#TypeConstraint): TypeConstraint = {
      val result = new TypeConstraint(constr.loBounds map importType, constr.hiBounds map importType)
      result.inst = importType(constr.inst)
      result
    }

    // !!! todo: override to vcater for PackageScopes
    def importScope(decls: SymbolTable#Scope): Scope =
      new Scope(decls.toList map importSymbol)

    def importName(name: SymbolTable#Name): Name =
      if (name.isTypeName) newTypeName(name.toString) else newTermName(name.toString)
    def importTypeName(name: SymbolTable#TypeName): TypeName = importName(name).toTypeName
    def importTermName(name: SymbolTable#TermName): TermName = importName(name).toTermName

    def importModifiers(mods: SymbolTable#Modifiers): Modifiers =
      new Modifiers(mods.flags, importName(mods.privateWithin), mods.annotations map importTree)

    def importImportSelector(sel: SymbolTable#ImportSelector): ImportSelector =
      new ImportSelector(importName(sel.name), sel.namePos, importName(sel.rename), sel.renamePos)

    def importTree(tree: SymbolTable#Tree): Tree = {
      tree match {
        case x: SymbolTable#ClassDef =>
          new ClassDef(importModifiers(x.mods), importName(x.name).toTypeName, x.tparams map importTypeDef, importTemplate(x.impl))
        case x: SymbolTable#PackageDef =>
          new PackageDef(importRefTree(x.pid), x.stats map importTree)
        case x: SymbolTable#ModuleDef =>
          new ModuleDef(importModifiers(x.mods), importName(x.name).toTermName, importTemplate(x.impl))
        case x: SymbolTable#ValDef =>
          new ValDef(importModifiers(x.mods), importName(x.name).toTermName, importTree(x.tpt), importTree(x.rhs))
        case x: SymbolTable#DefDef =>
          new DefDef(importModifiers(x.mods), importName(x.name).toTermName, x.tparams map importTypeDef, x.vparamss map (_ map importValDef), importTree(x.tpt), importTree(x.rhs))
        case x: SymbolTable#TypeDef =>
          new TypeDef(importModifiers(x.mods), importName(x.name).toTypeName, x.tparams map importTypeDef, importTree(x.rhs))
        case x: SymbolTable#LabelDef =>
          new LabelDef(importName(x.name).toTermName, x.params map importIdent, importTree(x.rhs))
        case x: SymbolTable#Import =>
          new Import(importTree(x.expr), x.selectors map importImportSelector)
        case x: SymbolTable#Template =>
          new Template(x.parents map importTree, importValDef(x.self), x.body map importTree)
        case x: SymbolTable#Block =>
          new Block(x.stats map importTree, importTree(x.expr))
        case x: SymbolTable#CaseDef =>
          new CaseDef(importTree(x.pat), importTree(x.guard), importTree(x.body))
        case x: SymbolTable#Alternative =>
          new Alternative(x.trees map importTree)
        case x: SymbolTable#Star =>
          new Star(importTree(x.elem))
        case x: SymbolTable#Bind =>
          new Bind(importName(x.name), importTree(x.body))
        case x: SymbolTable#UnApply =>
          new UnApply(importTree(x.fun), x.args map importTree)
        case x: SymbolTable#ArrayValue =>
          new ArrayValue(importTree(x.elemtpt), x.elems map importTree)
        case x: SymbolTable#Function =>
          new Function(x.vparams map importValDef, importTree(x.body))
        case x: SymbolTable#Assign =>
          new Assign(importTree(x.lhs), importTree(x.rhs))
        case x: SymbolTable#If =>
          new If(importTree(x.cond), importTree(x.thenp), importTree(x.elsep))
        case x: SymbolTable#Match =>
          new Match(importTree(x.selector), x.cases map importCaseDef)
        case x: SymbolTable#Return =>
          new Return(importTree(x.expr))
        case x: SymbolTable#Try =>
          new Try(importTree(x.block), x.catches map importCaseDef, importTree(x.finalizer))
        case x: SymbolTable#Throw =>
          new Throw(importTree(x.expr))
        case x: SymbolTable#New =>
          new New(importTree(x.tpt))
        case x: SymbolTable#Typed =>
          new Typed(importTree(x.expr), importTree(x.tpt))
        case x: SymbolTable#TypeApply =>
          new TypeApply(importTree(x.fun), x.args map importTree)
        case x: SymbolTable#ApplyToImplicitArgs =>
          new ApplyToImplicitArgs(importTree(x.fun), x.args map importTree)
        case x: SymbolTable#ApplyImplicitView =>
          new ApplyImplicitView(importTree(x.fun), x.args map importTree)
        case x: SymbolTable#Apply =>
          new Apply(importTree(x.fun), x.args map importTree)
        case x: SymbolTable#ApplyDynamic =>
          new ApplyDynamic(importTree(x.qual), x.args map importTree)
        case x: SymbolTable#Super =>
          new Super(importTree(x.qual), importTypeName(x.mix))
        case x: SymbolTable#This =>
          new This(importName(x.qual).toTypeName)
        case x: SymbolTable#Select =>
          new Select(importTree(x.qualifier), importName(x.name))
        case x: SymbolTable#BackQuotedIdent =>
          new BackQuotedIdent(importName(x.name))
        case x: SymbolTable#Ident =>
          new Ident(importName(x.name))
        case x: SymbolTable#Literal =>
          new Literal(Constant(x.value))
        case x: SymbolTable#TypeTree =>
          new TypeTree()
        case x: SymbolTable#Annotated =>
          new Annotated(importTree(x.annot), importTree(x.arg))
        case x: SymbolTable#SingletonTypeTree =>
          new SingletonTypeTree(importTree(x.ref))
        case x: SymbolTable#SelectFromTypeTree =>
          new SelectFromTypeTree(importTree(x.qualifier), importName(x.name).toTypeName)
        case x: SymbolTable#CompoundTypeTree =>
          new CompoundTypeTree(importTemplate(x.templ))
        case x: SymbolTable#AppliedTypeTree =>
          new AppliedTypeTree(importTree(x.tpt), x.args map importTree)
        case x: SymbolTable#TypeBoundsTree =>
          new TypeBoundsTree(importTree(x.lo), importTree(x.hi))
        case x: SymbolTable#ExistentialTypeTree =>
          new ExistentialTypeTree(importTree(x.tpt), x.whereClauses map importTree)
        case x =>
          if (x.getClass == EmptyTree.getClass) EmptyTree
          else if (x == null) null
          else throw new MatchError(x)
      }
    } // copyAttrs tree

    def importValDef(tree: SymbolTable#ValDef): ValDef = importTree(tree).asInstanceOf[ValDef]
    def importTypeDef(tree: SymbolTable#TypeDef): TypeDef = importTree(tree).asInstanceOf[TypeDef]
    def importTemplate(tree: SymbolTable#Template): Template = importTree(tree).asInstanceOf[Template]
    def importRefTree(tree: SymbolTable#RefTree): RefTree = importTree(tree).asInstanceOf[RefTree]
    def importIdent(tree: SymbolTable#Ident): Ident = importTree(tree).asInstanceOf[Ident]
    def importCaseDef(tree: SymbolTable#CaseDef): CaseDef = importTree(tree).asInstanceOf[CaseDef]
  }
}
