package scala
package reflect
package internal

import Flags._
import util._

trait ReificationSupport { self: SymbolTable =>
  import definitions._

  class ReificationSupportImpl extends ReificationSupportApi {
    def selectType(owner: Symbol, name: String): TypeSymbol =
      select(owner, newTypeName(name)).asType

    def selectTerm(owner: Symbol, name: String): TermSymbol = {
      val result = select(owner, newTermName(name)).asTerm
      if (result.isOverloaded) result.suchThat(!_.isMethod).asTerm
      else result
    }

    protected def select(owner: Symbol, name: Name): Symbol = {
      val result = owner.info decl name
      if (result ne NoSymbol) result
      else
        mirrorThatLoaded(owner).missingHook(owner, name) orElse {
          throw new ScalaReflectionException("%s %s in %s not found".format(if (name.isTermName) "term" else "type", name, owner.fullName))
        }
    }

    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol = {
      val result = owner.info.decl(newTermName(name)).alternatives(index)
      if (result ne NoSymbol) result.asMethod
      else throw new ScalaReflectionException("overloaded method %s #%d in %s not found".format(name, index, owner.fullName))
    }

    def newFreeTerm(name: String, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
      newFreeTermSymbol(newTermName(name), value, flags, origin).markFlagsCompleted(mask = AllFlags)

    def newFreeType(name: String, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      newFreeTypeSymbol(newTypeName(name), flags, origin).markFlagsCompleted(mask = AllFlags)

    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: Long, isClass: Boolean): Symbol =
      owner.newNestedSymbol(name, pos, flags, isClass).markFlagsCompleted(mask = AllFlags)

    def newScopeWith(elems: Symbol*): Scope =
      self.newScopeWith(elems: _*)

    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S =
      sym.setAnnotations(annots)

    def setInfo[S <: Symbol](sym: S, tpe: Type): S =
      sym.setInfo(tpe).markAllCompleted()

    def mkThis(sym: Symbol): Tree = self.This(sym)

    def mkSelect(qualifier: Tree, sym: Symbol): Select = self.Select(qualifier, sym)

    def mkIdent(sym: Symbol): Ident = self.Ident(sym)

    def mkTypeTree(tp: Type): TypeTree = self.TypeTree(tp)

    def ThisType(sym: Symbol): Type = self.ThisType(sym)

    def SingleType(pre: Type, sym: Symbol): Type = self.SingleType(pre, sym)

    def SuperType(thistpe: Type, supertpe: Type): Type = self.SuperType(thistpe, supertpe)

    def ConstantType(value: Constant): ConstantType = self.ConstantType(value)

    def TypeRef(pre: Type, sym: Symbol, args: List[Type]): Type = self.TypeRef(pre, sym, args)

    def RefinedType(parents: List[Type], decls: Scope, typeSymbol: Symbol): RefinedType = self.RefinedType(parents, decls, typeSymbol)

    def ClassInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType = self.ClassInfoType(parents, decls, typeSymbol)

    def MethodType(params: List[Symbol], resultType: Type): MethodType = self.MethodType(params, resultType)

    def NullaryMethodType(resultType: Type): NullaryMethodType = self.NullaryMethodType(resultType)

    def PolyType(typeParams: List[Symbol], resultType: Type): PolyType = self.PolyType(typeParams, resultType)

    def ExistentialType(quantified: List[Symbol], underlying: Type): ExistentialType = self.ExistentialType(quantified, underlying)

    def AnnotatedType(annotations: List[Annotation], underlying: Type): AnnotatedType = self.AnnotatedType(annotations, underlying)

    def TypeBounds(lo: Type, hi: Type): TypeBounds = self.TypeBounds(lo, hi)

    def BoundedWildcardType(bounds: TypeBounds): BoundedWildcardType = self.BoundedWildcardType(bounds)

    def thisPrefix(sym: Symbol): Type = sym.thisPrefix

    def setType[T <: Tree](tree: T, tpe: Type): T = { tree.setType(tpe); tree }

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T = { tree.setSymbol(sym); tree }

    def toStats(tree: Tree): List[Tree] = tree match {
      case EmptyTree             => Nil
      case SyntacticBlock(stats) => stats
      case defn if defn.isDef    => defn :: Nil
      case imp: Import           => imp :: Nil
      case _                     => throw new IllegalArgumentException(s"can't flatten $tree")
    }

    def mkAnnotation(tree: Tree): Tree = tree match {
      case SyntacticNew(Nil, SyntacticApplied(SyntacticAppliedType(_, _), _) :: Nil, noSelfType, Nil) =>
        tree
      case _ =>
        throw new IllegalArgumentException(s"Tree ${showRaw(tree)} isn't a correct representation of annotation." +
                                            """Consider reformatting it into a q"new $name[..$targs](...$argss)" shape""")
    }

    def mkAnnotation(trees: List[Tree]): List[Tree] = trees.map(mkAnnotation)

    def mkParam(argss: List[List[Tree]], extraFlags: FlagSet = NoFlags, excludeFlags: FlagSet = DEFERRED): List[List[ValDef]] =
      argss.map { args => args.map { mkParam(_, extraFlags, excludeFlags) } }

    def mkParam(tree: Tree, extraFlags: FlagSet, excludeFlags: FlagSet): ValDef = tree match {
      case Typed(Ident(name: TermName), tpt) =>
        mkParam(ValDef(NoMods, name, tpt, EmptyTree), extraFlags, excludeFlags)
      case vd: ValDef =>
        var newmods = vd.mods & (~excludeFlags)
        if (vd.rhs.nonEmpty) newmods |= DEFAULTPARAM
        copyValDef(vd)(mods = newmods | extraFlags)
      case _ =>
        throw new IllegalArgumentException(s"$tree is not valid representation of a parameter, " +
                                            """consider reformatting it into q"val $name: $T = $default" shape""")
    }

    def mkImplicitParam(args: List[Tree]): List[ValDef] = args.map(mkImplicitParam)

    def mkImplicitParam(tree: Tree): ValDef = mkParam(tree, IMPLICIT | PARAM, NoFlags)

    def mkTparams(tparams: List[Tree]): List[TypeDef] =
      tparams.map {
        case td: TypeDef => copyTypeDef(td)(mods = (td.mods | PARAM) & (~DEFERRED))
        case other => throw new IllegalArgumentException(s"can't splice $other as type parameter")
      }

    def mkRefineStat(stat: Tree): Tree = {
      stat match {
        case dd: DefDef => require(dd.rhs.isEmpty, "can't use DefDef with non-empty body as refine stat")
        case vd: ValDef => require(vd.rhs.isEmpty, "can't use ValDef with non-empty rhs as refine stat")
        case td: TypeDef =>
        case _ => throw new IllegalArgumentException(s"not legal refine stat: $stat")
      }
      stat
    }

    def mkRefineStat(stats: List[Tree]): List[Tree] = stats.map(mkRefineStat)

    def mkPackageStat(stat: Tree): Tree = {
      stat match {
        case cd: ClassDef =>
        case md: ModuleDef =>
        case pd: PackageDef =>
        case _ => throw new IllegalArgumentException(s"not legal package stat: $stat")
      }
      stat
    }

    def mkPackageStat(stats: List[Tree]): List[Tree] = stats.map(mkPackageStat)

    object ScalaDot extends ScalaDotExtractor {
      def apply(name: Name): Tree = gen.scalaDot(name)
      def unapply(tree: Tree): Option[Name] = tree match {
        case Select(id @ Ident(nme.scala_), name) if id.symbol == ScalaPackage => Some(name)
        case _ => None
      }
    }

    def mkEarlyDef(defn: Tree): Tree = defn match {
      case vdef @ ValDef(mods, _, _, _) if !mods.isDeferred =>
        copyValDef(vdef)(mods = mods | PRESUPER)
      case tdef @ TypeDef(mods, _, _, _) =>
        copyTypeDef(tdef)(mods = mods | PRESUPER)
      case _ =>
        throw new IllegalArgumentException(s"not legal early def: $defn")
    }

    def mkEarlyDef(defns: List[Tree]): List[Tree] = defns.map(mkEarlyDef)

    def mkRefTree(qual: Tree, sym: Symbol) = self.RefTree(qual, sym.name) setSymbol sym

    def freshTermName(prefix: String = nme.FRESH_TERM_NAME_PREFIX): TermName = self.freshTermName(prefix)

    def freshTypeName(prefix: String): TypeName = self.freshTypeName(prefix)

    protected implicit def fresh: FreshNameCreator = self.currentFreshNameCreator

    object ImplicitParams extends ImplicitParamsExtractor {
      def apply(paramss: List[List[Tree]], implparams: List[Tree]): List[List[Tree]] =
        if (implparams.nonEmpty) paramss :+ mkImplicitParam(implparams) else paramss

      def unapply(vparamss: List[List[ValDef]]): Some[(List[List[ValDef]], List[ValDef])] = vparamss match {
        case init :+ (last @ (initlast :: _)) if initlast.mods.isImplicit => Some((init, last))
        case _ => Some((vparamss, Nil))
      }
    }

    object FlagsRepr extends FlagsReprExtractor {
      def apply(bits: Long): FlagSet = bits
      def unapply(flags: Long): Some[Long] = Some(flags)
    }

    /** Construct/deconstruct type application term trees.
     *  Treats other term trees as zero-argument type applications.
     */
    object SyntacticTypeApplied extends SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree =
        if (targs.isEmpty) tree
        else if (tree.isTerm) TypeApply(tree, targs)
        else throw new IllegalArgumentException(s"can't apply type arguments to $tree")

      def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
        case TypeApply(fun, targs) => Some((fun, targs))
        case _ if tree.isTerm      => Some((tree, Nil))
        case _                     => None
      }
    }

    /** Construct/deconstruct applied type trees.
     *  Treats other types as zero-arity applied types.
     */
    object SyntacticAppliedType extends SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree =
        if (targs.isEmpty) tree
        else if (tree.isType) AppliedTypeTree(tree, targs)
        else throw new IllegalArgumentException(s"can't create applied type from non-type $tree")

      def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
        case MaybeTypeTreeOriginal(AppliedTypeTree(tpe, targs)) => Some((tpe, targs))
        case _ if tree.isType => Some((tree, Nil))
        case _ => None
      }
    }

    object SyntacticApplied extends SyntacticAppliedExtractor {
      def apply(tree: Tree, argss: List[List[Tree]]): Tree =
        argss.foldLeft(tree) { (f, args) => Apply(f, args.map(treeInfo.assignmentToMaybeNamedArg)) }

      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = tree match {
        case UnApply(treeInfo.Unapplied(Select(fun, nme.unapply)), pats) =>
          Some((fun, pats :: Nil))
        case treeInfo.Applied(fun, targs, argss) =>
          fun match {
            case Select(_: New, nme.CONSTRUCTOR) =>
              Some((tree, Nil))
            case _ =>
              val callee =
                if (fun.isTerm) SyntacticTypeApplied(fun, targs)
                else SyntacticAppliedType(fun, targs)
              Some((callee, argss))
          }
      }
    }

    // recover constructor contents generated by gen.mkTemplate
    protected object UnCtor {
      def unapply(tree: Tree): Option[(Modifiers, List[List[ValDef]], List[Tree])] = tree match {
        case DefDef(mods, nme.MIXIN_CONSTRUCTOR, _, _, _, SyntacticBlock(lvdefs :+ _)) =>
          Some((mods | Flag.TRAIT, Nil, lvdefs))
        case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, SyntacticBlock(lvdefs :+ _ :+ _)) =>
          Some((mods, vparamss, lvdefs))
        case _ => None
      }
    }

    // undo gen.mkTemplate
    protected object UnMkTemplate {
      def unapply(templ: Template): Option[(List[Tree], ValDef, Modifiers, List[List[ValDef]], List[Tree], List[Tree])] = {
        val Template(parents, selfType, _) = templ
        val tbody = treeInfo.untypecheckedTemplBody(templ)

        def result(ctorMods: Modifiers, vparamss: List[List[ValDef]], edefs: List[Tree], body: List[Tree]) =
          Some((parents, selfType, ctorMods, vparamss, edefs, body))
        def indexOfCtor(trees: List[Tree]) =
          trees.indexWhere { case UnCtor(_, _, _) => true ; case _ => false }

        if (tbody forall treeInfo.isInterfaceMember)
          result(NoMods | Flag.TRAIT, Nil, Nil, tbody)
        else if (indexOfCtor(tbody) == -1)
          None
        else {
          val (rawEdefs, rest) = tbody.span(treeInfo.isEarlyDef)
          val (gvdefs, etdefs) = rawEdefs.partition(treeInfo.isEarlyValDef)
          val (fieldDefs, UnCtor(ctorMods, ctorVparamss, lvdefs) :: body) = rest.splitAt(indexOfCtor(rest))
          val evdefs = gvdefs.zip(lvdefs).map {
            case (gvdef @ ValDef(_, _, tpt: TypeTree, _), ValDef(_, _, _, rhs)) =>
              copyValDef(gvdef)(tpt = tpt.original, rhs = rhs)
          }
          val edefs = evdefs ::: etdefs
          if (ctorMods.isTrait)
            result(ctorMods, Nil, edefs, body)
          else {
            // undo conversion from (implicit ... ) to ()(implicit ... ) when it's the only parameter section
            val vparamssRestoredImplicits = ctorVparamss match {
              case Nil :: (tail @ ((head :: _) :: _)) if head.mods.isImplicit => tail
              case other => other
            }
            // undo flag modifications by merging flag info from constructor args and fieldDefs
            val modsMap = fieldDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
            def ctorArgsCorrespondToFields = vparamssRestoredImplicits.flatten.forall { vd => modsMap.contains(vd.name) }
            if (!ctorArgsCorrespondToFields) None
            else {
              val vparamss = mmap(vparamssRestoredImplicits) { vd =>
                val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
                atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
              }
              result(ctorMods, vparamss, edefs, body)
            }
          }
        }
      }
    }

    protected def mkSelfType(tree: Tree) = tree match {
      case vd: ValDef =>
        require(vd.rhs.isEmpty, "self types must have empty right hand side")
        copyValDef(vd)(mods = (vd.mods | PRIVATE) & (~DEFERRED))
      case _ =>
        throw new IllegalArgumentException(s"$tree is not a valid representation of self type, " +
                                           """consider reformatting into q"val $self: $T" shape""")
    }

    object SyntacticClassDef extends SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                constrMods: Modifiers, vparamss: List[List[Tree]],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef = {
        val extraFlags = PARAMACCESSOR | (if (mods.isCase) CASEACCESSOR else 0L)
        val vparamss0 = mkParam(vparamss, extraFlags, excludeFlags = DEFERRED | PARAM)
        val tparams0 = mkTparams(tparams)
        val parents0 = gen.mkParents(mods,
          if (mods.isCase) parents.filter {
            case ScalaDot(tpnme.Product | tpnme.Serializable | tpnme.AnyRef) => false
            case _ => true
          } else parents
        )
        val body0 = earlyDefs ::: body
        val selfType0 = mkSelfType(selfType)
        val templ = gen.mkTemplate(parents0, selfType0, constrMods, vparamss0, body0)
        gen.mkClassDef(mods, name, tparams0, templ)
      }

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                       List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfType, ctorMods, vparamss, earlyDefs, body))
          if !ctorMods.isTrait && !ctorMods.hasFlag(JAVA) =>
          Some((mods, name, tparams, ctorMods, vparamss, earlyDefs, parents, selfType, body))
        case _ =>
          None
      }
    }

    object SyntacticTraitDef extends SyntacticTraitDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree], earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef = {
        val mods0 = mods | TRAIT | ABSTRACT
        val templ = gen.mkTemplate(parents, mkSelfType(selfType), Modifiers(TRAIT), Nil, earlyDefs ::: body)
        gen.mkClassDef(mods0, name, mkTparams(tparams), templ)
      }

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                       List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfType, ctorMods, vparamss, earlyDefs, body))
          if mods.isTrait =>
          Some((mods, name, tparams, earlyDefs, parents, selfType, body))
        case _ => None
      }
    }

    object SyntacticObjectDef extends SyntacticObjectDefExtractor {
      def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): ModuleDef =
        ModuleDef(mods, name, gen.mkTemplate(parents, mkSelfType(selfType), NoMods, Nil, earlyDefs ::: body))

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case ModuleDef(mods, name, UnMkTemplate(parents, selfType, _, _, earlyDefs, body)) =>
          Some((mods, name, earlyDefs, parents, selfType, body))
        case _ =>
          None
      }
    }

    object SyntacticPackageObjectDef extends SyntacticPackageObjectDefExtractor {
      def apply(name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): PackageDef =
        gen.mkPackageObject(SyntacticObjectDef(NoMods, name, earlyDefs, parents, selfType, body))

      def unapply(tree: Tree): Option[(TermName, List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case PackageDef(Ident(name: TermName), List(SyntacticObjectDef(NoMods, nme.PACKAGEkw, earlyDefs, parents, selfType, body))) =>
          Some((name, earlyDefs, parents, selfType, body))
        case _ =>
          None
      }
    }

    // match references to `scala.$name`
    protected class ScalaMemberRef(symbols: Seq[Symbol]) {
      def result(name: Name): Option[Symbol] =
        symbols.collect { case sym if sym.name == name => sym }.headOption
      def unapply(tree: Tree): Option[Symbol] = tree match {
        case id @ Ident(name) if symbols.contains(id.symbol) && name == id.symbol.name =>
          Some(id.symbol)
        case Select(scalapkg @ Ident(nme.scala_), name) if scalapkg.symbol == ScalaPackage =>
          result(name)
        case Select(Select(Ident(nme.ROOTPKG), nme.scala_), name) =>
          result(name)
        case _ => None
      }
    }
    protected object TupleClassRef extends ScalaMemberRef(TupleClass.seq)
    protected object TupleCompanionRef extends ScalaMemberRef(TupleClass.seq.map { _.companionModule })
    protected object UnitClassRef extends ScalaMemberRef(Seq(UnitClass))
    protected object FunctionClassRef extends ScalaMemberRef(FunctionClass.seq)

    object SyntacticTuple extends SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree = {
        require(args.isEmpty || TupleClass(args.length).exists, s"Tuples with ${args.length} arity aren't supported")
        gen.mkTuple(args)
      }

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case Literal(Constant(())) =>
          Some(Nil)
        case Apply(MaybeTypeTreeOriginal(SyntacticTypeApplied(MaybeSelectApply(TupleCompanionRef(sym)), targs)), args)
          if sym == TupleClass(args.length).companionModule
          && (targs.isEmpty || targs.length == args.length) =>
          Some(args)
        case _ if tree.isTerm =>
          Some(tree :: Nil)
        case _ =>
          None
      }
    }

    object SyntacticTupleType extends SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree = {
        require(args.isEmpty || TupleClass(args.length).exists, s"Tuples with ${args.length} arity aren't supported")
        gen.mkTupleType(args)
      }

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case MaybeTypeTreeOriginal(UnitClassRef(_)) =>
          Some(Nil)
        case MaybeTypeTreeOriginal(AppliedTypeTree(TupleClassRef(sym), args))
          if sym == TupleClass(args.length) =>
          Some(args)
        case _ if tree.isType =>
          Some(tree :: Nil)
        case _ =>
          None
      }
    }

    object SyntacticFunctionType extends SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[Tree], restpe: Tree): Tree = {
        require(FunctionClass(argtpes.length).exists, s"Function types with ${argtpes.length} arity aren't supported")
        gen.mkFunctionTypeTree(argtpes, restpe)
      }

      def unapply(tree: Tree): Option[(List[Tree], Tree)] = tree match {
        case MaybeTypeTreeOriginal(AppliedTypeTree(FunctionClassRef(sym), args @ (argtpes :+ restpe)))
          if sym == FunctionClass(args.length - 1) =>
          Some((argtpes, restpe))
        case _ => None
      }
    }

    object SyntheticUnit {
      def unapply(tree: Tree): Boolean = tree match {
        case Literal(Constant(())) if tree.hasAttachment[SyntheticUnitAttachment.type] => true
        case _ => false
      }
    }

    /** Syntactic combinator that abstracts over Block tree.
     *
     *  Apart from providing a more straightforward api that exposes
     *  block as a list of elements rather than (stats, expr) pair
     *  it also:
     *
     *  1. Strips trailing synthetic units which are inserted by the
     *     compiler if the block ends with a definition rather
     *     than an expression or is empty.
     *
     *  2. Matches non-block term trees and recognizes them as
     *     single-element blocks for sake of consistency with
     *     compiler's default to treat single-element blocks with
     *     expressions as just expressions. The only exception is q""
     *     which is not considered to be a block.
     */
    object SyntacticBlock extends SyntacticBlockExtractor {
      def apply(stats: List[Tree]): Tree = gen.mkBlock(stats)

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case bl @ self.Block(stats, SyntheticUnit()) => Some(treeInfo.untypecheckedBlockBody(bl))
        case bl @ self.Block(stats, expr)            => Some(treeInfo.untypecheckedBlockBody(bl) :+ expr)
        case SyntheticUnit()                         => Some(Nil)
        case _ if tree.isTerm && tree.nonEmpty       => Some(tree :: Nil)
        case _                                       => None
      }
    }

    object SyntacticFunction extends SyntacticFunctionExtractor {
      def apply(params: List[Tree], body: Tree): Function = {
        val params0 :: Nil = mkParam(params :: Nil, PARAM)
        require(params0.forall { _.rhs.isEmpty }, "anonymous functions don't support parameters with default values")
        Function(params0, body)
      }

      def unapply(tree: Function): Option[(List[ValDef], Tree)] = Function.unapply(tree)
    }

    object SyntacticNew extends SyntacticNewExtractor {
      def apply(earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): Tree =
        gen.mkNew(parents, mkSelfType(selfType), earlyDefs ::: body, NoPosition, NoPosition)

      def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case treeInfo.Applied(Select(New(SyntacticAppliedType(ident, targs)), nme.CONSTRUCTOR), Nil, List(Nil)) =>
          Some((Nil, SyntacticAppliedType(ident, targs) :: Nil, noSelfType, Nil))
        case treeInfo.Applied(Select(New(SyntacticAppliedType(ident, targs)), nme.CONSTRUCTOR), Nil, argss) =>
          Some((Nil, SyntacticApplied(SyntacticAppliedType(ident, targs), argss) :: Nil, noSelfType, Nil))
        case SyntacticBlock(SyntacticClassDef(_, tpnme.ANON_CLASS_NAME, Nil, _, ListOfNil, earlyDefs, parents, selfType, body) ::
                            Apply(Select(New(Ident(tpnme.ANON_CLASS_NAME)), nme.CONSTRUCTOR), Nil) :: Nil) =>
          Some((earlyDefs, parents, selfType, body))
        case _ =>
          None
      }
    }

    object SyntacticDefDef extends SyntacticDefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[Tree],
                vparamss: List[List[Tree]], tpt: Tree, rhs: Tree): DefDef = {
        val tparams0 = mkTparams(tparams)
        val vparamss0 = mkParam(vparamss, PARAM)
        val rhs0 = {
          if (name != nme.CONSTRUCTOR) rhs
          else rhs match {
            case Block(_, _) => rhs
            case _ => Block(List(rhs), gen.mkSyntheticUnit)
          }
        }
        DefDef(mods, name, tparams0, vparamss0, tpt, rhs0)
      }

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)] = tree match {
        case DefDef(mods, nme.CONSTRUCTOR, tparams, vparamss, tpt, Block(List(expr), Literal(Constant(())))) =>
          Some((mods, nme.CONSTRUCTOR, tparams, vparamss, tpt, expr))
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          Some((mods, name, tparams, vparamss, tpt, rhs))
        case _ => None
      }
    }

    protected class SyntacticValDefBase(isMutable: Boolean) extends SyntacticValDefExtractor {
      def modifiers(mods: Modifiers): Modifiers = if (isMutable) mods | MUTABLE else mods

      def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef = ValDef(modifiers(mods), name, tpt, rhs)

      def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = tree match {
        case ValDef(mods, name, tpt, rhs) if mods.hasFlag(MUTABLE) == isMutable =>
          Some((mods, name, tpt, rhs))
        case _ =>
          None
      }
    }
    object SyntacticValDef extends SyntacticValDefBase(isMutable = false)
    object SyntacticVarDef extends SyntacticValDefBase(isMutable = true)

    object SyntacticAssign extends SyntacticAssignExtractor {
      def apply(lhs: Tree, rhs: Tree): Tree = gen.mkAssign(lhs, rhs)
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Assign(lhs, rhs) => Some((lhs, rhs))
        case AssignOrNamedArg(lhs, rhs) => Some((lhs, rhs))
        case Apply(Select(fn, nme.update), args :+ rhs) => Some((atPos(fn.pos)(Apply(fn, args)), rhs))
        case _ => None
      }
    }

    def UnliftListElementwise[T](unliftable: Unliftable[T]) = new UnliftListElementwise[T] {
      def unapply(lst: List[Tree]): Option[List[T]] = {
        val unlifted = lst.flatMap { unliftable.unapply(_) }
        if (unlifted.length == lst.length) Some(unlifted) else None
      }
    }

    def UnliftListOfListsElementwise[T](unliftable: Unliftable[T]) = new UnliftListOfListsElementwise[T] {
      def unapply(lst: List[List[Tree]]): Option[List[List[T]]] = {
        val unlifted = lst.map { l => l.flatMap { unliftable.unapply(_) } }
        if (unlifted.flatten.length == lst.flatten.length) Some(unlifted) else None
      }
    }

    object SyntacticValFrom extends SyntacticValFromExtractor {
      def apply(pat: Tree, rhs: Tree): Tree = gen.ValFrom(pat, gen.mkCheckIfRefutable(pat, rhs))
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case gen.ValFrom(pat, UnCheckIfRefutable(pat1, rhs1)) if pat.equalsStructure(pat1) =>
          Some((pat, rhs1))
        case gen.ValFrom(pat, rhs) =>
          Some((pat, rhs))
        case _ => None
      }
    }

    object SyntacticValEq extends SyntacticValEqExtractor {
      def apply(pat: Tree, rhs: Tree): Tree         = gen.ValEq(pat, rhs)
      def unapply(tree: Tree): Option[(Tree, Tree)] = gen.ValEq.unapply(tree)
    }

    object SyntacticFilter extends SyntacticFilterExtractor {
      def apply(tree: Tree): Tree           = gen.Filter(tree)
      def unapply(tree: Tree): Option[Tree] = gen.Filter.unapply(tree)
    }

    // If a tree in type position isn't provided by the user (e.g. `tpt` fields of
    // `ValDef` and `DefDef`, function params etc), then it's going to be parsed as
    // TypeTree with empty original and empty tpe. This extractor matches such trees
    // so that one can write q"val x = 2" to match typecheck(q"val x = 2"). Note that
    // TypeTree() is the only possible representation for empty trees in type positions.
    // We used to sometimes receive EmptyTree in such cases, but not anymore.
    object SyntacticEmptyTypeTree extends SyntacticEmptyTypeTreeExtractor {
      def apply(): TypeTree = self.TypeTree()
      def unapply(tt: TypeTree): Boolean = tt.original == null || tt.original.isEmpty
    }

    // match a sequence of desugared `val $pat = $value`
    protected object UnPatSeq {
      def unapply(trees: List[Tree]): Option[List[(Tree, Tree)]] = {
        val imploded = implodePatDefs(trees)
        val patvalues = imploded.flatMap {
          case SyntacticPatDef(_, pat, EmptyTree, rhs) => Some((pat, rhs))
          case ValDef(_, name, SyntacticEmptyTypeTree(), rhs) => Some((Bind(name, self.Ident(nme.WILDCARD)), rhs))
          case ValDef(_, name, tpt, rhs) => Some((Bind(name, Typed(self.Ident(nme.WILDCARD), tpt)), rhs))
          case _ => None
        }
        if (patvalues.length == imploded.length) Some(patvalues) else None
      }
    }

    // implode multiple-statement desugaring of pattern definitions
    // into single-statement valdefs with nme.QUASIQUOTE_PAT_DEF name
    object implodePatDefs extends Transformer {
      override def transform(tree: Tree) = tree match {
        case templ: Template => deriveTemplate(templ)(transformStats)
        case block: Block =>
          val Block(init, last) = block
          Block(transformStats(init), transform(last)).copyAttrs(block)
        case ValDef(mods, name1, SyntacticEmptyTypeTree(), Match(MaybeTyped(MaybeUnchecked(value), tpt), CaseDef(pat, EmptyTree, Ident(name2)) :: Nil))
          if name1 == name2 =>
          ValDef(mods, nme.QUASIQUOTE_PAT_DEF, Typed(pat, tpt), transform(value))
        case _ =>
          super.transform(tree)
      }
      def transformStats(trees: List[Tree]): List[Tree] = trees match {
        case Nil => Nil
        case ValDef(mods, _, SyntacticEmptyTypeTree(), Match(MaybeTyped(MaybeUnchecked(value), tpt), CaseDef(pat, EmptyTree, SyntacticTuple(ids)) :: Nil)) :: tail
          if mods.hasFlag(SYNTHETIC) && mods.hasFlag(ARTIFACT) =>
          ids match {
            case Nil =>
              ValDef(NoMods, nme.QUASIQUOTE_PAT_DEF, Typed(pat, tpt), transform(value)) :: transformStats(tail)
            case _   =>
              val mods = tail.take(1).head.asInstanceOf[ValDef].mods
              ValDef(mods, nme.QUASIQUOTE_PAT_DEF, Typed(pat, tpt), transform(value)) :: transformStats(tail.drop(ids.length))
          }
        case other :: tail =>
          transform(other) :: transformStats(tail)
      }
      def apply(tree: Tree) = transform(tree)
      def apply(trees: List[Tree]) = transformStats(trees)
    }

    object SyntacticPatDef extends SyntacticPatDefExtractor {
      def apply(mods: Modifiers, pat: Tree, tpt: Tree, rhs: Tree): List[ValDef] = tpt match {
        case SyntacticEmptyTypeTree() => gen.mkPatDef(mods, pat, rhs)
        case _                        => gen.mkPatDef(mods, Typed(pat, tpt), rhs)
      }
      def unapply(tree: Tree): Option[(Modifiers, Tree, Tree, Tree)] = tree match {
        case ValDef(mods, nme.QUASIQUOTE_PAT_DEF, Typed(pat,  tpt), rhs) => Some((mods, pat, tpt, rhs))
        case _ => None
      }
    }

    // match a sequence of desugared `val $pat = $value` with a tuple in the end
    protected object UnPatSeqWithRes {
      def unapply(tree: Tree): Option[(List[(Tree, Tree)], List[Tree])] = tree match {
        case SyntacticBlock(UnPatSeq(trees) :+ SyntacticTuple(elems)) => Some((trees, elems))
        case _ => None
      }
    }

    // undo gen.mkSyntheticParam
    protected object UnSyntheticParam {
      def unapply(tree: Tree): Option[TermName] = tree match {
        case ValDef(mods, name, _, EmptyTree)
          if mods.hasFlag(SYNTHETIC) && mods.hasFlag(PARAM) =>
          Some(name)
        case _ => None
      }
    }

    // undo gen.mkVisitor
    protected object UnVisitor {
      def unapply(tree: Tree): Option[(TermName, List[CaseDef])] = tree match {
        case Function(UnSyntheticParam(x1) :: Nil, Match(MaybeUnchecked(Ident(x2)), cases))
          if x1 == x2 =>
          Some((x1, cases))
        case _ => None
      }
    }

    // undo gen.mkFor:makeClosure
    protected object UnClosure {
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Function(ValDef(Modifiers(PARAM, _, _), name, tpt, EmptyTree) :: Nil, body) =>
          tpt match {
            case SyntacticEmptyTypeTree() => Some((Bind(name, self.Ident(nme.WILDCARD)), body))
            case _                        => Some((Bind(name, Typed(self.Ident(nme.WILDCARD), tpt)), body))
          }
        case UnVisitor(_, CaseDef(pat, EmptyTree, body) :: Nil) =>
          Some((pat, body))
        case _ => None
      }
    }

    // match call to either withFilter or filter
    protected object FilterCall {
      def unapply(tree: Tree): Option[(Tree,Tree)] = tree match {
        case Apply(Select(obj, nme.withFilter | nme.filter), arg :: Nil) =>
          Some(obj, arg)
        case _ => None
      }
    }

    // transform a chain of withFilter calls into a sequence of for filters
    protected object UnFilter {
      def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
        case UnCheckIfRefutable(_, _) =>
          Some((tree, Nil))
        case FilterCall(UnFilter(rhs, rest), UnClosure(_, test)) =>
          Some((rhs, rest :+ SyntacticFilter(test)))
        case _ =>
          Some((tree, Nil))
      }
    }

    // undo gen.mkCheckIfRefutable
    protected object UnCheckIfRefutable {
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case FilterCall(rhs, UnVisitor(name,
            CaseDef(pat, EmptyTree, Literal(Constant(true))) ::
            CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false))) :: Nil))
          if name.toString.contains(nme.CHECK_IF_REFUTABLE_STRING) =>
          Some((pat, rhs))
        case _ => None
      }
    }

    // undo gen.mkFor:makeCombination accounting for possible extra implicit argument
    protected class UnForCombination(name: TermName) {
      def unapply(tree: Tree) = tree match {
        case SyntacticApplied(SyntacticTypeApplied(sel @ Select(lhs, meth), _), (f :: Nil) :: Nil)
          if name == meth && sel.hasAttachment[ForAttachment.type] =>
          Some(lhs, f)
        case SyntacticApplied(SyntacticTypeApplied(sel @ Select(lhs, meth), _), (f :: Nil) :: _ :: Nil)
          if name == meth && sel.hasAttachment[ForAttachment.type] =>
          Some(lhs, f)
        case _ => None
      }
    }
    protected object UnMap     extends UnForCombination(nme.map)
    protected object UnForeach extends UnForCombination(nme.foreach)
    protected object UnFlatMap extends UnForCombination(nme.flatMap)

    // undo desugaring done in gen.mkFor
    protected object UnFor {
      def unapply(tree: Tree): Option[(List[Tree], Tree)] = {
        val interm = tree match {
          case UnFlatMap(UnFilter(rhs, filters), UnClosure(pat, UnFor(rest, body))) =>
            Some(((pat, rhs), filters ::: rest, body))
          case UnForeach(UnFilter(rhs, filters), UnClosure(pat, UnFor(rest, body))) =>
            Some(((pat, rhs), filters ::: rest, body))
          case UnMap(UnFilter(rhs, filters), UnClosure(pat, cbody)) =>
            Some(((pat, rhs), filters, gen.Yield(cbody)))
          case UnForeach(UnFilter(rhs, filters), UnClosure(pat, cbody)) =>
            Some(((pat, rhs), filters, cbody))
          case _ => None
        }
        interm.flatMap {
          case ((Bind(_, SyntacticTuple(_)) | SyntacticTuple(_),
                 UnFor(SyntacticValFrom(pat, rhs) :: innerRest, gen.Yield(UnPatSeqWithRes(pats, elems2)))),
                outerRest, fbody) =>
            val valeqs = pats.map { case (pat, rhs) => SyntacticValEq(pat, rhs) }
            Some((SyntacticValFrom(pat, rhs) :: innerRest ::: valeqs ::: outerRest, fbody))
          case ((pat, rhs), filters, body) =>
            Some((SyntacticValFrom(pat, rhs) :: filters, body))
        }
      }
    }

    // check that enumerators are valid
    protected def mkEnumerators(enums: List[Tree]): List[Tree] = {
      require(enums.nonEmpty, "enumerators can't be empty")
      enums.head match {
        case SyntacticValFrom(_, _) =>
        case t => throw new IllegalArgumentException(s"$t is not a valid first enumerator of for loop")
      }
      enums.tail.foreach {
        case SyntacticValEq(_, _) | SyntacticValFrom(_, _) | SyntacticFilter(_) =>
        case t => throw new IllegalArgumentException(s"$t is not a valid representation of a for loop enumerator")
      }
      enums
    }

    object SyntacticFor extends SyntacticForExtractor {
      def apply(enums: List[Tree], body: Tree): Tree = gen.mkFor(mkEnumerators(enums), body)
      def unapply(tree: Tree) = tree match {
        case UnFor(enums, gen.Yield(body)) => None
        case UnFor(enums, body) => Some((enums, body))
        case _ => None
      }
    }

    object SyntacticForYield extends SyntacticForExtractor {
      def apply(enums: List[Tree], body: Tree): Tree = gen.mkFor(mkEnumerators(enums), gen.Yield(body))
      def unapply(tree: Tree) = tree match {
        case UnFor(enums, gen.Yield(body)) => Some((enums, body))
        case _ => None
      }
    }

    // use typetree's original instead of typetree itself
    protected object MaybeTypeTreeOriginal {
      def unapply(tree: Tree): Some[Tree] = tree match {
        case tt: TypeTree => Some(tt.original)
        case _            => Some(tree)
      }
    }

    // drop potential extra call to .apply
    protected object MaybeSelectApply {
      def unapply(tree: Tree): Some[Tree] = tree match {
        case Select(f, nme.apply) => Some(f)
        case other                => Some(other)
      }
    }

    // drop potential @scala.unchecked annotation
    protected object MaybeUnchecked {
      def unapply(tree: Tree): Some[Tree] = tree match {
        case Annotated(SyntacticNew(Nil, ScalaDot(tpnme.unchecked) :: Nil, noSelfType, Nil), annottee) =>
          Some(annottee)
        case Typed(annottee, MaybeTypeTreeOriginal(
          Annotated(SyntacticNew(Nil, ScalaDot(tpnme.unchecked) :: Nil, noSelfType, Nil), _))) =>
          Some(annottee)
        case annottee => Some(annottee)
      }
    }

    protected object MaybeTyped {
      def unapply(tree: Tree): Some[(Tree, Tree)] = tree match {
        case Typed(v, tpt) => Some((v, tpt))
        case v             => Some((v, SyntacticEmptyTypeTree()))
      }
    }

    protected def mkCases(cases: List[Tree]): List[CaseDef] = cases.map {
      case c: CaseDef => c
      case tree => throw new IllegalArgumentException(s"$tree is not valid representation of pattern match case")
    }

    object SyntacticPartialFunction extends SyntacticPartialFunctionExtractor {
      def apply(cases: List[Tree]): Match = Match(EmptyTree, mkCases(cases))
      def unapply(tree: Tree): Option[List[CaseDef]] = tree match {
        case Match(EmptyTree, cases) => Some(cases)
        case Typed(
               Block(
                 List(ClassDef(clsMods, tpnme.ANON_FUN_NAME, Nil, Template(
                   List(abspf: TypeTree, ser: TypeTree), noSelfType, List(
                     DefDef(_, nme.CONSTRUCTOR, _, _, _, _),
                     DefDef(_, nme.applyOrElse, _, _, _,
                       Match(_, cases :+
                         CaseDef(Bind(nme.DEFAULT_CASE, Ident(nme.WILDCARD)), _, _))),
                     DefDef(_, nme.isDefinedAt, _, _, _, _))))),
                 Apply(Select(New(Ident(tpnme.ANON_FUN_NAME)), termNames.CONSTRUCTOR), List())),
               pf: TypeTree)
          if pf.tpe != null && pf.tpe.typeSymbol.eq(PartialFunctionClass) &&
             abspf.tpe != null && abspf.tpe.typeSymbol.eq(AbstractPartialFunctionClass) &&
             ser.tpe != null && ser.tpe.typeSymbol.eq(SerializableClass) &&
             clsMods.hasFlag(FINAL) && clsMods.hasFlag(SYNTHETIC) =>
          Some(cases)
        case _ => None
      }
    }

    object SyntacticMatch extends SyntacticMatchExtractor {
      def apply(scrutinee: Tree, cases: List[Tree]) = {
        require(scrutinee.nonEmpty, "match's scrutinee may not be empty")
        Match(scrutinee, mkCases(cases))
      }

      def unapply(tree: Match): Option[(Tree, List[CaseDef])] = tree match {
        case Match(scrutinee, cases) if scrutinee.nonEmpty => Some((scrutinee, cases))
        case _                                             => None
      }
    }

    object SyntacticTry extends SyntacticTryExtractor {
      def apply(block: Tree, catches: List[Tree], finalizer: Tree) = Try(block, mkCases(catches), finalizer)
      def unapply(tree: Try): Option[(Tree, List[CaseDef], Tree)] = Try.unapply(tree)
    }

    object SyntacticTermIdent extends SyntacticTermIdentExtractor {
      def apply(name: TermName, isBackquoted: Boolean): Ident = {
        val id = self.Ident(name)
        if (isBackquoted) id updateAttachment BackquotedIdentifierAttachment
        id
      }
      def unapply(id: Ident): Option[(TermName, Boolean)] = id.name match {
        case name: TermName => Some((name, id.hasAttachment[BackquotedIdentifierAttachment.type]))
        case _              => None
      }
    }

    object SyntacticTypeIdent extends SyntacticTypeIdentExtractor {
      def apply(name: TypeName): Ident = self.Ident(name)
      def unapply(tree: Tree): Option[TypeName] = tree match {
        case MaybeTypeTreeOriginal(Ident(name: TypeName)) => Some(name)
        case _ => None
      }
    }

    /** Facade over Imports and ImportSelectors that lets to structurally
     *  deconstruct/reconstruct them.
     *
     *  Selectors are represented in the following way:
     *  1. q"import foo._"            <==> q"import foo.${pq"_"}"
     *  2. q"import foo.bar"          <==> q"import foo.${pq"bar"}"
     *  3. q"import foo.{bar => baz}" <==> q"import foo.${pq"bar -> baz"}"
     *  4. q"import foo.{bar => _}"   <==> q"import foo.${pq"bar -> _"}"
     *
     *  All names in selectors are TermNames despite the fact ImportSelector
     *  can theoretically contain TypeNames too (but they never do in practice.)
     */
    object SyntacticImport extends SyntacticImportExtractor {
      // construct/deconstruct {_} import selector
      private object WildcardSelector {
        def apply(offset: Int): ImportSelector = ImportSelector(nme.WILDCARD, offset, null, -1)
        def unapply(sel: ImportSelector): Option[Int] = sel match {
          case ImportSelector(nme.WILDCARD, offset, null, -1) => Some(offset)
          case _                                              => None
        }
      }

      // construct/deconstruct {foo} import selector
      private object NameSelector {
        def apply(name: TermName, offset: Int): ImportSelector = ImportSelector(name, offset, name, offset)
        def unapply(sel: ImportSelector): Option[(TermName, Int)] = sel match {
          case ImportSelector(name1, offset1, name2, offset2) if name1 == name2 && offset1 == offset2 =>
            Some((name1.toTermName, offset1))
          case _ =>
            None
        }
      }

      // construct/deconstruct {foo => bar} import selector
      private object RenameSelector {
        def apply(name1: TermName, offset1: Int, name2: TermName, offset2: Int): ImportSelector =
          ImportSelector(name1, offset1, name2, offset2)
        def unapply(sel: ImportSelector): Option[(TermName, Int, TermName, Int)] = sel match {
          case ImportSelector(_, _, null | nme.WILDCARD, _) =>
            None
          case ImportSelector(name1, offset1, name2, offset2) if name1 != name2 =>
            Some((name1.toTermName, offset1, name2.toTermName, offset2))
          case _ =>
            None
        }
      }

      // construct/deconstruct {foo => _} import selector
      private object UnimportSelector {
        def apply(name: TermName, offset: Int): ImportSelector =
          ImportSelector(name, offset, nme.WILDCARD, -1)
        def unapply(sel: ImportSelector): Option[(TermName, Int)] = sel match {
          case ImportSelector(name, offset, nme.WILDCARD, _) => Some((name.toTermName, offset))
          case _                                             => None
        }
      }

      // represent {_} import selector as pq"_"
      private object WildcardSelectorRepr {
        def apply(pos: Position): Tree = atPos(pos)(self.Ident(nme.WILDCARD))
        def unapply(tree: Tree): Option[Position] = tree match {
          case self.Ident(nme.WILDCARD) => Some(tree.pos)
          case _                        => None
        }
      }

      // represent {foo} import selector as pq"foo"
      private object NameSelectorRepr {
        def apply(name: TermName, pos: Position): Tree = atPos(pos)(Bind(name, WildcardSelectorRepr(pos)))
        def unapply(tree: Tree): Option[(TermName, Position)] = tree match {
          case Bind(name, WildcardSelectorRepr(_)) => Some((name.toTermName, tree.pos))
          case _                                   => None
        }
      }

      // pq"left -> right"
      private object Arrow {
        def apply(left: Tree, right: Tree): Apply =
          Apply(self.Ident(nme.MINGT), left :: right :: Nil)
        def unapply(tree: Apply): Option[(Tree, Tree)] = tree match {
          case Apply(self.Ident(nme.MINGT), left :: right :: Nil) => Some((left, right))
          case _ => None
        }
      }

      // represent {foo => bar} import selector as pq"foo -> bar"
      private object RenameSelectorRepr {
        def apply(name1: TermName, pos1: Position, name2: TermName, pos2: Position): Tree = {
          val left = NameSelectorRepr(name1, pos1)
          val right = NameSelectorRepr(name2, pos2)
          atPos(wrappingPos(left :: right :: Nil))(Arrow(left, right))
        }
        def unapply(tree: Tree): Option[(TermName, Position, TermName, Position)] = tree match {
          case Arrow(NameSelectorRepr(name1, pos1), NameSelectorRepr(name2, pos2)) =>
            Some((name1.toTermName, pos1, name2.toTermName, pos2))
          case _ =>
            None
        }
      }

      // represent {foo => _} import selector as pq"foo -> _"
      private object UnimportSelectorRepr {
        def apply(name: TermName, pos: Position): Tree =
          atPos(pos)(Arrow(NameSelectorRepr(name, pos), WildcardSelectorRepr(pos)))
        def unapply(tree: Tree): Option[(TermName, Position)] = tree match {
          case Arrow(NameSelectorRepr(name, pos), WildcardSelectorRepr(_)) =>
            Some((name, pos))
          case _ =>
            None
        }
      }

      private def derivedPos(t: Tree, offset: Int): Position =
        if (t.pos == NoPosition) NoPosition else t.pos.withPoint(offset)

      private def derivedOffset(pos: Position): Int =
        if (pos == NoPosition) -1 else pos.point

      def apply(expr: Tree, selectors: List[Tree]): Import = {
        val importSelectors = selectors.map {
          case WildcardSelectorRepr(pos)                    => WildcardSelector(derivedOffset(pos))
          case NameSelectorRepr(name, pos)                  => NameSelector(name, derivedOffset(pos))
          case RenameSelectorRepr(name1, pos1, name2, pos2) => RenameSelector(name1, derivedOffset(pos1), name2, derivedOffset(pos2))
          case UnimportSelectorRepr(name, pos)              => UnimportSelector(name, derivedOffset(pos))
          case tree                                         => throw new IllegalArgumentException(s"${showRaw(tree)} doesn't correspond to import selector")
        }
        Import(expr, importSelectors)
      }

      def unapply(imp: Import): Some[(Tree, List[Tree])] = {
        val selectors = imp.selectors.map {
          case WildcardSelector(offset)                       => WildcardSelectorRepr(derivedPos(imp, offset))
          case NameSelector(name, offset)                     => NameSelectorRepr(name, derivedPos(imp, offset))
          case RenameSelector(name1, offset1, name2, offset2) => RenameSelectorRepr(name1, derivedPos(imp, offset1), name2, derivedPos(imp, offset2))
          case UnimportSelector(name, offset)                 => UnimportSelectorRepr(name, derivedPos(imp, offset))
        }
        Some((imp.expr, selectors))
      }
    }

    object SyntacticSelectType extends SyntacticSelectTypeExtractor {
      def apply(qual: Tree, name: TypeName): Select = Select(qual, name)
      def unapply(tree: Tree): Option[(Tree, TypeName)] = tree match {
        case MaybeTypeTreeOriginal(Select(qual, name: TypeName)) => Some((qual, name))
        case _ => None
      }
    }

    object SyntacticSelectTerm extends SyntacticSelectTermExtractor {
      def apply(qual: Tree, name: TermName): Select = Select(qual, name)
      def unapply(tree: Tree): Option[(Tree, TermName)] = tree match {
        case Select(qual, name: TermName) => Some((qual, name))
        case _                            => None
      }
    }

    object SyntacticCompoundType extends SyntacticCompoundTypeExtractor {
      def apply(parents: List[Tree], defns: List[Tree]) =
        CompoundTypeTree(Template(gen.mkParents(NoMods, parents), noSelfType, defns))
      def unapply(tree: Tree): Option[(List[Tree], List[Tree])] = tree match {
        case MaybeTypeTreeOriginal(CompoundTypeTree(Template(parents, _, defns))) =>
          Some((parents, defns))
        case _ =>
          None
      }
    }

    object SyntacticSingletonType extends SyntacitcSingletonTypeExtractor {
      def apply(ref: Tree): SingletonTypeTree = SingletonTypeTree(ref)
      def unapply(tree: Tree): Option[Tree] = tree match {
        case MaybeTypeTreeOriginal(SingletonTypeTree(ref)) =>
          Some(ref)
        case _ =>
          None
      }
    }

    object SyntacticTypeProjection extends SyntacticTypeProjectionExtractor {
      def apply(qual: Tree, name: TypeName): SelectFromTypeTree =
        SelectFromTypeTree(qual, name)
      def unapply(tree: Tree): Option[(Tree, TypeName)] = tree match {
        case MaybeTypeTreeOriginal(SelectFromTypeTree(qual, name)) =>
          Some((qual, name))
        case _ =>
          None
      }
    }

    object SyntacticAnnotatedType extends SyntacticAnnotatedTypeExtractor {
      def apply(tpt: Tree, annot: Tree): Annotated =
        Annotated(annot, tpt)
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case MaybeTypeTreeOriginal(Annotated(annot, tpt)) =>
          Some((tpt, annot))
        case _ =>
          None
      }
    }

    object SyntacticExistentialType extends SyntacticExistentialTypeExtractor {
      def apply(tpt: Tree, where: List[Tree]): ExistentialTypeTree =
        ExistentialTypeTree(tpt, where.map {
          case md: MemberDef => md
          case tree => throw new IllegalArgumentException("$tree is not legal forSome definition")
        })
      def unapply(tree: Tree): Option[(Tree, List[MemberDef])] = tree match {
        case MaybeTypeTreeOriginal(ExistentialTypeTree(tpt, where)) =>
          Some((tpt, where))
        case _ =>
          None
      }
    }
  }

  val build = new ReificationSupportImpl
}
