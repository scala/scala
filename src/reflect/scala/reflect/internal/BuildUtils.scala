package scala
package reflect
package internal

import Flags._

trait BuildUtils { self: SymbolTable =>
  import definitions.{TupleClass, FunctionClass, MaxTupleArity, MaxFunctionArity, ScalaPackage, UnitClass}

  class BuildImpl extends BuildApi {

    def selectType(owner: Symbol, name: String): TypeSymbol =
      select(owner, newTypeName(name)).asType

    def selectTerm(owner: Symbol, name: String): TermSymbol = {
      val result = select(owner, newTermName(name)).asTerm
      if (result.isOverloaded) result.suchThat(!_.isMethod).asTerm
      else result
    }

    private def select(owner: Symbol, name: Name): Symbol = {
      val result = owner.info decl name
      if (result ne NoSymbol) result
      else
        mirrorThatLoaded(owner).missingHook(owner, name) orElse
        MissingRequirementError.notFound("%s %s in %s".format(if (name.isTermName) "term" else "type", name, owner.fullName))
    }

    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol = {
      val result = owner.info.decl(newTermName(name)).alternatives(index)
      if (result ne NoSymbol) result.asMethod
      else MissingRequirementError.notFound("overloaded method %s #%d in %s".format(name, index, owner.fullName))
    }

    def newFreeTerm(name: String, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
      newFreeTermSymbol(newTermName(name), value, flags, origin)

    def newFreeType(name: String, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      newFreeTypeSymbol(newTypeName(name), flags, origin)

    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: Long, isClass: Boolean): Symbol =
      owner.newNestedSymbol(name, pos, flags, isClass)

    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S =
      sym.setAnnotations(annots)

    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S =
      sym.setTypeSignature(tpe)

    def This(sym: Symbol): Tree = self.This(sym)

    def Select(qualifier: Tree, sym: Symbol): Select = self.Select(qualifier, sym)

    def Ident(sym: Symbol): Ident = self.Ident(sym)

    def TypeTree(tp: Type): TypeTree = self.TypeTree(tp)

    def thisPrefix(sym: Symbol): Type = sym.thisPrefix

    def setType[T <: Tree](tree: T, tpe: Type): T = { tree.setType(tpe); tree }

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T = { tree.setSymbol(sym); tree }

    def mkAnnotation(tree: Tree): Tree = tree match {
      case SyntacticNew(Nil, SyntacticApplied(SyntacticTypeApplied(_, _), _) :: Nil, emptyValDef, Nil) =>
        tree
      case _ =>
        throw new IllegalArgumentException(s"Tree ${showRaw(tree)} isn't a correct representation of annotation." +
                                            """Consider reformatting it into a q"new $name[..$targs](...$argss)" shape""")
    }

    def mkAnnotation(trees: List[Tree]): List[Tree] = trees.map(mkAnnotation)

    def mkVparamss(argss: List[List[ValDef]]): List[List[ValDef]] = argss.map(_.map(mkParam))

    def mkParam(vd: ValDef): ValDef = {
      var newmods = (vd.mods | PARAM) & (~DEFERRED)
      if (vd.rhs.nonEmpty) newmods |= DEFAULTPARAM
      copyValDef(vd)(mods = newmods)
    }

    def mkTparams(tparams: List[Tree]): List[TypeDef] =
      tparams.map {
        case td: TypeDef => copyTypeDef(td)(mods = (td.mods | PARAM) & (~DEFERRED))
        case other => throw new IllegalArgumentException("can't splice $other as type parameter")
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
        copyTypeDef(tdef)(mods =  mods | PRESUPER)
      case _ =>
        throw new IllegalArgumentException(s"not legal early def: $defn")
    }

    def mkEarlyDef(defns: List[Tree]): List[Tree] = defns.map(mkEarlyDef)

    def RefTree(qual: Tree, sym: Symbol) = self.RefTree(qual, sym.name) setSymbol sym

    object FlagsRepr extends FlagsReprExtractor {
      def apply(bits: Long): FlagSet = bits
      def unapply(flags: Long): Some[Long] = Some(flags)
    }

    object SyntacticTypeApplied extends SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree =
        if (targs.isEmpty) tree
        else if (tree.isTerm) TypeApply(tree, targs)
        else if (tree.isType) AppliedTypeTree(tree, targs)
        else throw new IllegalArgumentException(s"can't apply types to $tree")

      def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
        case TypeApply(fun, targs) => Some((fun, targs))
        case AppliedTypeTree(tpe, targs) => Some((tpe, targs))
        case _ => Some((tree, Nil))
      }
    }

    object SyntacticApplied extends SyntacticAppliedExtractor {
      def apply(tree: Tree, argss: List[List[Tree]]): Tree =
        argss.foldLeft(tree) { Apply(_, _) }

      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
        val treeInfo.Applied(fun, targs, argss) = tree
        Some((SyntacticTypeApplied(fun, targs), argss))
      }
    }

    private object UnCtor {
      def unapply(tree: Tree): Option[(Modifiers, List[List[ValDef]], List[Tree])] = tree match {
        case DefDef(mods, nme.MIXIN_CONSTRUCTOR, _, _, _, Block(lvdefs, _)) =>
          Some(mods | Flag.TRAIT, Nil, lvdefs)
        case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, Block(lvdefs :+ _, _)) =>
          Some(mods, vparamss, lvdefs)
        case _ => None
      }
    }

    private object UnMkTemplate {
      def unapply(templ: Template): Option[(List[Tree], ValDef, Modifiers, List[List[ValDef]], List[Tree], List[Tree])] = {
        val Template(parents, selfdef, tbody) = templ
        def result(ctorMods: Modifiers, vparamss: List[List[ValDef]], edefs: List[Tree], body: List[Tree]) =
          Some((parents, selfdef, ctorMods, vparamss, edefs, body))
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
            // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
            val vparamssRestoredImplicits = ctorVparamss match {
              case Nil :: (tail @ ((head :: _) :: _)) if head.mods.isImplicit => tail
              case other => other
            }
            // undo flag modifications by mergeing flag info from constructor args and fieldDefs
            val modsMap = fieldDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
            val vparamss = mmap(vparamssRestoredImplicits) { vd =>
              val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
              atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
            }
            result(ctorMods, vparamss, edefs, body)
          }
        }
      }
    }

    object SyntacticClassDef extends SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
                constrMods: Modifiers, vparamss: List[List[ValDef]], earlyDefs: List[Tree],
                parents: List[Tree], selfdef: ValDef, body: List[Tree]): ClassDef = {
        val extraFlags = PARAMACCESSOR | (if (mods.isCase) CASEACCESSOR else 0L)
        val vparamss0 = vparamss.map { _.map { vd => copyValDef(vd)(mods = (vd.mods | extraFlags) & (~DEFERRED)) } }
        val tparams0 = mkTparams(tparams)
        val parents0 = gen.mkParents(mods,
          if (mods.isCase) parents.filter {
            case ScalaDot(tpnme.Product | tpnme.Serializable | tpnme.AnyRef) => false
            case _ => true
          } else parents
        )
        val body0 = earlyDefs ::: body
        val templ = gen.mkTemplate(parents0, selfdef, constrMods, vparamss0, body0)
        gen.mkClassDef(mods, name, tparams0, templ)
      }

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                       List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfdef, ctorMods, vparamss, earlyDefs, body))
          if !ctorMods.isTrait && !ctorMods.hasFlag(JAVA) =>
          Some((mods, name, tparams, ctorMods, vparamss, earlyDefs, parents, selfdef, body))
        case _ =>
          None
      }
    }

    object SyntacticTraitDef extends SyntacticTraitDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef], earlyDefs: List[Tree],
                parents: List[Tree], selfdef: ValDef, body: List[Tree]): ClassDef = {
        val mods0 = mods | TRAIT | ABSTRACT
        val templ = gen.mkTemplate(parents, selfdef, Modifiers(TRAIT), Nil, earlyDefs ::: body)
        gen.mkClassDef(mods0, name, mkTparams(tparams), templ)
      }

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                       List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfdef, ctorMods, vparamss, earlyDefs, body))
          if mods.isTrait =>
          Some((mods, name, tparams, earlyDefs, parents, selfdef, body))
        case _ => None
      }
    }

    object SyntacticModuleDef extends SyntacticModuleDefExtractor {
      def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfdef: ValDef, body: List[Tree]) =
        ModuleDef(mods, name, gen.mkTemplate(parents, selfdef, NoMods, Nil, earlyDefs ::: body))

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case ModuleDef(mods, name, UnMkTemplate(parents, selfdef, _, _, earlyDefs, body)) =>
          Some((mods, name, earlyDefs, parents, selfdef, body))
        case _ =>
          None
      }
    }

    private trait ScalaMemberRef {
      val symbols: Seq[Symbol]
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
    private object TupleClassRef extends ScalaMemberRef {
      val symbols = TupleClass.filter { _ != null }.toSeq
    }
    private object TupleCompanionRef extends ScalaMemberRef {
      val symbols = TupleClassRef.symbols.map { _.companionModule }
    }
    private object UnitClassRef extends ScalaMemberRef {
      val symbols = Seq(UnitClass)
    }
    private object FunctionClassRef extends ScalaMemberRef {
      val symbols = FunctionClass.toSeq
    }

    object SyntacticTuple extends SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree = args match {
        case Nil      => Literal(Constant(()))
        case _        =>
          require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
          self.Apply(TupleClass(args.length).companionModule, args: _*)
      }

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case Literal(Constant(())) =>
          Some(Nil)
        case Apply(TupleCompanionRef(sym), args)
          if args.length <= MaxTupleArity
          && sym == TupleClass(args.length).companionModule =>
          Some(args)
        case _ =>
          None
      }
    }

    object SyntacticTupleType extends SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree = args match {
        case Nil => self.Select(self.Ident(nme.scala_), tpnme.Unit)
        case _   =>
          require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
          AppliedTypeTree(Ident(TupleClass(args.length)), args)
      }

      def unapply(tree: Tree): Option[List[Tree]] =  tree match {
        case UnitClassRef(_) =>
          Some(Nil)
        case AppliedTypeTree(TupleClassRef(sym), args)
          if args.length <= MaxTupleArity && sym == TupleClass(args.length) =>
          Some(args)
        case _ =>
          None
      }
    }

    object SyntacticFunctionType extends SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[Tree], restpe: Tree): Tree = {
        require(argtpes.length <= MaxFunctionArity + 1, s"Function types with arity bigger than $MaxFunctionArity aren't supported")
        gen.mkFunctionTypeTree(argtpes, restpe)
      }

      def unapply(tree: Tree): Option[(List[Tree], Tree)] = tree match {
        case AppliedTypeTree(FunctionClassRef(sym), args @ (argtpes :+ restpe))
          if args.length - 1 <= MaxFunctionArity && sym == FunctionClass(args.length - 1) =>
          Some((argtpes, restpe))
        case _ => None
      }
    }

    object SyntacticBlock extends SyntacticBlockExtractor {
      def apply(stats: List[Tree]): Tree = gen.mkBlock(stats)

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case self.Block(stats, expr) => Some(stats :+ expr)
        case _ if tree.isTerm => Some(tree :: Nil)
        case _ => None
      }
    }

    object SyntacticFunction extends SyntacticFunctionExtractor {
      def apply(params: List[ValDef], body: Tree): Tree = {
        val params0 = params.map { arg =>
          require(arg.rhs.isEmpty, "anonymous functions don't support default values")
          mkParam(arg)
        }
        Function(params0, body)
      }

      def unapply(tree: Tree): Option[(List[ValDef], Tree)] = tree match {
        case Function(params, body) => Some((params, body))
        case _ => None
      }
    }

    object SyntacticNew extends SyntacticNewExtractor {
      def apply(earlyDefs: List[Tree], parents: List[Tree], selfdef: ValDef, body: List[Tree]): Tree =
        gen.mkNew(parents, selfdef, earlyDefs ::: body, NoPosition, NoPosition)

      def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case SyntacticApplied(Select(New(SyntacticTypeApplied(ident, targs)), nme.CONSTRUCTOR), argss) =>
          Some((Nil, SyntacticApplied(SyntacticTypeApplied(ident, targs), argss) :: Nil, emptyValDef, Nil))
        case SyntacticBlock(SyntacticClassDef(_, tpnme.ANON_CLASS_NAME, Nil, _, List(Nil), earlyDefs, parents, selfdef, body) ::
                            Apply(Select(New(Ident(tpnme.ANON_CLASS_NAME)), nme.CONSTRUCTOR), Nil) :: Nil) =>
          Some((earlyDefs, parents, selfdef, body))
        case _ =>
          None
      }
    }

    object SyntacticDefDef extends SyntacticDefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[Tree], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef =
        DefDef(mods, name, mkTparams(tparams), mkVparamss(vparamss), tpt, rhs)

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[List[ValDef]], Tree, Tree)] = tree match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) => Some((mods, name, tparams, vparamss, tpt, rhs))
        case _ => None
      }
    }

    trait SyntacticValDefBase extends SyntacticValDefExtractor {
      val isMutable: Boolean

      def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) = {
        val mods1 = if (isMutable) mods | MUTABLE else mods
        ValDef(mods1, name, tpt, rhs)
      }

      def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = tree match {
        case ValDef(mods, name, tpt, rhs) if mods.hasFlag(MUTABLE) == isMutable =>
          Some((mods, name, tpt, rhs))
        case _ =>
          None
      }
    }

    object SyntacticValDef extends SyntacticValDefBase { val isMutable = false }
    object SyntacticVarDef extends SyntacticValDefBase { val isMutable = true }
  }

  val build: BuildApi = new BuildImpl
}
