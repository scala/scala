package scala
package reflect
package internal

import Flags._
import util._

trait BuildUtils { self: SymbolTable =>
  import definitions.{TupleClass, FunctionClass, ScalaPackage, UnitClass}

  class BuildImpl extends BuildApi {
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
      case SyntacticNew(Nil, SyntacticApplied(SyntacticTypeApplied(_, _), _) :: Nil, noSelfType, Nil) =>
        tree
      case _ =>
        throw new IllegalArgumentException(s"Tree ${showRaw(tree)} isn't a correct representation of annotation." +
                                            """Consider reformatting it into a q"new $name[..$targs](...$argss)" shape""")
    }

    def mkAnnotation(trees: List[Tree]): List[Tree] = trees.map(mkAnnotation)

    def mkVparamss(argss: List[List[Tree]]): List[List[ValDef]] = argss.map(_.map(mkParam))

    def mkParam(tree: Tree): ValDef = tree match {
      case vd: ValDef =>
        var newmods = (vd.mods | PARAM) & (~DEFERRED)
        if (vd.rhs.nonEmpty) newmods |= DEFAULTPARAM
        copyValDef(vd)(mods = newmods)
      case _ =>
        throw new IllegalArgumentException(s"$tree is not valid represenation of function parameter, " +
                                            """consider reformatting it into q"val $name: $T = $default" shape""")
    }

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

    def RefTree(qual: Tree, sym: Symbol) = self.RefTree(qual, sym.name) setSymbol sym

    def withFreshTermName[T](prefix: String)(f: TermName => T): T = f(freshTermName(prefix))

    def withFreshTypeName[T](prefix: String)(f: TypeName => T): T = f(freshTypeName(prefix))

    protected implicit def fresh: FreshNameCreator = self.currentFreshNameCreator

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
        argss.foldLeft(tree) { (f, args) => Apply(f, args.map(treeInfo.assignmentToMaybeNamedArg)) }

      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
        val treeInfo.Applied(fun, targs, argss) = tree
        Some((SyntacticTypeApplied(fun, targs), argss))
      }
    }

    // recover constructor contents generated by gen.mkTemplate
    protected object UnCtor {
      def unapply(tree: Tree): Option[(Modifiers, List[List[ValDef]], List[Tree])] = tree match {
        case DefDef(mods, nme.MIXIN_CONSTRUCTOR, _, _, _, Block(lvdefs, _)) =>
          Some((mods | Flag.TRAIT, Nil, lvdefs))
        case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, Block(lvdefs :+ _, _)) =>
          Some((mods, vparamss, lvdefs))
        case _ => None
      }
    }

    // undo gen.mkTemplate
    protected object UnMkTemplate {
      def unapply(templ: Template): Option[(List[Tree], ValDef, Modifiers, List[List[ValDef]], List[Tree], List[Tree])] = {
        val Template(parents, selfType, tbody) = templ
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
            // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
            val vparamssRestoredImplicits = ctorVparamss match {
              case Nil :: (tail @ ((head :: _) :: _)) if head.mods.isImplicit => tail
              case other => other
            }
            // undo flag modifications by mergeing flag info from constructor args and fieldDefs
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
                constrMods: Modifiers, vparamss: List[List[Tree]], earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef = {
        val extraFlags = PARAMACCESSOR | (if (mods.isCase) CASEACCESSOR else 0L)
        val vparamss0 = vparamss.map { _.map {
          case vd: ValDef => copyValDef(vd)(mods = (vd.mods | extraFlags) & (~DEFERRED))
          case tree => throw new IllegalArgumentException(s"$tree is not valid representation of class parameter, " +
                                                          """consider reformatting it into q"val $name: $T = $default" shape""")
        } }
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
                parents: List[Tree], selfType: Tree, body: List[Tree]) =
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
                parents: List[Tree], selfType: Tree, body: List[Tree]): Tree =
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
        gen.mkTuple(args, flattenUnary = false)
      }

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case Literal(Constant(())) =>
          Some(Nil)
        case Apply(MaybeTypeTreeOriginal(SyntacticTypeApplied(MaybeSelectApply(TupleCompanionRef(sym)), targs)), args)
          if sym == TupleClass(args.length).companionModule
          && (targs.isEmpty || targs.length == args.length) =>
          Some(args)
        case _ =>
          None
      }
    }

    object SyntacticTupleType extends SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree = {
        require(args.isEmpty || TupleClass(args.length).exists, s"Tuples with ${args.length} arity aren't supported")
        gen.mkTupleType(args, flattenUnary = false)
      }

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case MaybeTypeTreeOriginal(UnitClassRef(_)) =>
          Some(Nil)
        case MaybeTypeTreeOriginal(AppliedTypeTree(TupleClassRef(sym), args))
          if sym == TupleClass(args.length) =>
          Some(args)
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

    object SyntacticBlock extends SyntacticBlockExtractor {
      def apply(stats: List[Tree]): Tree = gen.mkBlock(stats)

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case self.Block(stats, expr) => Some(stats :+ expr)
        case _ if tree.isTerm => Some(tree :: Nil)
        case _ => None
      }
    }

    object SyntacticFunction extends SyntacticFunctionExtractor {
      def apply(params: List[Tree], body: Tree): Tree = {
        val params0 :: Nil = mkVparamss(params :: Nil)
        require(params0.forall { _.rhs.isEmpty }, "anonymous functions don't support default values")
        Function(params0, body)
      }

      def unapply(tree: Tree): Option[(List[ValDef], Tree)] = tree match {
        case Function(params, body) => Some((params, body))
        case _ => None
      }
    }

    object SyntacticNew extends SyntacticNewExtractor {
      def apply(earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): Tree =
        gen.mkNew(parents, mkSelfType(selfType), earlyDefs ::: body, NoPosition, NoPosition)

      def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
        case SyntacticApplied(Select(New(SyntacticTypeApplied(ident, targs)), nme.CONSTRUCTOR), argss) =>
          Some((Nil, SyntacticApplied(SyntacticTypeApplied(ident, targs), argss) :: Nil, noSelfType, Nil))
        case SyntacticBlock(SyntacticClassDef(_, tpnme.ANON_CLASS_NAME, Nil, _, ListOfNil, earlyDefs, parents, selfType, body) ::
                            Apply(Select(New(Ident(tpnme.ANON_CLASS_NAME)), nme.CONSTRUCTOR), Nil) :: Nil) =>
          Some((earlyDefs, parents, selfType, body))
        case _ =>
          None
      }
    }

    object SyntacticDefDef extends SyntacticDefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[Tree], vparamss: List[List[Tree]], tpt: Tree, rhs: Tree): DefDef =
        DefDef(mods, name, mkTparams(tparams), mkVparamss(vparamss), tpt, rhs)

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[List[ValDef]], Tree, Tree)] = tree match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) => Some((mods, name, tparams, vparamss, tpt, rhs))
        case _ => None
      }
    }

    protected class SyntacticValDefBase(isMutable: Boolean) extends SyntacticValDefExtractor {
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

    def UnliftHelper1[T](unliftable: Unliftable[T]) = new UnliftHelper1[T] {
      def unapply(lst: List[Tree]): Option[List[T]] = {
        val unlifted = lst.flatMap { unliftable.unapply(_) }
        if (unlifted.length == lst.length) Some(unlifted) else None
      }
    }

    def UnliftHelper2[T](unliftable: Unliftable[T]) = new UnliftHelper2[T] {
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

    // abstract over possible alternative representations of no type in valdef
    protected object EmptyTypTree {
      def unapply(tree: Tree): Boolean = tree match {
        case EmptyTree => true
        case tt: TypeTree if (tt.original == null || tt.original.isEmpty) => true
        case _ => false
      }
    }

    // match a sequence of desugared `val $pat = $value`
    protected object UnPatSeq {
      def unapply(trees: List[Tree]): Option[List[(Tree, Tree)]] = trees match {
        case Nil => Some(Nil)
        // case q"$mods val ${_}: ${_} = ${MaybeUnchecked(value)} match { case $pat => (..$ids) }" :: tail
        case ValDef(mods, _, _, Match(MaybeUnchecked(value), CaseDef(pat, EmptyTree, SyntacticTuple(ids)) :: Nil)) :: tail
          if mods.hasFlag(SYNTHETIC) && mods.hasFlag(ARTIFACT) =>
          tail.drop(ids.length) match {
            case UnPatSeq(rest) => Some((pat, value) :: rest)
            case _ => None
          }
        // case q"${_} val $name1: ${_} = ${MaybeUnchecked(value)} match { case $pat => ${Ident(name2)} }" :: UnPatSeq(rest)
        case ValDef(_, name1, _, Match(MaybeUnchecked(value), CaseDef(pat, EmptyTree, Ident(name2)) :: Nil)) :: UnPatSeq(rest)
          if name1 == name2 =>
          Some((pat, value) :: rest)
        // case q"${_} val $name: ${EmptyTypTree()} = $value" :: UnPatSeq(rest) =>
        case ValDef(_, name, EmptyTypTree(), value) :: UnPatSeq(rest) =>
          Some((Bind(name, self.Ident(nme.WILDCARD)), value) :: rest)
        // case q"${_} val $name: $tpt = $value" :: UnPatSeq(rest) =>
        case ValDef(_, name, tpt, value) :: UnPatSeq(rest) =>
          Some((Bind(name, Typed(self.Ident(nme.WILDCARD), tpt)), value) :: rest)
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
            case EmptyTypTree() => Some((Bind(name, self.Ident(nme.WILDCARD)), body))
            case _              => Some((Bind(name, Typed(self.Ident(nme.WILDCARD), tpt)), body))
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
        case t => throw new IllegalArgumentException(s"$t is not a valid fist enumerator of for loop")
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
        case Annotated(SyntacticNew(Nil, Apply(ScalaDot(tpnme.unchecked), Nil) :: Nil, noSelfType, Nil), annottee) =>
          Some(annottee)
        case Typed(annottee, MaybeTypeTreeOriginal(
          Annotated(SyntacticNew(Nil, Apply(ScalaDot(tpnme.unchecked), Nil) :: Nil, noSelfType, Nil), _))) =>
          Some(annottee)
        case annottee => Some(annottee)
      }
    }

    protected def mkCases(cases: List[Tree]): List[CaseDef] = cases.map {
      case c: CaseDef => c
      case tree => throw new IllegalArgumentException("$tree is not valid representation of pattern match case")
    }

    object SyntacticMatch extends SyntacticMatchExtractor {
      def apply(selector: Tree, cases: List[Tree]) = Match(selector, mkCases(cases))
      def unapply(tree: Match): Option[(Tree, List[CaseDef])] = Match.unapply(tree)
    }

    object SyntacticTry extends SyntacticTryExtractor {
      def apply(block: Tree, catches: List[Tree], finalizer: Tree) = Try(block, mkCases(catches), finalizer)
      def unapply(tree: Try): Option[(Tree, List[CaseDef], Tree)] = Try.unapply(tree)
    }

    object SyntacticIdent extends SyntacticIdentExtractor {
      def apply(name: Name, isBackquoted: Boolean) = {
        val id = self.Ident(name)
        if (isBackquoted) id updateAttachment BackquotedIdentifierAttachment
        id
      }
      def unapply(tree: Ident): Some[(Name, Boolean)] = Some((tree.name, tree.hasAttachment[BackquotedIdentifierAttachment.type]))
    }
  }

  val build: BuildImpl = new BuildImpl
}
