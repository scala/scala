import scala.reflect.runtime.universe._


trait TreeSimiliarity {

  val ellipsisFlag = (1L << 60).asInstanceOf[FlagSet]
  val ellipsisMods = Modifiers(ellipsisFlag)
  val ellipsisTypeName = TypeName("…")
  val ellipsisTermName = TermName("…")

  val ellipsisTree = EmptyTree.asInstanceOf[scala.reflect.internal.SymbolTable#Tree].updateAttachment(ellipsis).asInstanceOf[Tree]
  val ellipsisValDef = ValDef(Modifiers(), ellipsis, EmptyTree, EmptyTree)

  object ellipsis {
    override def equals(other: Any) = other match {
      case name: TermName => name == ellipsisTermName
      case name: TypeName => name == ellipsisTypeName
      case flag: Long => ellipsisFlag == flag
      case mods: Modifiers => mods.hasFlag(ellipsisFlag)
      case Ident(name) => ellipsis == name
      case valdef: ValDef => valdef.name == ellipsisTermName
      case tree: Tree => tree == ellipsisTree
      case _ => other == ellipsis
    }
  }

  implicit def ellipsisToTermName(v: ellipsis.type): TermName = ellipsisTermName
  implicit def ellipsisToTypeName(v: ellipsis.type): TypeName = ellipsisTypeName
  implicit def ellipsisToFlagSet(v: ellipsis.type): FlagSet = ellipsisFlag
  implicit def ellipsisToModifiers(v: ellipsis.type): Modifiers = ellipsisMods
  implicit def ellipsisToTree(v: ellipsis.type): Tree = ellipsisTree
  implicit def ellipsisToString(v: ellipsis.type): String = "…"


  implicit class TestSimilar(tree1: Tree) {

    def ≈(tree2: Tree) = similar(tree1, tree2)

    private object similar {

      def apply(const1: Constant, const2: Constant) = const1.value == const2.value

      def apply(i1: Int, i2: Int) = i1 == i2

      def apply(name1: Name, name2: Name): Boolean =
        (name1 == name2) || (ellipsis == name1) || (ellipsis == name2)

      def apply(l1: List[Tree], l2: List[Tree]): Boolean =
        (l1.length == l2.length) && l1.zip(l2).forall(pair => apply(pair._1, pair._2))

      def apply1(ll1: List[List[Tree]], ll2: List[List[Tree]]): Boolean =
        (ll1.length == ll2.length) && ll1.zip(ll2).forall(pair => apply(pair._1, pair._2))

      def apply2(l1: List[ImportSelector], l2: List[ImportSelector]) =
        (l1.length == l2.length) && l1.zip(l2).forall(pair => apply(pair._1, pair._2))

      def apply(mod1: Modifiers, mod2: Modifiers): Boolean =
        (mod1 == mod2) || (ellipsis == mod1) || (ellipsis == mod2)

      def apply(sel1: ImportSelector, sel2: ImportSelector): Boolean =
        apply(sel1.name, sel2.name) && apply(sel1.rename, sel2.rename)

      def apply(t1: Tree, t2: Tree): Boolean = {
        val res = ((t1, t2) match {
          case (Alternative(trees1), Alternative(trees2)) =>
            apply(trees1, trees2)
          case (Annotated(annot1, arg1), Annotated(annot2, arg2)) =>
            apply(annot1, annot2) && apply(arg1, arg2)
          case (AppliedTypeTree(tpt1, args1), AppliedTypeTree(tpt2, args2)) =>
            apply(tpt1, tpt2) && apply(args1, args2)
          case (Apply(fun1, args1), Apply(fun2, args2)) =>
            apply(fun1, fun2) && apply(args1, args2)
          case (Assign(lhs1, rhs1), Assign(lhs2, rhs2)) =>
            apply(lhs1, lhs2) && apply(rhs1, rhs2)
          case (AssignOrNamedArg(lhs1, rhs1), AssignOrNamedArg(lhs2, rhs2)) =>
            apply(lhs1, lhs2) && apply(rhs1, rhs2)
          case (Bind(name1, body1), Bind(name2, body2)) =>
            apply(name1, name2) && apply(body1, body2)
          case (Block(stats1, expr1), Block(stats2, expr2)) =>
            apply(stats1, stats2) && apply(expr1, expr2)
          case (CaseDef(pat1, guard1, body1), CaseDef(pat2, guard2, body2)) =>
            apply(pat1, pat2) && apply(guard1, guard2) && apply(body1, body2)
          case (ClassDef(mods1, name1, tparams1, impl1),
                ClassDef(mods2, name2, tparams2, impl2)) =>
            apply(mods1, mods2) && apply(name1, name2) &&
            apply(tparams1, tparams2) && apply(impl1, impl2)
          case (CompoundTypeTree(templ1), CompoundTypeTree(templ2)) =>
            apply(templ1, templ2)
          case (DefDef(mods1, name1, tparams1, vparamss1, tpt1, rhs1),
                DefDef(mods2, name2, tparams2, vparamss2, tpt2, rhs2)) =>
            apply(mods1, mods2) && apply(name1, name2) &&
            apply(tparams1, tparams2) && apply1(vparamss1, vparamss2) &&
            apply(tpt1, tpt2) && apply(rhs1, rhs2)
          case (ExistentialTypeTree(tpt1, where1), ExistentialTypeTree(tpt2, where2)) =>
            apply(tpt1, tpt2) && apply(where1, where2)
          case (ValDef(mods1, name1, tpt1, rhs1), ValDef(mods2, name2, tpt2, rhs2)) =>
            apply(mods1, mods2) && apply(name1, name2) && apply(tpt1, tpt2) && apply(rhs1, rhs2)
          case (UnApply(fun1, args1), UnApply(fun2, args2)) =>
            apply(fun1, fun2) && apply(args1, args2)
          case (Typed(expr1, _), Typed(expr2, _)) =>
            apply(expr1, expr2)
          case (Function(vparams1, body1), Function(vparams2, body2)) =>
            apply(vparams1, vparams2) && apply(body1, body2)
          case (Ident(name1), Ident(name2)) =>
            apply(name1, name2)
          case (If(cond1, then1, else1), If(cond2, then2, else2)) =>
            apply(cond1, cond2) && apply(then1, then2) && apply(else1, else2)
          case (Import(expr1, sel1), Import(expr2, sel2)) =>
            apply(expr1, expr2) && apply2(sel1, sel2)
          case (ImportSelector(name1, namepos1, rename1, renamepos1),
                ImportSelector(name2, namepos2, rename2, renamepos2)) =>
            apply(name1, name2) && apply(namepos1, namepos2) &&
            apply(rename1, rename2) && apply(renamepos1, renamepos2)
          case (LabelDef(name1, params1, rhs1), LabelDef(name2, params2, rhs2)) =>
            apply(name1, name2) && apply(params1, params2) && apply(rhs1, rhs2)
          case (Literal(const1), Literal(const2)) =>
            apply(const1, const2)
          case (Match(sel1, cases1), Match(sel2, cases2)) =>
            apply(sel1, sel2) && apply(cases1, cases2)
          case (ModuleDef(mods1, name1, impl1), ModuleDef(mods2, name2, impl2)) =>
            apply(mods1, mods2) && apply(name1, name2) && apply(impl1, impl2)
          case (New(tpt1), New(tpt2)) =>
            apply(tpt1, tpt2)
          case (PackageDef(pid1, stats1), PackageDef(pid2, stats2)) =>
            apply(pid1, pid2) && apply(stats1, stats2)
          case (ReferenceToBoxed(ident1), ReferenceToBoxed(ident2)) =>
            apply(ident1, ident2)
          case (Return(expr1), Return(expr2)) =>
            apply(expr1, expr2)
          case (Select(qual1, name1), Select(qual2, name2)) =>
            apply(qual1, qual2) && apply(name1, name2)
          case (SelectFromTypeTree(qual1, name1), SelectFromTypeTree(qual2, name2)) =>
            apply(qual1, qual2) && apply(name1, name2)
          case (SingletonTypeTree(ref1), SingletonTypeTree(ref2)) =>
            apply(ref1, ref2)
          case (Star(el1), Star(el2)) =>
            apply(el1, el2)
          case (Super(qual1, mix1), Super(qual2, mix2)) =>
            apply(qual1, qual2) && apply(mix1, mix2)
          case (Template(parents1, self1, body1), Template(parents2, self2, body2)) =>
            apply(parents1, parents2) && apply(self1, self2) && apply(body1, body2)
          case (This(qual1), This(qual2)) =>
            apply(qual1, qual2)
          case (Throw(expr1), Throw(expr2)) =>
            apply(expr1, expr2)
          case (Try(block1, catches1, finalizer1), Try(block2, catches2, finalizer2)) =>
            apply(block1, block2) && apply(catches1, catches2) && apply(finalizer1, finalizer2)
          case (TypeApply(fun1, args1), TypeApply(fun2, args2)) =>
            apply(fun1, fun2) && apply(args1, args2)
          case (TypeBoundsTree(lo1, hi1), TypeBoundsTree(lo2, hi2)) =>
            apply(lo1, lo2) && apply(hi1, hi2)
          case (TypeDef(mods1, name1, tparams1, rhs1), TypeDef(mods2, name2, tparams2, rhs2)) =>
            apply(mods1, mods2) && apply(name1, name2) &&
            apply(tparams1, tparams2) && apply(rhs1, rhs2)
          case (TypeTree(), TypeTree()) => true
          case (EmptyTree, EmptyTree) => true
          case (DependentTypeTree(tpt1, args1), DependentTypeTree(tpt2, args2)) =>
            apply(tpt1, tpt2) && apply(args1, args2)
          case _ => false
        }) || (ellipsis == t1) || (ellipsis == t2)
        if(!res) println("---\n" + showRaw(t1, printIds=true) + "\n=/=\n" + showRaw(t2, printIds=true) )
        res
      }
    }
  }
}