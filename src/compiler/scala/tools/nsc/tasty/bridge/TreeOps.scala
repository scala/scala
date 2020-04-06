package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyUniverse

trait TreeOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  object untpd {
    final val EmptyTypeIdent: Ident = new Ident(nme.EMPTY) {
      override def isEmpty: Boolean = true
    }
    final val EmptyTree: Tree = u.EmptyTree
  }

  object tpd {
    @inline final def Constant(value: Any): Constant = u.Constant(value)
  }

  @inline final def Annotated(tpt: Tree, annot: Tree): Annotated = u.Annotated(annot, tpt)
  @inline final def Block(stats: List[Tree], value: Tree): Block = u.Block((stats :+ value):_*)
  @inline final def SingletonTypeTree(ref: Tree): SingletonTypeTree = u.SingletonTypeTree(ref)
  @inline final def Apply(fun: Tree, args: List[Tree]): Apply = u.Apply(fun, args)
  @inline final def TypeApply(fun: Tree, args: List[Tree]): TypeApply = u.TypeApply(fun, args)
  @inline final def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree = u.AppliedTypeTree(tpt, args)
  @inline final def Super(qual: Tree, mix: TypeName): Super = u.Super(qual, mix)
  @inline final def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree = u.TypeBoundsTree(lo, hi)
  @inline final def CompoundTypeTree(tps: List[Tree]): CompoundTypeTree = u.CompoundTypeTree(u.Template(tps, u.noSelfType, Nil))
  @inline final def NamedArg(name: Name, value: Tree): NamedArg = u.NamedArg(Ident(name), value)
  @inline final def RefTree(qual: Tree, name: Name): RefTree = u.RefTree(qual, name)
  @inline final def TypeTree(tp: Type): TypeTree = u.TypeTree(tp)
  @inline final def This(name: TypeName): This = u.This(name)
  @inline final def SeqLiteral(trees: List[Tree], tpt: Tree): SeqLiteral = u.ArrayValue(tpt, trees)
  @inline final def Typed(expr: Tree, tpt: Tree): Typed = u.Typed(expr, tpt)
  @inline final def Literal(c: Constant): Literal = u.Literal(c)
  @inline final def Ident(name: Name): Ident = u.Ident(name)
  @inline final def New(tpt: Tree): New = u.New(tpt)
  @inline final def If(cond: Tree, thenp: Tree, elsep: Tree): If = u.If(cond, thenp, elsep)
  @inline final def Select(qual: Tree, name: Name): Select = u.Select(qual, name)

  @inline final def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = u.gen.mkFunctionTypeTree(argtpes, restpe)

  def noPosition: Position = u.NoPosition

}
