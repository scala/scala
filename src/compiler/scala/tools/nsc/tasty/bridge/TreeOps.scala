package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyUniverse
import scala.tools.nsc.tasty.TastyName, TastyName.TypeName

trait TreeOps { self: TastyUniverse =>
  import self.{symbolTable => u}, u.{internal => ui}

  object untpd {
    final val EmptyTypeIdent: Ident = new Ident(u.nme.EMPTY) {
      override def isEmpty: Boolean = true
    }
    final val EmptyTree: Tree = u.EmptyTree
  }

  object tpd {
    @inline final def Constant(value: Any): Constant = u.Constant(value)
    def Ident(name: TastyName)(tpe: Type): Ident = u.Ident(encodeTastyName(name)).setType(tpe)
    def Select(qual: Tree, name: TastyName)(tpe: Type): Tree = u.Select(qual, encodeTastyName(name)).setType(tpe)
    def This(name: TypeName)(tpe: Type): Tree = u.This(encodeTypeName(name)).setType(tpe)
    def This(qual: Ident)(tref: TypeRef): Tree = u.This(qual.name.toTypeName).setType(ui.thisType(tref.sym))
    def New(tpt: Tree): Tree = u.New(tpt).setType(tpt.tpe)
    def SingletonTypeTree(ref: Tree): Tree = u.SingletonTypeTree(ref).setType(ref.tpe)
    def ByNameTypeTree(arg: Tree): Tree = u.gen.mkFunctionTypeTree(Nil, arg).setType(u.definitions.byNameType(arg.tpe))
    def NamedArg(name: TastyName, value: Tree): Tree = u.NamedArg(u.Ident(encodeTastyName(name)), value).setType(value.tpe)
    def Super(qual: Tree, mixId: Ident)(mixTpe: Type): Tree = {
      val owntype = (
        if (!mixId.isEmpty) mixTpe
        else ui.intersectionType(qual.tpe.parents)
      )
      u.Super(qual, mixId.name.toTypeName).setType(mkSuperType(qual.tpe, owntype))
    }

    def PathTree(tpe: Type): Tree = tpe match {
      case _:TypeRef | _:u.SingleType => u.TypeTree(tpe)
      case path: u.ThisType           => u.This(path.sym.name.toTypeName).setType(path)
      case path: u.ConstantType       => u.Literal(path.value).setType(tpe)
    }

    @inline final def TypeTree(tp: Type): Tree = u.TypeTree(tp)

    def LambdaTypeTree(tparams: List[Symbol], body: Tree): Tree = {
      u.TypeTree(mkLambdaFromParams(tparams, body.tpe))
    }

    def Typed(expr: Tree, tpt: Tree): Tree = u.Typed(expr, tpt).setType(tpt.tpe)

    def Apply(fun: Tree, args: List[Tree]): Tree = u.Apply(fun, args).setType(fnResult(fun.tpe))

    def TypeApply(fun: Tree, args: List[Tree]): Tree =
      u.TypeApply(fun, args).setType(tyconResult(fun.tpe, args.map(_.tpe)))

    def If(cond: Tree, thenp: Tree, elsep: Tree): Tree =
      u.If(cond, thenp, elsep).setType(if (elsep == u.EmptyTree) defn.UnitTpe else u.lub(thenp.tpe :: elsep.tpe :: Nil))

    def SeqLiteral(trees: List[Tree], tpt: Tree): Tree = u.ArrayValue(tpt, trees).setType(tpt.tpe)

    def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit ctx: Context): Tree = {
      if (tpt.tpe === AndType) {
        u.CompoundTypeTree(u.Template(args, u.noSelfType, Nil)).setType(ui.intersectionType(args.map(_.tpe)))
      } else {
        u.AppliedTypeTree(tpt, args).setType(boundedAppliedType(tpt.tpe, args.map(_.tpe)))
      }
    }

    def Annotated(tpt: Tree, annot: Tree)(implicit ctx: Context): Tree = {
      defn.repeatedAnnotationClass match {
        case Some(repeated)
        if annot.tpe.typeSymbol === repeated
        && tpt.tpe.typeSymbol.isSubClass(defn.SeqClass)
        && tpt.tpe.typeArgs.length == 1 =>
          tpd.TypeTree(defn.scalaRepeatedType(tpt.tpe.typeArgs.head))
        case _ =>
          u.Annotated(annot, tpt).setType(u.AnnotatedType(mkAnnotation(annot) :: Nil, tpt.tpe))
      }
    }

    def TypeBoundsTree(lo: Tree, hi: Tree): Tree = {
      u.TypeBoundsTree(lo, hi).setType(u.TypeBounds(lo.tpe, hi.tpe))
    }
  }

}
