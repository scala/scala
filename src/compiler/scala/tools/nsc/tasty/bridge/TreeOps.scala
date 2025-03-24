/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.{TastyUniverse, TastyModes}, TastyModes._

import scala.tools.tasty.TastyName
import scala.reflect.internal.Flags

/**This layer adds factories that construct typed `scala.reflect` Trees in the shapes that TASTy expects
 */
trait TreeOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  object untpd {
    final val EmptyTree: Tree = u.EmptyTree
  }

  private class TastyIdent(val tname: TastyName) extends u.Ident(encodeTastyName(tname))

  implicit class TreeDecorator(val tree: Tree) {
    def typeIdent: TastyName.TypeName = tree match {
      case tree: TastyIdent => tree.tname.toTypeName
      case _                => TastyName.EmptyTpe
    }
  }

  def showTree(tree: Tree)(implicit ctx: Context): String = {
    // here we want to avoid forcing the symbols of type trees,
    // so instead substitute the type tree with an Identifier
    // of the `showType`, which does not force.
    val tree1 = tree.transform(new u.Transformer {
      override def transform(tree: Tree) = tree match {
        case tree: u.TypeTree => u.Ident(s"${showType(tree.tpe, wrap = false)}") // ident prints its name directly
        case tree => super.transform(tree)
      }
    })
    u.show(tree1)
  }

  object tpd {

    @inline final def Constant(value: Any): Constant =
      u.Constant(value)

    @inline final def Ident(name: TastyName)(tpe: Type): Tree =
      new TastyIdent(name).setType(tpe)

    @inline final def Select(qual: Tree, name: TastyName)(implicit ctx: Context): Tree =
      selectImpl(qual, name)(implicit ctx => lookupTypeFrom(qual.tpe)(qual.tpe, name))

    @inline final def Select(owner: Type)(qual: Tree, name: TastyName)(implicit ctx: Context): Tree =
      selectImpl(qual, name)(implicit ctx => lookupTypeFrom(owner)(qual.tpe, name))

    private def selectImpl(qual: Tree, name: TastyName)(lookup: Context => Type)(implicit ctx: Context): Tree = {

      def selectName(qual: Tree, name: TastyName)(lookup: Context => Type) =
        u.Select(qual, encodeTastyName(name)).setType(lookup(ctx))

      def selectCtor(qual: Tree) =
        u.Select(qual, u.nme.CONSTRUCTOR).setType(qual.tpe.typeSymbol.primaryConstructor.tpe)

      if (ctx.mode.is(ReadAnnotationCtor) && name.isSignedConstructor)
        selectCtor(qual)
      else
        selectName(qual, name)(lookup)

    }

    @inline final def This(qual: TastyName.TypeName)(tpe: Type): Tree =
      u.This(encodeTypeName(qual)).setType(tpe)

    @inline final def New(tpt: Tree): Tree =
      u.New(tpt).setType(safeClassType(tpt.tpe))

    @inline final def SingletonTypeTree(ref: Tree): Tree =
      u.SingletonTypeTree(ref).setType(ref.tpe)

    @inline final def ByNameTypeTree(arg: Tree): Tree =
      u.gen.mkFunctionTypeTree(Nil, arg).setType(u.definitions.byNameType(arg.tpe))

    @inline final def NamedArg(name: TastyName, value: Tree): Tree =
      u.NamedArg(u.Ident(encodeTastyName(name)), value).setType(value.tpe)

    def Super(qual: Tree, mixId: TastyName.TypeName)(mixTpe: Type): Tree = {
      val owntype = (
        if (!mixId.isEmpty) mixTpe
        else u.intersectionType(qual.tpe.parents)
      )
      u.Super(qual, encodeTypeName(mixId)).setType(u.SuperType(qual.tpe, owntype))
    }

    def PathTree(tpe: Type): Tree = tpe match {
      case _:u.TypeRef | _:u.SingleType => u.TypeTree(tpe)
      case path: u.ThisType             => u.This(path.sym.name.toTypeName).setType(path)
      case path: u.ConstantType         => u.Literal(path.value).setType(tpe)
      case x                            => throw new MatchError(x)
    }

    @inline final def TypeTree(tp: Type): Tree = u.TypeTree(tp)

    @inline final def LambdaTypeTree(tparams: List[Symbol], body: Tree): Tree =
      u.TypeTree(defn.LambdaFromParams(tparams, body.tpe))

    def Macro(impl: Tree): Tree = impl match {
      case tree @ u.TypeApply(qual, args) =>
        u.TypeApply(Macro(qual), args).setType(tree.tpe)
      case tree @ u.Select(pre, sel) =>
        val sym = if (sel.isTermName) tree.tpe.termSymbol else tree.tpe.typeSymbol
        u.Select(Macro(pre), sym).setType(tree.tpe)
      case tree: u.TypeTree if tree.tpe.prefix !== u.NoType =>
        val sym = tree.tpe match {
          case u.SingleType(_, sym) => sym
          case u.TypeRef(_, sym, _) => sym
          case u.ThisType(sym)      => sym
          case x                    => throw new MatchError(x)
        }
        if (tree.tpe.prefix === u.NoPrefix && (sym.hasFlag(Flags.PACKAGE) && !sym.isPackageObjectOrClass || sym.isLocalToBlock)) {
          if (sym.isLocalToBlock) u.Ident(sym).setType(tree.tpe)
          else u.This(sym).setType(tree.tpe)
        }
        else {
          u.Select(Macro(u.TypeTree(tree.tpe.prefix)), sym).setType(tree.tpe)
        }
      case tree =>
        tree
    }

    @inline final def Typed(expr: Tree, tpt: Tree): Tree = u.Typed(expr, tpt).setType(tpt.tpe)

    @inline final def Apply(fun: Tree, args: List[Tree]): Tree = u.Apply(fun, args).setType(fnResult(fun.tpe))

    def TypeApply(fun: Tree, args: List[Tree]): Tree = {
      if (u.definitions.isPredefMemberNamed(fun.tpe.termSymbol, u.TermName("classOf"))) {
        assert(args.length == 1 && !fun.tpe.termSymbol.isOverloaded)
        u.Literal(Constant(args.head.tpe))
      }
      else {
        u.TypeApply(fun, args).setType(tyconResult(fun.tpe, args.map(_.tpe)))
      }
    }

    def If(cond: Tree, thenp: Tree, elsep: Tree): Tree =
      u.If(cond, thenp, elsep).setType(
        if (elsep === u.EmptyTree) u.definitions.UnitTpe
        else u.lub(thenp.tpe :: elsep.tpe :: Nil)
      )

    @inline final def SeqLiteral(trees: List[Tree], tpt: Tree): Tree = u.ArrayValue(tpt, trees).setType(tpt.tpe)

    def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit ctx: Context): Tree = {
      if (tpt.tpe === AndTpe) {
        u.CompoundTypeTree(u.Template(args, u.noSelfType, Nil)).setType(u.intersectionType(args.map(_.tpe)))
      }
      else if (ctx.isJava && u.definitions.isScalaRepeatedParamType(tpt.tpe)) {
        u.AppliedTypeTree(tpt, args).setType(u.definitions.javaRepeatedType(args.head.tpe))
      }
      else {
        u.AppliedTypeTree(tpt, args).setType(defn.AppliedType(tpt.tpe, args.map(_.tpe)))
      }
    }

    def Annotated(tpt: Tree, annot: Tree)(implicit ctx: Context): Tree = {
      if (annot.tpe.typeSymbol === defn.RepeatedAnnot
          && tpt.tpe.typeSymbol.isSubClass(u.definitions.SeqClass)
          && tpt.tpe.typeArgs.length == 1) {
        if (ctx.isJava) tpd.TypeTree(u.definitions.javaRepeatedType(tpt.tpe.typeArgs.head))
        else tpd.TypeTree(u.definitions.scalaRepeatedType(tpt.tpe.typeArgs.head))
      }
      else {
        u.Annotated(annot, tpt).setType(defn.AnnotatedType(tpt.tpe, annot))
      }
    }

    def RefinedTypeTree(parent: Tree, decls: List[Tree], refinedCls: Symbol)(implicit ctx: Context): Tree = {
      refinedCls.info.parents.head match {
        case defn.PolyFunctionType() =>
          val polyType = refinedCls.info.decls.map(_.tpe).headOption.fold(defn.NoType)(x => x)
          polyFuncIsUnsupported(polyType)
        case _ =>
          u.CompoundTypeTree(u.Template(parent :: Nil, u.noSelfType, decls)).setType(refinedCls.info)
      }
    }

    def TypeBoundsTree(lo: Tree, hi: Tree, alias: Tree): Tree = {
      val tpe = alias match {
        case untpd.EmptyTree => u.TypeBounds(lo.tpe, hi.tpe)
        case alias           => new OpaqueTypeBounds(lo.tpe, hi.tpe, alias.tpe)
      }
      u.TypeBoundsTree(lo, hi).setType(tpe)
    }
  }

}
