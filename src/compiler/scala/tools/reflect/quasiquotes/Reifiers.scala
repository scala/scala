package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros
import scala.collection.immutable.ListMap

trait Reifiers { self: Quasiquotes =>
  import global._
  import global.Flag._
  import global.treeInfo._
  import global.definitions._

  type Placeholders = ListMap[String, (Tree, Int)]

  abstract class Reifier(val universe: Tree, val placeholders: Placeholders) extends {

    val global: self.global.type = self.global
    val mirror = EmptyTree
    val typer = null
    val reifee = null
    val concrete = false

  } with ReflectReifier with Types {

    /** Extractor that matches simple identity-like trees which
     *  correspond to placeholders within quasiquote.
     */
    object Placeholder {

      def unapply(tree: Tree): Option[String] = {
        val name = tree match {
          case Ident(name) => name.toString
          case TypeDef(_, name, List(), TypeBoundsTree(
            Select(Select(Ident(nme.ROOTPKG), nme.scala_), tpnme.Nothing),
            Select(Select(Ident(nme.ROOTPKG), nme.scala_), tpnme.Any))) => name.toString
          case ValDef(_, name, TypeTree(), EmptyTree) => name.toString
          case _ => ""
        }
        if (placeholders.contains(name))
          Some(name)
        else
          None
      }
    }

    override def reifyTree(tree: Tree): Tree = reifyBasicTree(tree)
  }

  class ApplyReifier(universe: Tree, placeholders: Placeholders) extends Reifier(universe, placeholders) {

    object CorrespondsTo {

      def unapply(name: Name): Option[(Tree, Type)] = unapply(name.toString)

      def unapply(name: String): Option[(Tree, Type)] =
        placeholders.get(name).flatMap { case (tree, card) =>
          (card, tree.tpe) match {
            case (0, tpe) if tpe <:< treeType || tpe <:< nameType =>
              Some((tree, tpe))
            case (0, LiftableType(lift)) =>
              Some((wrapLift(lift, tree), treeType))
            case (card, iterable) if card > 0 && iterable <:< iterableType =>
              extractIterableN(card, iterable).map {
                case tpe if tpe <:< treeType =>
                  Some((wrapIterableN(tree, card) { t => t }, iterableN(card, tpe)))
                case LiftableType(lift) =>
                  Some((wrapIterableN(tree, card) { t => wrapLift(lift, t) }, iterableN(card, treeType)))
                case tpe =>
                  c.abort(tree.pos, s"Can't splice an Iterable of non-liftable type $tpe.")
              }.getOrElse {
                c.abort(tree.pos, s"Incorrect cardinality.")
              }
            case (card, tpe) =>
              c.abort(tree.pos, s"Splicing of type $tpe with '${fmtCard(card)}' cardinality isn't supported.")
          }
        }

      def wrapLift(lift: Tree, tree: Tree) =
        q"$lift($u, $tree).asInstanceOf[$u.Tree]"

      def wrapIterableN(tree: Tree, n: Int)(default: Tree => Tree): Tree = n match {
        case 0 => default(tree)
        case _ =>
          val x: TermName = c.freshName()
          val wrapped = wrapIterableN(Ident(x), n - 1)(default)
          q"$tree.map { $x => $wrapped }.toList"
      }

      object LiftableType {
        def unapply(tpe: Type): Option[Tree] = {
          val liftType = appliedType(liftableType, List(tpe))
          val lift = c.inferImplicitValue(liftType, silent = true)
          if (lift != EmptyTree)
            Some(lift)
          else
            None
        }
      }

      def iterableN(n: Int, tpe: Type): Type =
        if (n == 0)
          tpe
        else
          appliedType(IterableClass.toType, List(iterableN(n - 1, tpe)))

      def extractIterableN(n: Int, tpe: Type): Option[Type] =
        if (n == 0)
          Some(tpe)
        else
          if (tpe <:< iterableType)
            extractIterableN(n - 1, tpe.typeArguments(0))
          else
            None
    }

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< treeType => tree
      case Apply(f, List(Placeholder(CorrespondsTo(argss, tpe)))) if tpe <:< iterableIterableTreeType =>
        val f1 = reifyTree(f)
        q"$argss.foldLeft[$u.Tree]($f1) { $u.Apply(_, _) }"
      case Placeholder(name) if placeholders(name)._2 > 0 =>
        val (tree, card) = placeholders(name)
        c.abort(tree.pos, s"Can't splice tree with '${fmtCard(card)}' cardinality in this position.")
      case _ =>
        super.reifyBasicTree(tree)
    }

    override def reifyName(name: Name): Tree = name match {
      case CorrespondsTo(tree, tpe) =>
        if (tpe <:< nameType)
          tree
        else
          c.abort(tree.pos, s"Name expected but ${tpe} found.")
      case _ =>
        super.reifyName(name)
    }

    override def reifyList(xs: List[Any]): Tree =
      Select(
        mkList(xs.map {
          case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< iterableTreeType => tree
          case List(Placeholder(CorrespondsTo(tree, tpe))) if tpe <:< iterableIterableTreeType => tree
          case x @ _ => mkList(List(reify(x)))
        }),
        nme.flatten)
  }

  class UnapplyReifier(universe: Tree, placeholders: Placeholders) extends Reifier(universe, placeholders) {

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case global.emptyValDef =>
        mirrorBuildCall("EmptyValDefLike")
      case global.pendingSuperCall =>
        mirrorBuildCall("PendingSuperCallLike")
      case Placeholder(name) =>
        Bind(TermName(name), Ident(nme.WILDCARD))
      case Applied(fun, targs, argss) if fun != tree =>
        if (targs.length > 0)
          mirrorBuildCall("Applied", reify(fun), reifyList(targs), reifyList(argss))
        else
          mirrorBuildCall("Applied2", reify(fun), reifyList(argss))
      case _ =>
        super.reifyBasicTree(tree)
    }

    override def scalaFactoryCall(name: String, args: Tree*): Tree =
      call("scala." + name, args: _*)

    override def reifyName(name: Name): Tree =
      if (!placeholders.contains(name.toString))
        super.reifyName(name)
      else {
        Bind(TermName(name.toString), Ident(nme.WILDCARD))
      }

    override def reifyModifiers(m: global.Modifiers) =
      mirrorFactoryCall(nme.Modifiers, mirrorBuildCall("FlagsAsBits", reify(m.flags)), reify(m.privateWithin), reify(m.annotations))

    override def reifyList(xs: List[Any]): Tree = {
      val last = if (xs.length > 0) xs.last else EmptyTree
      last match {
        case Placeholder(name) if placeholders(name)._2 == 1 =>
          val bnd = Bind(TermName(name), Ident(nme.WILDCARD))
          xs.init.foldRight[Tree](bnd) { (el, rest) =>
            scalaFactoryCall("collection.immutable.$colon$colon", reify(el), rest)
          }
        case List(Placeholder(name)) if placeholders(name)._2 == 2 =>
          val bnd = Bind(TermName(name), Ident(nme.WILDCARD))
          xs.init.foldRight[Tree](bnd) { (el, rest) =>
            scalaFactoryCall("collection.immutable.$colon$colon", reify(el), rest)
          }
        case _ =>
          super.reifyList(xs)
      }
    }
  }

  trait Types {
    val universe: Tree

    lazy val universeType = universe.tpe
    lazy val nameType = memberType(universeType, tpnme.Name)
    lazy val termNameType = memberType(universeType, tpnme.TypeName)
    lazy val typeNameType = memberType(universeType, tpnme.TermName)
    lazy val treeType = memberType(universeType, tpnme.Tree)
    lazy val typeDefType = memberType(universeType, tpnme.TypeDef)
    lazy val liftableType = LiftableClass.toType
    lazy val iterableType = typeOf[Iterable[_]]//IterableClass.toType
    lazy val iterableTreeType = appliedType(iterableType, List(treeType))
    lazy val iterableIterableTreeType = appliedType(iterableType, List(iterableTreeType))
    lazy val optionTreeType = appliedType(OptionClass.toType, List(treeType))
    lazy val optionNameType = appliedType(OptionClass.toType, List(nameType))
  }

  def memberType(thistype: Type, name: TypeName): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(name)
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }

  def fmtCard(cardinality: Int) =
    if (cardinality == 0)
      ""
    else
      "." * (cardinality + 1)
}