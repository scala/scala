package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros
import scala.collection.Map
import scala.collection.mutable

trait Reifiers { self: Quasiquotes =>
  import global._
  import global.Flag._
  import global.treeInfo._
  import global.definitions._

  type Placeholders = Map[String, (Tree, String)]

  abstract class Reifier(val universe: Tree, val placeholders: Placeholders) extends {

    val global: self.global.type = self.global
    val mirror = EmptyTree
    val typer = null
    val reifee = null
    val concrete = false

  } with ReflectReifier with Types {

    val u = nme.UNIVERSE_SHORT

    override def reifyTree(tree: Tree) = reifyBasicTree(tree)

    // Extractor that matches simple identity-like trees which
    // correspond to placeholders within quasiquote.
    object SimpleTree {

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
  }

  class ApplyReifier(universe: Tree, placeholders: Placeholders) extends Reifier(universe, placeholders) {

    object SubsToTree {

      def unapply(name: Name): Option[(Tree, String)] =
        unapply(name.toString)

      def unapply(name: String): Option[(Tree, String)] =
        placeholders.get(name).flatMap { case (tree, card) =>
          if (tree.tpe <:< treeType) {
            if (card != "")
              throw new Exception(s"Incorrect cardinality, expected '', got '$card'")
            Some((tree, card))
          } else if (tree.tpe <:< iterableTreeType) {
            if (card != "..")
              throw new Exception(s"Incorrect cardinality, expected '..', but got '$card'")
            Some((wrapIterable(tree), card))
          } else if (tree.tpe <:< iterableIterableTreeType) {
            if (card != "...")
              throw new Exception(s"Incorrect cardinality, expected '...', but got '$card'")
            Some((wrapIterable2(tree), card))
          } else {
            val liftType = appliedType(liftableType, List(tree.tpe))
            val lift = c.inferImplicitValue(liftType, silent = true)
            if (lift != EmptyTree) {
              if (card != "")
                throw new Exception(s"Incorrect cardinality, expected '', but got '$card'")
              Some((wrapLift(lift, tree), card))
            } else
              None
          }
        }

      def wrapLift(lift: Tree, tree: Tree) = q"$lift($universe, $tree).asInstanceOf[$u.Tree]"
      def wrapIterable(tree: Tree) = q"$tree.toList"
      def wrapIterable2(tree: Tree) = q"$tree.map { _.toList }.toList"
    }

    object SubsToNameTree {

      def unapply(name: Name): Option[Tree] =
        placeholders.get(name.toString).collect { case (tree, _) if tree.tpe <:< nameType => tree }
    }

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case SimpleTree(SubsToTree(tree, "")) => tree
      case Apply(f, List(SimpleTree(SubsToTree(argss, "...")))) =>
        val f1 = reifyTree(f)
        q"$argss.foldLeft[$u.Tree]($f1) { $u.Apply(_, _) }"
      case _ => super.reifyBasicTree(tree)
    }

    override def reifyName(name: Name): Tree =
      if (!placeholders.contains(name.toString))
        super.reifyName(name)
      else
        name match {
          case SubsToNameTree(tree) => tree
          case _ => throw new Exception(s"Name expected but ${placeholders(name.toString)._1.tpe} found [$name:${placeholders(name.toString)}]")
      }

    override def reifyList(xs: List[Any]): Tree =
      Select(
        mkList(xs.map {
          case SimpleTree(SubsToTree(tree, "..")) => tree
          case List(SimpleTree(SubsToTree(tree, "..."))) => tree
          case x @ _ => mkList(List(reify(x)))
        }),
        nme.flatten)
  }

  class UnapplyReifier(universe: Tree, placeholders: Placeholders) extends Reifier(universe, placeholders) {

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case SimpleTree(name) =>
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

    override def reifyName(name: Name): Tree = {
      if (!placeholders.contains(name.toString))
        super.reifyName(name)
      else {
        Bind(TermName(name.toString), Ident(nme.WILDCARD))
      }
    }
    override def reifyModifiers(m: global.Modifiers) =
      mirrorFactoryCall(nme.Modifiers, mirrorBuildCall("FlagsAsBits", reify(m.flags)), reify(m.privateWithin), reify(m.annotations))

    override def reifyList(xs: List[Any]): Tree = {
      val last = if (xs.length > 0) xs.last else EmptyTree
      last match {
        case SimpleTree(name) if placeholders(name)._2 == ".." =>
          val bnd = Bind(TermName(name), Ident(nme.WILDCARD))
          xs.init.foldRight[Tree](bnd) { (el, rest) =>
            scalaFactoryCall("collection.immutable.$colon$colon", reify(el), rest)
          }
        case List(SimpleTree(name)) if placeholders(name)._2 == "..." =>
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
    lazy val iterableTreeType = appliedType(IterableClass.toType, List(treeType))
    lazy val iterableIterableTreeType = appliedType(IterableClass.toType, List(iterableTreeType))
    lazy val optionTreeType = appliedType(OptionClass.toType, List(treeType))
    lazy val optionNameType = appliedType(OptionClass.toType, List(nameType))
  }

  def memberType(thistype: Type, name: TypeName): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(name)
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }
}