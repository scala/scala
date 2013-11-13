package scala.tools.reflect
package quasiquotes

import scala.collection.{immutable, mutable}
import scala.reflect.internal.Flags._
import scala.reflect.macros.TypecheckException

class Cardinality private[Cardinality](val value: Int) extends AnyVal {
  def pred = { assert(value - 1 >= 0); new Cardinality(value - 1) }
  def succ = new Cardinality(value + 1)
  override def toString = if (value == 0) "no dots" else "." * (value + 1)
}

object Cardinality {
  val NoDot = new Cardinality(0)
  val DotDot = new Cardinality(1)
  val DotDotDot = new Cardinality(2)
  object Dot { def unapply(card: Cardinality) = card != NoDot }
  def parseDots(part: String) = {
    if (part.endsWith("...")) (part.stripSuffix("..."), DotDotDot)
    else if (part.endsWith("..")) (part.stripSuffix(".."), DotDot)
    else (part, NoDot)
  }
}

/** Defines abstractions that provide support for splicing into Scala syntax.
 */
trait Holes { self: Quasiquotes =>
  import global._
  import Cardinality._
  import definitions._
  import universeTypes._

  protected def inferParamImplicit(tfun: Type, targ: Type) = c.inferImplicitValue(appliedType(tfun, List(targ)), silent = true)
  protected def inferLiftable(tpe: Type): Tree = inferParamImplicit(liftableType, tpe)
  protected def inferUnliftable(tpe: Type): Tree = inferParamImplicit(unliftableType, tpe)
  protected def isLiftableType(tpe: Type) = inferLiftable(tpe) != EmptyTree
  protected def isNativeType(tpe: Type) =
    (tpe <:< treeType) || (tpe <:< nameType) || (tpe <:< modsType) ||
    (tpe <:< flagsType) || (tpe <:< symbolType)
  protected def stripIterable(tpe: Type, limit: Option[Cardinality] = None): (Cardinality, Type) =
    if (limit.map { _ == NoDot }.getOrElse { false }) (NoDot, tpe)
    else if (tpe != null && !isIterableType(tpe)) (NoDot, tpe)
    else {
      val (card, innerTpe) = stripIterable(tpe.typeArguments.head, limit.map { _.pred })
      (card.succ, innerTpe)
    }
  protected def iterableTypeFromCard(n: Cardinality, tpe: Type): Type = {
    if (n == NoDot) tpe
    else appliedType(IterableClass.toType, List(iterableTypeFromCard(n.pred, tpe)))
  }

  abstract class Hole {
    val tree: Tree
    val tpe: Type
  }

  object Hole {
    def apply(card: Cardinality, tree: Tree): Hole =
      if (method != nme.unapply) new ApplyHole(card, tree)
      else new UnapplyHole(card, tree)
    def unapply(hole: Hole): Some[(Tree, Type)] = Some((hole.tree, hole.tpe))
  }

  class ApplyHole(card: Cardinality, splicee: Tree) extends Hole {
    val (strippedTpe, tpe) = {
      if (stripIterable(splicee.tpe)._1.value < card.value) cantSplice()
      val (_, strippedTpe) = stripIterable(splicee.tpe, limit = Some(card))
      if (NativeType.unapply(strippedTpe)) (strippedTpe, iterableTypeFromCard(card, strippedTpe))
      else if (isLiftableType(strippedTpe)) (strippedTpe, iterableTypeFromCard(card, treeType))
      else cantSplice()
    }

    val tree = {
      def inner(itpe: Type)(tree: Tree) = itpe match {
        case NativeType()          => tree
        case ltpe @ LiftableType() => lifted(ltpe)(tree)
      }
      if (card == NoDot) inner(strippedTpe)(splicee)
      else iterated(card, strippedTpe, inner(strippedTpe))(splicee)
    }

    protected def cantSplice() = {
      val (iterableCard, iterableType) = stripIterable(splicee.tpe)
      val holeCardMsg = if (card != NoDot) s" with $card" else ""
      val action = "splice " + splicee.tpe + holeCardMsg
      val suggestCard = card != iterableCard || card != NoDot
      val spliceeCardMsg = if (card != iterableCard && iterableCard != NoDot) s"using $iterableCard" else "omitting the dots"
      val cardSuggestion = if (suggestCard) spliceeCardMsg else ""
      val suggestLifting = (card == NoDot || iterableCard != NoDot) && !(iterableType <:< treeType) && !isLiftableType(iterableType)
      val liftedTpe = if (card != NoDot) iterableType else splicee.tpe
      val liftSuggestion = if (suggestLifting) s"providing an implicit instance of Liftable[$liftedTpe]" else ""
      val advice = List(cardSuggestion, liftSuggestion).filter(_ != "").mkString(" or ")
      c.abort(splicee.pos, s"Can't $action, consider $advice")
    }

    protected def lifted(tpe: Type)(tree: Tree): Tree = {
      val lifter = inferLiftable(tpe)
      assert(lifter != EmptyTree, s"couldnt find a liftable for $tpe")
      val lifted = Apply(lifter, List(tree))
      val targetType = Select(u, tpnme.Tree)
      atPos(tree.pos)(TypeApply(Select(lifted, nme.asInstanceOf_), List(targetType)))
    }

    protected def iterated(card: Cardinality, tpe: Type, elementTransform: Tree => Tree = identity)(tree: Tree): Tree = {
      assert(card != NoDot)
      def reifyIterable(tree: Tree, n: Cardinality): Tree = {
        def loop(tree: Tree, n: Cardinality): Tree =
          if (n == NoDot) elementTransform(tree)
          else {
            val x: TermName = c.freshName()
            val wrapped = reifyIterable(Ident(x), n.pred)
            val xToWrapped = Function(List(ValDef(Modifiers(PARAM), x, TypeTree(), EmptyTree)), wrapped)
            Select(Apply(Select(tree, nme.map), List(xToWrapped)), nme.toList)
          }
        if (tree.tpe != null && (tree.tpe <:< listTreeType || tree.tpe <:< listListTreeType)) tree
        else atPos(tree.pos)(loop(tree, n))
      }
      reifyIterable(tree, card)
    }
  }

  class UnapplyHole(card: Cardinality, pat: Tree) extends Hole {
    val tpe  = iterableTypeFromCard(card, treeType)
    val tree = {
      val (placeholderName, tptopt) = pat match {
        case Bind(pname, Bind(_, Typed(Ident(nme.WILDCARD), tpt))) => (pname, Some(tpt))
        case Bind(pname, Typed(Ident(nme.WILDCARD), tpt))          => (pname, Some(tpt))
        case Bind(pname, _)                                        => (pname, None)
      }
      def fallback = Bind(placeholderName, Ident(nme.WILDCARD))
      tptopt.map { tpt =>
        val TypeDef(_, _, _, typedTpt) =
          try c.typeCheck(TypeDef(NoMods, TypeName("T"), Nil, tpt))
          catch { case TypecheckException(pos, msg) => c.abort(pos.asInstanceOf[c.Position], msg) }
        val tpe = typedTpt.tpe
        val (iterableCard, _) = stripIterable(tpe)
        if (iterableCard.value < card.value)
          c.abort(pat.pos, s"Can't extract $tpe with $card, consider using $iterableCard")
        val (_, strippedTpe) = stripIterable(tpe, limit = Some(card))
        if (strippedTpe <:< treeType) fallback
        else
          unlifters.spawn(strippedTpe, card).map {
            Apply(_, fallback :: Nil)
          }.getOrElse {
            c.abort(pat.pos, s"Can't find $unliftableType[$strippedTpe], consider providing it")
          }
      }.getOrElse { fallback }
    }
  }

  object unlifters {
    private var records = List.empty[(Type, Cardinality)]
    def spawn(tpe: Type, card: Cardinality): Option[Tree] = {
      val unlifter = inferUnliftable(tpe)
      if (unlifter == EmptyTree) None
      else if (card == NoDot) Some(unlifter)
      else {
        val idx = records.indexWhere { p => p._1 =:= tpe && p._2 == card }
        val resIdx = if (idx != -1) idx else { records +:= (tpe, card); records.length - 1}
        Some(Ident(TermName("helper$" + resIdx)))
      }
    }
    def preamble(): List[Tree] =
      records.zipWithIndex.map { case ((tpe, card), idx) =>
        val name = TermName("helper$" + idx)
        val helperName = card match { case DotDot => nme.UnliftHelper1 case DotDotDot => nme.UnliftHelper2 }
        val lifter = inferUnliftable(tpe)
        ValDef(NoMods, name, EmptyTree, Apply(Select(Select(u, nme.build), helperName), lifter :: Nil))
      }
  }

  object NativeType { def unapply(tpe: Type): Boolean = isNativeType(tpe) }
  object LiftableType { def unapply(tpe: Type): Boolean = isLiftableType(tpe) }
  object TreeType { def unapply(tpe: Type): Boolean = tpe <:< treeType }
  object IterableType {
    def unapply(tpe: Type): Option[(Cardinality, Type)] = {
      val (card, innerTpe) = stripIterable(tpe)
      if (card != NoDot) Some((card, innerTpe)) else None
    }
  }
  object WithCard {
    def unapply(tpe: Type): Some[Cardinality] = tpe match {
      case IterableType(card, _) => Some(card)
      case _                     => Some(NoDot)
    }
  }
}