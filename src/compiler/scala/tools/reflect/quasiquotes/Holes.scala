package scala.tools.reflect
package quasiquotes

import scala.collection.{immutable, mutable}
import scala.reflect.internal.Flags._

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

  protected lazy val IterableTParam = IterableClass.typeParams(0).asType.toType
  protected def inferParamImplicit(tfun: Type, targ: Type) = c.inferImplicitValue(appliedType(tfun, List(targ)), silent = true)
  protected def inferLiftable(tpe: Type): Tree = inferParamImplicit(liftableType, tpe)
  protected def isLiftableType(tpe: Type) = inferLiftable(tpe) != EmptyTree
  protected def isNativeType(tpe: Type) =
    (tpe <:< treeType) || (tpe <:< nameType) || (tpe <:< modsType) ||
    (tpe <:< flagsType) || (tpe <:< symbolType)
  protected def isBottomType(tpe: Type) =
    tpe <:< NothingClass.tpe || tpe <:< NullClass.tpe
  protected def stripIterable(tpe: Type, limit: Option[Cardinality] = None): (Cardinality, Type) =
    if (limit.map { _ == NoDot }.getOrElse { false }) (NoDot, tpe)
    else if (tpe != null && !isIterableType(tpe)) (NoDot, tpe)
    else if (isBottomType(tpe)) (NoDot, tpe)
    else {
      val targ = IterableTParam.asSeenFrom(tpe, IterableClass)
      val (card, innerTpe) = stripIterable(targ, limit.map { _.pred })
      (card.succ, innerTpe)
    }
  protected def iterableTypeFromCard(n: Cardinality, tpe: Type): Type = {
    if (n == NoDot) tpe
    else appliedType(IterableClass.toType, List(iterableTypeFromCard(n.pred, tpe)))
  }

  /** Hole encapsulates information about splices in quasiquotes.
   *  It packs together a cardinality of a splice, pre-reified tree
   *  representation (possibly preprocessed) and position.
   */
  abstract class Hole {
    val tree: Tree
    val pos: Position
    val cardinality: Cardinality
  }

  object Hole {
    def apply(card: Cardinality, tree: Tree): Hole =
      if (method != nme.unapply) new ApplyHole(card, tree)
      else new UnapplyHole(card, tree)
    def unapply(hole: Hole): Some[(Tree, Cardinality)] = Some((hole.tree, hole.cardinality))
  }

  class ApplyHole(card: Cardinality, splicee: Tree) extends Hole {
    val (strippedTpe: Type, tpe: Type) = {
      if (stripIterable(splicee.tpe)._1.value < card.value) cantSplice()
      val (_, strippedTpe) = stripIterable(splicee.tpe, limit = Some(card))
      if (isBottomType(strippedTpe)) cantSplice()
      else if (isNativeType(strippedTpe)) (strippedTpe, iterableTypeFromCard(card, strippedTpe))
      else if (isLiftableType(strippedTpe)) (strippedTpe, iterableTypeFromCard(card, treeType))
      else cantSplice()
    }

    val tree = {
      def inner(itpe: Type)(tree: Tree) =
        if (isNativeType(itpe)) tree
        else if (isLiftableType(itpe)) lifted(itpe)(tree)
        else global.abort("unreachable")
      if (card == NoDot) inner(strippedTpe)(splicee)
      else iterated(card, strippedTpe, inner(strippedTpe))(splicee)
    }

    val pos = splicee.pos

    val cardinality = stripIterable(tpe)._1

    protected def cantSplice(): Nothing = {
      val (iterableCard, iterableType) = stripIterable(splicee.tpe)
      val holeCardMsg = if (card != NoDot) s" with $card" else ""
      val action = "splice " + splicee.tpe + holeCardMsg
      val suggestCard = card != iterableCard || card != NoDot
      val spliceeCardMsg = if (card != iterableCard && iterableCard != NoDot) s"using $iterableCard" else "omitting the dots"
      val cardSuggestion = if (suggestCard) spliceeCardMsg else ""
      val suggestLifting = (card == NoDot || iterableCard != NoDot) && !(iterableType <:< treeType) && !isLiftableType(iterableType)
      val liftedTpe = if (card != NoDot) iterableType else splicee.tpe
      val liftSuggestion = if (suggestLifting) s"providing an implicit instance of Liftable[$liftedTpe]" else ""
      val advice =
        if (isBottomType(iterableType)) "bottom type values often indicate programmer mistake"
        else "consider " + List(cardSuggestion, liftSuggestion).filter(_ != "").mkString(" or ")
      c.abort(splicee.pos, s"Can't $action, $advice")
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

  class UnapplyHole(val cardinality: Cardinality, pat: Tree) extends Hole {
    val (tree, pos) = pat match {
      case Bind(pname, inner) => (Bind(pname, Ident(nme.WILDCARD)), inner.pos)
    }
  }
}
