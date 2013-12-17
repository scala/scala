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

  protected lazy val IterableTParam = IterableClass.typeParams(0).asType.toType
  protected def inferParamImplicit(tfun: Type, targ: Type) = c.inferImplicitValue(appliedType(tfun, List(targ)), silent = true)
  protected def inferLiftable(tpe: Type): Tree = inferParamImplicit(liftableType, tpe)
  protected def inferUnliftable(tpe: Type): Tree = inferParamImplicit(unliftableType, tpe)
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
    val (strippedTpe, tpe): (Type, Type) = {
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
      atPos(tree.pos)(lifted)
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
    val (placeholderName, pos, tptopt) = pat match {
      case Bind(pname, inner @ Bind(_, Typed(Ident(nme.WILDCARD), tpt))) => (pname, inner.pos, Some(tpt))
      case Bind(pname, inner @ Typed(Ident(nme.WILDCARD), tpt))          => (pname, inner.pos, Some(tpt))
      case Bind(pname, inner)                                            => (pname, inner.pos, None)
    }
    val treeNoUnlift = Bind(placeholderName, Ident(nme.WILDCARD))
    lazy val tree =
      tptopt.map { tpt =>
        val TypeDef(_, _, _, typedTpt) =
          try c.typeCheck(TypeDef(NoMods, TypeName("T"), Nil, tpt))
          catch { case TypecheckException(pos, msg) => c.abort(pos.asInstanceOf[c.Position], msg) }
        val tpe = typedTpt.tpe
        val (iterableCard, _) = stripIterable(tpe)
        if (iterableCard.value < cardinality.value)
          c.abort(pat.pos, s"Can't extract $tpe with $cardinality, consider using $iterableCard")
        val (_, strippedTpe) = stripIterable(tpe, limit = Some(cardinality))
        if (strippedTpe <:< treeType) treeNoUnlift
        else
          unlifters.spawn(strippedTpe, cardinality).map {
            Apply(_, treeNoUnlift :: Nil)
          }.getOrElse {
            c.abort(pat.pos, s"Can't find $unliftableType[$strippedTpe], consider providing it")
          }
      }.getOrElse { treeNoUnlift }
  }

  /** Full support for unliftable implies that it's possible to interleave
   *  deconstruction with higher cardinality and unlifting of the values.
   *  In particular extraction of List[Tree] as List[T: Unliftable] requires
   *  helper extractors that would do the job: UnliftHelper1[T]. Similarly
   *  List[List[Tree]] needs UnliftHelper2[T].
   *
   *  See also "unlift list" tests in UnapplyProps.scala
   */
  object unlifters {
    private var records = List.empty[(Type, Cardinality)]
    // Request an UnliftHelperN[T] where n == card and T == tpe.
    // If card == 0 then helper is not needed and plain instance
    // of unliftable is returned.
    def spawn(tpe: Type, card: Cardinality): Option[Tree] = {
      val unlifter = inferUnliftable(tpe)
      if (unlifter == EmptyTree) None
      else if (card == NoDot) Some(unlifter)
      else {
        val idx = records.indexWhere { p => p._1 =:= tpe && p._2 == card }
        val resIdx = if (idx != -1) idx else { records +:= (tpe, card); records.length - 1}
        Some(Ident(TermName(nme.QUASIQUOTE_UNLIFT_HELPER + resIdx)))
      }
    }
    // Returns a list of vals that will defined required unlifters
    def preamble(): List[Tree] =
      records.zipWithIndex.map { case ((tpe, card), idx) =>
        val name = TermName(nme.QUASIQUOTE_UNLIFT_HELPER + idx)
        val helperName = card match { case DotDot => nme.UnliftHelper1 case DotDotDot => nme.UnliftHelper2 }
        val lifter = inferUnliftable(tpe)
        assert(helperName.isTermName)
        // q"val $name: $u.build.${helperName.toTypeName} = $u.build.$helperName($lifter)"
        ValDef(NoMods, name,
          AppliedTypeTree(Select(Select(u, nme.build), helperName.toTypeName), List(TypeTree(tpe))),
          Apply(Select(Select(u, nme.build), helperName), lifter :: Nil))
      }
  }
}
