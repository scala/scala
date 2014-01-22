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
  protected def extractIterableTParam(tpe: Type) =
    IterableTParam.asSeenFrom(tpe, IterableClass)
  protected def stripIterable(tpe: Type, limit: Option[Cardinality] = None): (Cardinality, Type) =
    if (limit.map { _ == NoDot }.getOrElse { false }) (NoDot, tpe)
    else if (tpe != null && !isIterableType(tpe)) (NoDot, tpe)
    else if (isBottomType(tpe)) (NoDot, tpe)
    else {
      val targ = extractIterableTParam(tpe)
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
      else iterated(card, splicee, splicee.tpe)
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

    protected def toList(tree: Tree, tpe: Type): Tree =
      if (isListType(tpe)) tree
      else Select(tree, nme.toList)

    protected def mapF(tree: Tree, f: Tree => Tree): Tree =
      if (f(Ident(TermName("x"))) equalsStructure Ident(TermName("x"))) tree
      else {
        val x: TermName = c.freshName()
        // q"$tree.map { $x => ${f(Ident(x))} }"
        Apply(Select(tree, nme.map),
          Function(ValDef(Modifiers(PARAM), x, TypeTree(), EmptyTree) :: Nil,
            f(Ident(x))) :: Nil)
      }

    protected object IterableType {
      def unapply(tpe: Type): Option[Type] =
        if (isIterableType(tpe)) Some(extractIterableTParam(tpe)) else None
    }

    protected object LiftedType {
      def unapply(tpe: Type): Option[Tree => Tree] =
        if (tpe <:< treeType) Some(t => t)
        else if (isLiftableType(tpe)) Some(lifted(tpe)(_))
        else None
    }

    /** Map high-cardinality splice onto an expression that eveluates as a list of given cardinality.
     *
     *  All possible combinations of representations are given in the table below:
     *
     *    input                          output for T <: Tree          output for T: Liftable
     *
     *    ..${x: List[T]}                x                             x.map(lift)
     *    ..${x: Iterable[T]}            x.toList                      x.toList.map(lift)
     *
     *    ...${x: List[List[T]]}         x                             x.map { _.map(lift) } }
     *    ...${x: List[Iterable[T]}      x.map { _.toList }            x.map { _.toList.map(lift) } }
     *    ...${x: Iterable[List[T]]}     x.toList                      x.toList.map { _.map(lift) }
     *    ...${x: Iterable[Iterable[T]]} x.toList { _.toList }         x.toList.map { _.toList.map(lift) }
     *
     *  As you can see table is quite repetetive. Middle column is equivalent to the right one with
     *  lift function equal to identity. Cases with List are equivalent to Iterated ones (due to
     *  the fact that toList method call is just an identity we can omit it altogether.)
     */
    protected def iterated(card: Cardinality, tree: Tree, tpe: Type): Tree = (card, tpe) match {
      case (DotDot, tpe @ IterableType(LiftedType(lift))) => mapF(toList(tree, tpe), lift)
      case (DotDotDot, tpe @ IterableType(inner))         => mapF(toList(tree, tpe), t => iterated(DotDot, t, inner))
      case _                                              => global.abort("unreachable")
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
   *  helper extractors that would do the job: UnliftListElementwise[T]. Similarly
   *  List[List[Tree]] needs UnliftListOfListsElementwise[T].
   *
   *  See also "unlift list" tests in UnapplyProps.scala
   */
  object unlifters {
    private var records = List.empty[(Type, Cardinality)]
    // Materialize unlift helper that does elementwise
    // unlifting for corresponding cardinality and type.
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
        val helperName = card match {
          case DotDot    => nme.UnliftListElementwise
          case DotDotDot => nme.UnliftListOfListsElementwise
        }
        val lifter = inferUnliftable(tpe)
        assert(helperName.isTermName)
        // q"val $name: $u.build.${helperName.toTypeName} = $u.build.$helperName($lifter)"
        ValDef(NoMods, name,
          AppliedTypeTree(Select(Select(u, nme.build), helperName.toTypeName), List(TypeTree(tpe))),
          Apply(Select(Select(u, nme.build), helperName), lifter :: Nil))
      }
  }
}
