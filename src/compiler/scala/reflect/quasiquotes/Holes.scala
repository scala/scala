package scala.reflect
package quasiquotes

import scala.reflect.internal.Flags._
import scala.reflect.macros.TypecheckException

class Rank private[Rank](val value: Int) extends AnyVal {
  def pred = { assert(value - 1 >= 0); new Rank(value - 1) }
  def succ = new Rank(value + 1)
  override def toString = if (value == 0) "no dots" else "." * (value + 1)
}

object Rank {
  val NoDot = new Rank(0)
  val DotDot = new Rank(1)
  val DotDotDot = new Rank(2)
  object Dot { def unapply(rank: Rank) = rank != NoDot }
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
  import Rank._
  import definitions._
  import universeTypes._

  private lazy val IterableTParam = IterableClass.typeParams(0).asType.toType
  private def inferParamImplicit(tfun: Type, targ: Type) = c.inferImplicitValue(appliedType(tfun, List(targ)), silent = true)
  private def inferLiftable(tpe: Type): Tree = inferParamImplicit(liftableType, tpe)
  private def inferUnliftable(tpe: Type): Tree = inferParamImplicit(unliftableType, tpe)
  private def isLiftableType(tpe: Type) = inferLiftable(tpe) != EmptyTree
  private def isNativeType(tpe: Type) =
    (tpe <:< treeType) || (tpe <:< nameType) || (tpe <:< modsType) ||
    (tpe <:< flagsType) || (tpe <:< symbolType)
  private def isBottomType(tpe: Type) =
    tpe <:< NothingClass.tpe || tpe <:< NullClass.tpe
  private def extractIterableTParam(tpe: Type) =
    IterableTParam.asSeenFrom(tpe, IterableClass)
  private def stripIterable(tpe: Type, limit: Rank = DotDotDot): (Rank, Type) =
    if (limit == NoDot) (NoDot, tpe)
    else if (tpe != null && !isIterableType(tpe)) (NoDot, tpe)
    else if (isBottomType(tpe)) (NoDot, tpe)
    else {
      val targ = extractIterableTParam(tpe)
      val (rank, innerTpe) = stripIterable(targ, limit.pred)
      (rank.succ, innerTpe)
    }
  private def iterableTypeFromRank(n: Rank, tpe: Type): Type = {
    if (n == NoDot) tpe
    else appliedType(IterableClass.toType, List(iterableTypeFromRank(n.pred, tpe)))
  }

  /** Hole encapsulates information about unquotees in quasiquotes.
   *  It packs together a rank, pre-reified tree representation
   *  (possibly preprocessed) and position.
   */
  abstract class Hole {
    val tree: Tree
    val pos: Position
    val rank: Rank
  }

  object Hole {
    def apply(rank: Rank, tree: Tree): Hole =
      if (method != nme.unapply) new ApplyHole(rank, tree)
      else new UnapplyHole(rank, tree)
    def unapply(hole: Hole): Some[(Tree, Rank)] = Some((hole.tree, hole.rank))
  }

  class ApplyHole(annotatedRank: Rank, unquotee: Tree) extends Hole {
    val (strippedTpe, tpe): (Type, Type) = {
      val (strippedRank, strippedTpe) = stripIterable(unquotee.tpe, limit = annotatedRank)
      if (isBottomType(strippedTpe)) cantSplice()
      else if (isNativeType(strippedTpe)) {
        if (strippedRank != NoDot && !(strippedTpe <:< treeType) && !isLiftableType(strippedTpe)) cantSplice()
        else (strippedTpe, iterableTypeFromRank(annotatedRank, strippedTpe))
      } else if (isLiftableType(strippedTpe)) (strippedTpe, iterableTypeFromRank(annotatedRank, treeType))
      else cantSplice()
    }

    val tree = {
      def inner(itpe: Type)(tree: Tree) =
        if (isNativeType(itpe)) tree
        else if (isLiftableType(itpe)) lifted(itpe)(tree)
        else global.abort("unreachable")
      if (annotatedRank == NoDot) inner(strippedTpe)(unquotee)
      else iterated(annotatedRank, unquotee, unquotee.tpe)
    }

    val pos = unquotee.pos

    val rank = stripIterable(tpe)._1

    private def cantSplice(): Nothing = {
      val (iterableRank, iterableType) = stripIterable(unquotee.tpe)
      val holeRankMsg = if (annotatedRank != NoDot) s" with $annotatedRank" else ""
      val action = "unquote " + unquotee.tpe + holeRankMsg
      val suggestRank = annotatedRank != iterableRank || annotatedRank != NoDot
      val unquoteeRankMsg = if (annotatedRank != iterableRank && iterableRank != NoDot) s"using $iterableRank" else "omitting the dots"
      val rankSuggestion = if (suggestRank) unquoteeRankMsg else ""
      val suggestLifting = (annotatedRank == NoDot || iterableRank != NoDot) && !(iterableType <:< treeType) && !isLiftableType(iterableType)
      val liftedTpe = if (annotatedRank != NoDot) iterableType else unquotee.tpe
      val liftSuggestion = if (suggestLifting) s"providing an implicit instance of Liftable[$liftedTpe]" else ""
      val advice =
        if (isBottomType(iterableType)) "bottom type values often indicate programmer mistake"
        else "consider " + List(rankSuggestion, liftSuggestion).filter(_ != "").mkString(" or ")
      c.abort(unquotee.pos, s"Can't $action, $advice")
    }

    private def lifted(tpe: Type)(tree: Tree): Tree = {
      val lifter = inferLiftable(tpe)
      assert(lifter != EmptyTree, s"couldnt find a liftable for $tpe")
      val lifted = Apply(lifter, List(tree))
      atPos(tree.pos)(lifted)
    }

    private def toStats(tree: Tree): Tree =
      // q"$u.internal.reificationSupport.toStats($tree)"
      Apply(Select(Select(Select(u, nme.internal), nme.reificationSupport), nme.toStats), tree :: Nil)

    private def toList(tree: Tree, tpe: Type): Tree =
      if (isListType(tpe)) tree
      else Select(tree, nme.toList)

    private def mapF(tree: Tree, f: Tree => Tree): Tree =
      if (f(Ident(TermName("x"))) equalsStructure Ident(TermName("x"))) tree
      else {
        val x = TermName(c.freshName())
        // q"$tree.map { $x => ${f(Ident(x))} }"
        Apply(Select(tree, nme.map),
          Function(ValDef(Modifiers(PARAM), x, TypeTree(), EmptyTree) :: Nil,
            f(Ident(x))) :: Nil)
      }

    private object IterableType {
      def unapply(tpe: Type): Option[Type] =
        if (isIterableType(tpe)) Some(extractIterableTParam(tpe)) else None
    }

    private object LiftedType {
      def unapply(tpe: Type): Option[Tree => Tree] =
        if (tpe <:< treeType) Some(t => t)
        else if (isLiftableType(tpe)) Some(lifted(tpe)(_))
        else None
    }

    /** Map high-rank unquotee onto an expression that evaluates as a list of given rank.
     *
     *  All possible combinations of representations are given in the table below:
     *
     *    input                          output for T <: Tree          output for T: Liftable
     *
     *    ..${x: Iterable[T]}            x.toList                      x.toList.map(lift)
     *    ..${x: T}                      toStats(x)                    toStats(lift(x))
     *
     *    ...${x: Iterable[Iterable[T]]} x.toList { _.toList }         x.toList.map { _.toList.map(lift) }
     *    ...${x: Iterable[T]}           x.toList.map { toStats(_) }   x.toList.map { toStats(lift(_)) }
     *    ...${x: T}                     toStats(x).map { toStats(_) } toStats(lift(x)).map { toStats(_) }
     *
     *  For optimization purposes `x.toList` is represented as just `x` if it is statically known that
     *  x is not just an Iterable[T] but a List[T]. Similarly no mapping is performed if mapping function is
     *  known to be an identity.
     */
    private def iterated(rank: Rank, tree: Tree, tpe: Type): Tree = (rank, tpe) match {
      case (DotDot, tpe @ IterableType(LiftedType(lift))) => mapF(toList(tree, tpe), lift)
      case (DotDot, LiftedType(lift))                     => toStats(lift(tree))
      case (DotDotDot, tpe @ IterableType(inner))         => mapF(toList(tree, tpe), t => iterated(DotDot, t, inner))
      case (DotDotDot, LiftedType(lift))                  => mapF(toStats(lift(tree)), toStats)
      case _                                              => global.abort("unreachable")
    }
  }

  class UnapplyHole(val rank: Rank, pat: Tree) extends Hole {
    val (placeholderName, pos, tptopt) = pat match {
      case Bind(pname, inner @ Bind(_, Typed(Ident(nme.WILDCARD), tpt))) => (pname, inner.pos, Some(tpt))
      case Bind(pname, inner @ Typed(Ident(nme.WILDCARD), tpt))          => (pname, inner.pos, Some(tpt))
      case Bind(pname, inner)                                            => (pname, inner.pos, None)
    }
    val treeNoUnlift = Bind(placeholderName, Ident(nme.WILDCARD))
    lazy val tree =
      tptopt.map { tpt =>
        val TypeDef(_, _, _, typedTpt) =
          try c.typecheck(TypeDef(NoMods, TypeName("T"), Nil, tpt))
          catch { case TypecheckException(pos, msg) => c.abort(pos.asInstanceOf[c.Position], msg) }
        val tpe = typedTpt.tpe
        val (iterableRank, _) = stripIterable(tpe)
        if (iterableRank.value < rank.value)
          c.abort(pat.pos, s"Can't extract $tpe with $rank, consider using $iterableRank")
        val (_, strippedTpe) = stripIterable(tpe, limit = rank)
        if (strippedTpe <:< treeType) treeNoUnlift
        else
          unlifters.spawn(strippedTpe, rank).map {
            Apply(_, treeNoUnlift :: Nil)
          }.getOrElse {
            c.abort(pat.pos, s"Can't find $unliftableType[$strippedTpe], consider providing it")
          }
      }.getOrElse { treeNoUnlift }
  }

  /** Full support for unliftable implies that it's possible to interleave
   *  deconstruction with higher rank and unlifting of the values.
   *  In particular extraction of List[Tree] as List[T: Unliftable] requires
   *  helper extractors that would do the job: UnliftListElementwise[T]. Similarly
   *  List[List[Tree]] needs UnliftListOfListsElementwise[T].
   *
   *  See also "unlift list" tests in UnapplyProps.scala
   */
  object unlifters {
    private var records = List.empty[(Type, Rank)]
    // Materialize unlift helper that does elementwise
    // unlifting for corresponding rank and type.
    def spawn(tpe: Type, rank: Rank): Option[Tree] = {
      val unlifter = inferUnliftable(tpe)
      if (unlifter == EmptyTree) None
      else if (rank == NoDot) Some(unlifter)
      else {
        val idx = records.indexWhere { p => p._1 =:= tpe && p._2 == rank }
        val resIdx = if (idx != -1) idx else { records +:= ((tpe, rank)); records.length - 1}
        Some(Ident(TermName(nme.QUASIQUOTE_UNLIFT_HELPER + resIdx)))
      }
    }
    // Returns a list of vals that will defined required unlifters
    def preamble(): List[Tree] =
      records.zipWithIndex.map { case ((tpe, rank), idx) =>
        val name = TermName(nme.QUASIQUOTE_UNLIFT_HELPER + idx)
        val helperName = rank match {
          case DotDot    => nme.UnliftListElementwise
          case DotDotDot => nme.UnliftListOfListsElementwise
        }
        val lifter = inferUnliftable(tpe)
        assert(helperName.isTermName)
        // q"val $name: $u.internal.reificationSupport.${helperName.toTypeName} = $u.internal.reificationSupport.$helperName($lifter)"
        ValDef(NoMods, name,
          AppliedTypeTree(Select(Select(Select(u, nme.internal), nme.reificationSupport), helperName.toTypeName), List(TypeTree(tpe))),
          Apply(Select(Select(Select(u, nme.internal), nme.reificationSupport), helperName), lifter :: Nil))
      }
  }
}
