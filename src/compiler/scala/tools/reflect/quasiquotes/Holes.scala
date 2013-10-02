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

  /** Location characterizes a kind of a non-terminal in Scala syntax where something is going to be spliced.
   *  A location is typically associated with a type of the things that can be spliced there.
   *  Associated type might be different from an actual tpe of a splicee due to lifting.
   *  This is the first pillar of modularity in the quasiquote reifier.
   */
  sealed abstract class Location(val tpe: Type)
  case object UnknownLocation extends Location(NoType)
  case class TreeLocation(override val tpe: Type) extends Location(tpe)
  case object NameLocation extends Location(nameType)
  case object ModsLocation extends Location(modsType)
  case object FlagsLocation extends Location(flagsType)
  case object SymbolLocation extends Location(symbolType)
  case class IterableLocation(card: Cardinality, sublocation: TreeLocation) extends Location(NoType) {
    override val tpe = {
      def loop(n: Cardinality, tpe: Type): Type =
        if (n == NoDot) tpe
        else appliedType(IterableClass.toType, List(loop(n.pred, tpe)))
      loop(card, sublocation.tpe)
    }
  }

  /** Hole type describes location, cardinality and a pre-reification routine associated with a hole.
   *  An interesting thing about HoleType is that it can be completely inferred from the type of the splicee.
   *  This is the second pillar of modularity in the quasiquote reifier.
   */
  case class HoleType(preprocessor: Tree => Tree, location: Location, cardinality: Cardinality) {
    def makeHole(tree: Tree) = Hole(preprocessor(tree), location, cardinality)
  }
  object HoleType {
    def unapply(tpe: Type): Option[HoleType] = tpe match {
      case NativeType(holeTpe)           => Some(holeTpe)
      case LiftableType(holeTpe)         => Some(holeTpe)
      case IterableTreeType(holeTpe)     => Some(holeTpe)
      case IterableLiftableType(holeTpe) => Some(holeTpe)
      case _                             => None
    }

    trait HoleTypeExtractor {
      def unapply(tpe: Type): Option[HoleType] = {
        for {
          preprocessor <- this.preprocessor(tpe)
          location <- this.location(tpe)
          cardinality <- Some(this.cardinality(tpe))
        } yield HoleType(preprocessor, location, cardinality)
      }
      def preprocessor(tpe: Type): Option[Tree => Tree]
      def location(tpe: Type): Option[Location]
      def cardinality(tpe: Type): Cardinality = parseCardinality(tpe)._1

      def lifter(tpe: Type): Option[Tree => Tree] = {
        val lifterTpe = appliedType(LiftableClass.toType, List(tpe))
        val lifter = c.inferImplicitValue(lifterTpe, silent = true)
        if (lifter != EmptyTree) Some(tree => {
          val lifted = Apply(lifter, List(u, tree))
          val targetType = Select(u, tpnme.Tree)
          atPos(tree.pos)(TypeApply(Select(lifted, nme.asInstanceOf_), List(targetType)))
        }) else None
      }

      def iterator(tpe: Type)(elementTransform: Tree => Tree): Option[Tree => Tree] = {
        def reifyIterable(tree: Tree, n: Cardinality): Tree = {
          def loop(tree: Tree, n: Cardinality) =
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
        val card = parseCardinality(tpe)._1
        if (card != NoDot) Some(reifyIterable(_, card)) else None
      }
    }

    object NativeType extends HoleTypeExtractor {
      def preprocessor(tpe: Type) = Some(identity)
      def location(tpe: Type) = {
        if (tpe <:< treeType) Some(TreeLocation(tpe))
        else if (tpe <:< nameType) Some(NameLocation)
        else if (tpe <:< modsType) Some(ModsLocation)
        else if (tpe <:< flagsType) Some(FlagsLocation)
        else if (tpe <:< symbolType) Some(SymbolLocation)
        else None
      }
    }

    object LiftableType extends HoleTypeExtractor {
      def preprocessor(tpe: Type) = lifter(tpe)
      def location(tpe: Type) = Some(TreeLocation(treeType))
    }

    object IterableTreeType extends HoleTypeExtractor {
      def preprocessor(tpe: Type) = iterator(tpe)(identity)
      def location(tpe: Type) = {
        val (card, elementTpe) = parseCardinality(tpe)
        if (card != NoDot && elementTpe <:< treeType) Some(IterableLocation(card, TreeLocation(elementTpe)))
        else None
      }
    }

    object IterableLiftableType extends HoleTypeExtractor {
      def preprocessor(tpe: Type) = {
        val (_, elementTpe) = parseCardinality(tpe)
        for {
          lifter <- this.lifter(elementTpe)
          iterator <- this.iterator(tpe)(lifter)
        } yield iterator
      }
      def location(tpe: Type) = Some(IterableLocation(cardinality(tpe), TreeLocation(treeType)))
    }
  }

  /** Hole encapsulates information about splices in quasiquotes.
   *  It packs together a cardinality of a splice, a splicee (possibly preprocessed)
   *  and the description of the location in Scala syntax where the splicee can be spliced.
   *  This is the third pillar of modularity in the quasiquote reifier.
   */
  case class Hole(tree: Tree, location: Location, cardinality: Cardinality)

  object Hole {
    def apply(splicee: Tree, holeCard: Cardinality): Hole = {
      if (splicee.tpe == null) return new Hole(splicee, UnknownLocation, holeCard)
      val (spliceeCard, elementTpe) = parseCardinality(splicee.tpe)
      def cantSplice() = {
        val holeCardMsg = if (holeCard != NoDot) s" with $holeCard" else ""
        val action = "splice " + splicee.tpe + holeCardMsg
        val suggestCard = holeCard != spliceeCard || holeCard != NoDot
        val spliceeCardMsg = if (holeCard != spliceeCard && spliceeCard != NoDot) s"using $spliceeCard" else "omitting the dots"
        val cardSuggestion = if (suggestCard) spliceeCardMsg else ""
        def canBeLifted(tpe: Type) = HoleType.LiftableType.unapply(tpe).nonEmpty
        val suggestLifting = (holeCard == NoDot || spliceeCard != NoDot) && !(elementTpe <:< treeType) && !canBeLifted(elementTpe)
        val liftedTpe = if (holeCard != NoDot) elementTpe else splicee.tpe
        val liftSuggestion = if (suggestLifting) s"providing an implicit instance of Liftable[$liftedTpe]" else ""
        val advice = List(cardSuggestion, liftSuggestion).filter(_ != "").mkString(" or ")
        c.abort(splicee.pos, s"Can't $action, consider $advice")
      }
      val holeTpe = splicee.tpe match {
        case _ if holeCard != spliceeCard => cantSplice()
        case HoleType(holeTpe) => holeTpe
        case _ => cantSplice()
      }
      holeTpe.makeHole(splicee)
    }
  }

  def parseCardinality(tpe: Type): (Cardinality, Type) = {
    if (tpe != null && isIterableType(tpe)) {
      val (card, innerTpe) = parseCardinality(tpe.typeArguments.head)
      (card.succ, innerTpe)
    } else (NoDot, tpe)
  }
}