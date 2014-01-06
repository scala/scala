package scala.tools.reflect
package quasiquotes

import java.util.UUID.randomUUID
import scala.collection.{immutable, mutable}

/** Emulates hole support (see Holes.scala) in the quasiquote parser (see Parsers.scala).
 *  A principled solution to splicing into Scala syntax would be a parser that natively supports holes.
 *  Unfortunately, that's outside of our reach in Scala 2.11, so we have to emulate.
 *  This trait stores knowledge of how to represent the holes as something understandable by the parser
 *  and how to recover holes from the results of parsing the produced representation.
 */
trait Placeholders { self: Quasiquotes =>
  import global._
  import Cardinality._
  import universeTypes._

  // Step 1: Transform Scala source with holes into vanilla Scala source

  lazy val holeMap = new HoleMap()
  lazy val posMap = mutable.ListMap[Position, (Int, Int)]()
  lazy val code = {
    val sb = new StringBuilder()
    val sessionSuffix = randomUUID().toString.replace("-", "").substring(0, 8) + "$"

    def appendPart(value: String, pos: Position) = {
      val start = sb.length
      sb.append(value)
      val end = sb.length
      posMap += pos -> ((start, end))
    }

    def appendHole(tree: Tree, cardinality: Cardinality) = {
      val placeholderName = c.freshName(TermName(nme.QUASIQUOTE_PREFIX + sessionSuffix))
      sb.append(placeholderName)
      val holeTree =
        if (method != nme.unapply) tree
        else Bind(placeholderName, tree)
      holeMap(placeholderName) = Hole(cardinality, holeTree)
    }

    val iargs = method match {
      case nme.apply   => args
      case nme.unapply =>
        val (dummy @ Ident(nme.SELECTOR_DUMMY)) :: Nil = args
        dummy.attachments.get[SubpatternsAttachment].get.patterns
      case _           => global.abort("unreachable")
    }

    foreach2(iargs, parts.init) { case (tree, (p, pos)) =>
      val (part, cardinality) = parseDots(p)
      appendPart(part, pos)
      appendHole(tree, cardinality)
    }
    val (p, pos) = parts.last
    appendPart(p, pos)

    sb.toString
  }

  class HoleMap {
    private var underlying = immutable.SortedMap[String, Hole]()
    private val accessed = mutable.Set[String]()
    def unused: Set[Name] = (underlying.keys.toSet -- accessed).map(TermName(_))
    def contains(key: Name) = underlying.contains(key.toString)
    def apply(key: Name) = {
      val s = key.toString
      accessed += s
      underlying(s)
    }
    def update(key: Name, hole: Hole) = {
      underlying += key.toString -> hole
    }
    def get(key: Name) = {
      val s = key.toString
      accessed += s
      underlying.get(s)
    }
    def toList = underlying.toList
  }

  // Step 2: Transform vanilla Scala AST into an AST with holes

  trait HolePlaceholder {
    def matching: PartialFunction[Any, Name]
    def unapply(scrutinee: Any): Option[Hole] = {
      val name = matching.lift(scrutinee)
      name.flatMap { holeMap.get(_) }
    }
  }

  object Placeholder extends HolePlaceholder {
    def matching = {
      case name: Name => name
      case Ident(name) => name
      case Bind(name, Ident(nme.WILDCARD)) => name
      case TypeDef(_, name, List(), TypeBoundsTree(EmptyTree, EmptyTree)) => name
      case ValDef(_, name, TypeTree(), EmptyTree) => name
    }
  }

  object ModsPlaceholder extends HolePlaceholder {
    def matching = {
      case Apply(Select(New(Ident(tpnme.QUASIQUOTE_MODS)), nme.CONSTRUCTOR), List(Literal(Constant(s: String)))) => TermName(s)
    }
  }

  object AnnotPlaceholder extends HolePlaceholder {
    def matching = {
      case Apply(Select(New(Ident(name)), nme.CONSTRUCTOR), Nil) => name
    }
  }

  object TuplePlaceholder {
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case Apply(Ident(nme.QUASIQUOTE_TUPLE), args) => Some(args)
      case _ => None
    }
  }

  object TupleTypePlaceholder {
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), args) => Some(args)
      case _ => None
    }
  }

  object FunctionTypePlaceholder {
    def unapply(tree: Tree): Option[(List[Tree], Tree)] = tree match {
      case AppliedTypeTree(Ident(tpnme.QUASIQUOTE_FUNCTION), args :+ res) => Some((args, res))
      case _ => None
    }
  }

  object SymbolPlaceholder {
    def unapply(scrutinee: Any): Option[Hole] = scrutinee match {
      case Placeholder(hole: ApplyHole) if hole.tpe <:< symbolType => Some(hole)
      case _ => None
    }
  }

  object CasePlaceholder {
    def unapply(tree: Tree): Option[Hole] = tree match {
      case CaseDef(Apply(Ident(nme.QUASIQUOTE_CASE), List(Placeholder(hole))), EmptyTree, EmptyTree) => Some(hole)
      case _ => None
    }
  }

  object RefineStatPlaceholder {
    def unapply(tree: Tree): Option[Hole] = tree match {
      case ValDef(_, Placeholder(hole), Ident(tpnme.QUASIQUOTE_REFINE_STAT), _) => Some(hole)
      case _ => None
    }
  }

  object EarlyDefPlaceholder {
    def unapply(tree: Tree): Option[Hole] = tree match {
      case ValDef(_, Placeholder(hole), Ident(tpnme.QUASIQUOTE_EARLY_DEF), _) => Some(hole)
      case _ => None
    }
  }

  object PackageStatPlaceholder {
    def unapply(tree: Tree): Option[Hole] = tree match {
      case ValDef(NoMods, Placeholder(hole), Ident(tpnme.QUASIQUOTE_PACKAGE_STAT), EmptyTree) => Some(hole)
      case _ => None
    }
  }

  object ForEnumPlaceholder {
    def unapply(tree: Tree): Option[Hole] = tree match {
      case build.SyntacticValFrom(Bind(Placeholder(hole), Ident(nme.WILDCARD)), Ident(nme.QUASIQUOTE_FOR_ENUM)) =>
        Some(hole)
      case _ => None
    }
  }
}