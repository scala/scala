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

package scala.reflect
package quasiquotes

import scala.collection.mutable

/** Emulates hole support (see Holes.scala) in the quasiquote parser (see Parsers.scala).
 *  A principled solution to splicing into Scala syntax would be a parser that natively supports holes.
 *  Unfortunately, that's outside of our reach in Scala 2.11, so we have to emulate.
 *  This trait stores knowledge of how to represent the holes as something understandable by the parser
 *  and how to recover holes from the results of parsing the produced representation.
 */
trait Placeholders { self: Quasiquotes =>
  import global._
  import Rank._
  import universeTypes._

  // Step 1: Transform Scala source with holes into vanilla Scala source

  lazy val posMap = mutable.LinkedHashMap[Position, (Int, Int)]()
  lazy val code = {
    val sb = new StringBuilder()

    def appendPart(value: String, pos: Position): Unit = {
      val start = sb.length
      sb.append(value)
      val end = sb.length
      posMap += pos -> ((start, end))
    }

    def appendHole(tree: Tree, rank: Rank): Unit = {
      val placeholderName = c.freshName(TermName(nme.QUASIQUOTE_PREFIX))
      sb.append(placeholderName)
      val holeTree =
        if (method != nme.unapply) tree
        else Bind(placeholderName, tree)
      holeMap(placeholderName) = Hole(rank, holeTree)
    }

    val iargs = method match {
      case nme.apply   => args
      case nme.unapply => internal.subpatterns(args.head).get
      case _           => global.abort("unreachable")
    }

    foreach2(iargs, parts.init) { case (tree, (p, pos)) =>
      val (part, rank) = parseDots(p)
      appendPart(part, pos)
      appendHole(tree, rank)
    }
    val (p, pos) = parts.last
    appendPart(p, pos)

    sb.toString
  }

  object holeMap {
    private val underlying = mutable.LinkedHashMap.empty[String, Hole]
    private val accessed   = mutable.Set.empty[String]
    def unused: Set[Name] = (underlying.keys.toSet diff accessed).map(TermName(_))
    def contains(key: Name): Boolean = underlying.contains(key.toString)
    def apply(key: Name): Hole = {
      val skey = key.toString
      val value = underlying(skey)
      accessed += skey
      value
    }
    def update(key: Name, hole: Hole): Unit =
      underlying += key.toString -> hole
    def get(key: Name): Option[Hole] = {
      val skey = key.toString
      val res = underlying.get(skey)
      res.foreach(_ => accessed += skey)
      res
    }
    def keysIterator: Iterator[TermName] = underlying.keysIterator.map(TermName(_))
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
    }
  }

  object ModsPlaceholder extends HolePlaceholder {
    def apply(name: Name) =
      Apply(Select(New(Ident(tpnme.QUASIQUOTE_MODS)), nme.CONSTRUCTOR), List(Literal(Constant(name.toString))))
    def matching = {
      case Apply(Select(New(Ident(tpnme.QUASIQUOTE_MODS)), nme.CONSTRUCTOR), List(Literal(Constant(s: String)))) => TermName(s)
    }
  }

  object AnnotPlaceholder extends HolePlaceholder {
    def matching = {
      case Apply(Select(New(Ident(name)), nme.CONSTRUCTOR), Nil) => name
    }
  }

  object ParamPlaceholder extends HolePlaceholder {
    def apply(flags: FlagSet, name: Name) =
      ValDef(Modifiers(flags), nme.QUASIQUOTE_PARAM, Ident(name), EmptyTree)
    def matching = {
      case ValDef(_, nme.QUASIQUOTE_PARAM, Ident(name), EmptyTree) => name
    }
  }

  object TuplePlaceholder {
    def apply(args: List[Tree]) =
      Apply(Ident(nme.QUASIQUOTE_TUPLE), args)
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case Apply(Ident(nme.QUASIQUOTE_TUPLE), args) => Some(args)
      case _ => None
    }
  }

  object TupleTypePlaceholder {
    def apply(args: List[Tree]) =
      AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), args)
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), args) => Some(args)
      case _ => None
    }
  }

  object FunctionTypePlaceholder {
    def apply(args: List[Tree], res: Tree) =
      AppliedTypeTree(Ident(tpnme.QUASIQUOTE_FUNCTION), args :+ res)
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
    def apply(name: Name) =
      CaseDef(Apply(Ident(nme.QUASIQUOTE_CASE), Ident(name) :: Nil), EmptyTree, EmptyTree)
    def unapply(tree: Tree): Option[Hole] = tree match {
      case CaseDef(Apply(Ident(nme.QUASIQUOTE_CASE), List(Placeholder(hole))), EmptyTree, EmptyTree) => Some(hole)
      case _ => None
    }
  }

  object RefineStatPlaceholder {
    def apply(name: Name) =
      ValDef(NoMods, nme.QUASIQUOTE_REFINE_STAT, Ident(name), EmptyTree)
    def unapply(tree: Tree): Option[Hole] = tree match {
      case ValDef(_, nme.QUASIQUOTE_REFINE_STAT, Ident(Placeholder(hole)), _) => Some(hole)
      case _ => None
    }
  }

  object EarlyDefPlaceholder {
    def apply(name: Name) =
      ValDef(Modifiers(Flag.PRESUPER), nme.QUASIQUOTE_EARLY_DEF, Ident(name), EmptyTree)
    def unapply(tree: Tree): Option[Hole] = tree match {
      case ValDef(_, nme.QUASIQUOTE_EARLY_DEF, Ident(Placeholder(hole)), _) => Some(hole)
      case _ => None
    }
  }

  object PackageStatPlaceholder {
    def apply(name: Name) =
      ValDef(NoMods, nme.QUASIQUOTE_PACKAGE_STAT, Ident(name), EmptyTree)
    def unapply(tree: Tree): Option[Hole] = tree match {
      case ValDef(NoMods, nme.QUASIQUOTE_PACKAGE_STAT, Ident(Placeholder(hole)), EmptyTree) => Some(hole)
      case _ => None
    }
  }

  object ForEnumPlaceholder {
    def apply(name: Name) =
      build.SyntacticValFrom(Bind(name, Ident(nme.WILDCARD)), Ident(nme.QUASIQUOTE_FOR_ENUM))
    def unapply(tree: Tree): Option[Hole] = tree match {
      case build.SyntacticValFrom(Bind(Placeholder(hole), Ident(nme.WILDCARD)), Ident(nme.QUASIQUOTE_FOR_ENUM)) =>
        Some(hole)
      case _ => None
    }
  }
}
