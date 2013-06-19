package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import java.util.UUID.randomUUID
import scala.collection.immutable.ListMap

class Cardinality private[Cardinality](val value: Int) extends AnyVal {
  def pred = { assert(value - 1 >= 0); new Cardinality(value - 1) }
  def succ = new Cardinality(value + 1)
  override def toString = if (value == 0) "zero" else "." * (value + 1)
}

object Cardinality {
  val NoDot = new Cardinality(0)
  val DotDot = new Cardinality(1)
  val DotDotDot = new Cardinality(2)
  def stripDots(part: String) =
    if (part.endsWith("...")) (part.stripSuffix("..."), DotDotDot)
    else if (part.endsWith("..")) (part.stripSuffix(".."), DotDot)
    else (part, NoDot)
}

trait Macros { self: Quasiquotes =>
  import c.universe._
  import Cardinality._

  /** Generates scala code to be parsed by parser and holes map from incoming args and parts. */
  def generate(args: List[Tree], parts: List[String]): (String, Holes) = {
    val sb = new StringBuilder()
    var holes = ListMap[String, Hole]()

    foreach2(args, parts.init) { (tree, p) =>
      val (part, cardinality) = stripDots(p)
      val freshname = c.fresh(nme.QUASIQUOTE_PREFIX)
      sb.append(part)
      sb.append(freshname)
      holes += freshname -> Hole(tree, cardinality)
    }
    sb.append(parts.last)

    (sb.toString, Holes(holes))
  }

  /** Quasiquote macro expansion core logic. */
  def expand(universe: Tree, args: List[Tree], parts: List[String],
             parse: (String, Set[String]) => Tree, reifier: (Tree, Holes) => Reifier) = {
    val (code, holes) = generate(args, parts)
    debug(s"\ncode to parse=\n$code\n")
    val tree = parse(code, holes.keys.toSet)
    debug(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")
    val reified = reifier(universe, holes).reifyFillingHoles(tree)
    debug(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")
    reified
  }

  /** Disassamble macroApplication tree and configure the required flavor for expansion. */
  def dispatch = c.macroApplication match {
    case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), interpolator), method), args) =>
      val parts = parts0.map {
        case Literal(Constant(s: String)) => s
        case part => c.abort(part.pos, "Quasiquotes can only be used with constant string arguments.")
      }
      val reifier = method match {
        case nme.apply   => new ApplyReifier(_, _)
        case nme.unapply => new UnapplyReifier(_, _)
        case other       => global.abort(s"Unknown quasiquote api method: $other")
      }
      val parse = interpolator match {
        case nme.q       => TermParser.parse(_, _)
        case nme.tq      => TypeParser.parse(_, _)
        case nme.cq      => CaseParser.parse(_, _)
        case nme.pq      => PatternParser.parse(_, _)
        case other       => global.abort(s"Unknown quasiquote flavor: $other")
      }
      expand(universe, args, parts, parse, reifier)
    case _ =>
      global.abort(s"Couldn't parse call prefix tree ${c.macroApplication}.")
  }
}