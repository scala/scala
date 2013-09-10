package scala.tools.reflect
package quasiquotes

import scala.reflect.macros.runtime.Context

abstract class Quasiquotes extends Parsers
                              with Holes
                              with Placeholders
                              with Reifiers {
  val c: Context
  val global: c.universe.type = c.universe
  import c.universe._

  def debug(msg: String): Unit =
    if (settings.Yquasiquotedebug.value) println(msg)

  lazy val (universe: Tree, args, parts, parse, reify) = c.macroApplication match {
    case Apply(Select(Select(Apply(Select(universe0, _), List(Apply(_, parts0))), interpolator0), method0), args0) =>
      val parts1 = parts0.map {
        case lit @ Literal(Constant(s: String)) => s -> lit.pos
        case part => c.abort(part.pos, "Quasiquotes can only be used with literal strings")
      }
      val reify0 = method0 match {
        case nme.apply   => new ApplyReifier().reifyFillingHoles(_)
        case nme.unapply => new UnapplyReifier().reifyFillingHoles(_)
        case other       => global.abort(s"Unknown quasiquote api method: $other")
      }
      val parse0 = interpolator0 match {
        case nme.q       => TermParser.parse(_)
        case nme.tq      => TypeParser.parse(_)
        case nme.cq      => CaseParser.parse(_)
        case nme.pq      => PatternParser.parse(_)
        case other       => global.abort(s"Unknown quasiquote flavor: $other")
      }
      (universe0, args0, parts1, parse0, reify0)
    case _ =>
      global.abort(s"Couldn't parse call prefix tree ${c.macroApplication}.")
  }

  lazy val u = universe // shortcut
  lazy val universeTypes = new definitions.UniverseDependentTypes(universe)

  def expandQuasiquote = {
    debug(s"\ncode to parse:\n$code\n")
    val tree = parse(code)
    debug(s"parsed:\n${showRaw(tree)}\n$tree\n")
    val reified = reify(tree)
    debug(s"reified tree:\n$reified\n")
    reified
  }
}
