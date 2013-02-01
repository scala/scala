package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import java.util.UUID.randomUUID
import scala.collection.SortedMap

trait Macros { self: Quasiquotes =>
  import c.universe._

  val u = nme.UNIVERSE_SHORT

  /** This trait abstracts over all variations of quasiquotes
   *  and allows to share core logic between apply and unapply macros.
   */
  trait AbstractMacro {

    val parser: Parser

    /** Extracts universe tree, args trees and params strings from macroApplication. */
    def extract: (Tree, List[Tree], List[String])

    /** Reifier factory that abstracts over different reifiers need for apply and unapply macros. */
    def reifier(universe: Tree, placeholders: Placeholders): Reifier

    /** Wraps reified tree into a final result of macro expansion. */
    def wrap(universe: Tree, placeholders: Placeholders, reified: Tree): Tree

    /** Generates scala code to be parsed by parser and placeholders map from incoming args and parts. */
    def generate(args: List[Tree], parts: List[String]): (String, Placeholders) = {
      val sb = new StringBuilder()
      var placeholders = SortedMap[String, (Tree, String)]()

      foreach2(args, parts.init) { (tree, p) =>
        val (part, cardinality) =
          if (p.endsWith("..."))
            (p.stripSuffix("..."), "...")
          else if (p.endsWith(".."))
            (p.stripSuffix(".."), "..")
          else
            (p, "")
        val freshname = c.fresh(nme.QUASIQUOTE_PREFIX)
        sb.append(part)
        sb.append(freshname)
        placeholders += freshname -> (tree, cardinality)
      }
      sb.append(parts.last)

      (sb.toString, placeholders)
    }

    def debug(msg: String) =
      if (settings.Yquasiquotedebug.value) println(msg)

    /** Quasiquote macro expansion core logic. */
    def apply() = {
      val (universe, args, parts) = extract
      val (code, placeholders) = generate(args, parts)
      debug(s"\ncode to parse=\n$code\n")
      val tree = parser.parse(code, placeholders.keys.toSet)
      debug(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")
      val reified = reifier(universe, placeholders).reify(tree)
      debug(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")
      val result = wrap(universe, placeholders, reified)
      debug(s"result tree\n=${result}\n=${showRaw(result)}\n")
      result
    }
  }

  trait ApplyMacro extends AbstractMacro {

    def reifier(universe: Tree, placeholders: Placeholders): Reifier = new ApplyReifier(universe, placeholders)

    def extract = c.macroApplication match {
      case q"$universe.QuasiQuote($stringContext.apply(..$parts0)).${_}.apply(..$args)" =>
        val parts = parts0.map {
          case Literal(Constant(s: String)) => s
          case part => c.abort(part.pos, "Quasiquotes can only be used with constant string arguments.")
        }
        if (args.length != parts.length - 1)
          c.abort(c.enclosingPosition, "Imbalanced amount of arguments.")
        (universe, args, parts)
      case _ =>
        c.abort(c.macroApplication.pos, "Couldn't parse call prefix tree ${c.macroApplication}.")
    }

    def wrap(universe: Tree, placeholders: Placeholders, reified: Tree): Tree =
      q"""{
        val $u: $universe.type = $universe
        $reified
      }"""
  }

  trait UnapplyMacro extends AbstractMacro {

    def reifier(universe: Tree, placeholders: Placeholders): Reifier = new UnapplyReifier(universe, placeholders)

    def extract = c.macroApplication match {
      case q"$universe.QuasiQuote($stringContext.apply(..$parts0)).${_}.unapply(${_})" =>
        val parts = parts0.map{
          case Literal(Constant(s: String)) => s
          case part => c.abort(part.pos, "Quasiquotes can only be used with constant string arguments.")
        }
        if (!(parts.length >= 1 && parts.length <= 23))
          c.abort(c.enclosingPosition, "Inappropriate amount of quasiquote params.")
        // args are currently not exposed in macroApplication
        val args = List.fill(parts.length - 1)(EmptyTree)
        (universe, args, parts)
      case _ =>
        c.abort(c.macroApplication.pos, "Couldn't parse call prefix tree ${c.macroApplication}.")
    }

    def wrap(universe: Tree, placeholders: Placeholders, reified: Tree) = {

      val unapplyBody =
        if(isVariablePattern(reified))
          q"Some(tree)"
        else if (placeholders.size == 0)
          q"$reified.equalsStructure(tree)"
        else {
          val matchResult =
            if (placeholders.size == 1)
              q"Some(${TermName(placeholders.keys.head)})"
            else {
              val tupleN = TermName("Tuple" + placeholders.size.toString)
              val tupleArgs = placeholders.map(p => Ident(TermName(p._1)))
              q"Some($tupleN(..$tupleArgs))"
            }
          q"""{
            // importing type tags from universe for tree pattern matching to work as expected
            import $u._
            tree match {
              case $reified => $matchResult
              case _ => None
            }
          }"""
        }

      val moduleName = TermName(nme.QUASIQUOTE_MATCHER_NAME + randomUUID().toString.replace("-", ""))
      val modulePackage = nme.QUASIQUOTE_MATCHER_PACKAGE
      val modulePos = c.enclosingPosition.focus
      val moduleDef =
        q"""object $moduleName {
          def unapply($u: scala.reflect.api.Universe)(tree: $u.Tree) = $unapplyBody
        }"""

      debug(s"moduledef\n=${showRaw(moduleDef, printTypes=true, printIds=true)}\n=$moduleDef\n")

      c.introduceTopLevel(modulePackage, atPos(modulePos)(moduleDef))

      val unapplySelector = Ident(nme.SELECTOR_DUMMY)
      unapplySelector.setType(memberType(universe.tpe, tpnme.Tree))

      q"$modulePackage.$moduleName.unapply($universe)($unapplySelector)"
    }

    def isVariablePattern(tree: Tree) = tree match {
      case _: Bind => true
      case _ => false
    }
  }

  trait TermParsing { val parser = TermParser }
  trait TypeParsing { val parser = TypeParser }

  def applyQ = (new ApplyMacro with TermParsing).apply()
  def applyTq = (new ApplyMacro with TypeParsing).apply()
  def unapplyQ = (new UnapplyMacro with TermParsing).apply()
  def unapplyTq = (new UnapplyMacro with TypeParsing).apply()
}