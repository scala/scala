package scala.tools.reflect

import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import scala.collection.mutable

trait ApplyMacro { self: Quasiquotes =>
  import c.universe._

  def applyQ = apply(new QParser)

  def applyTq = apply(new TQParser)

  def apply(parser: QParser): Tree = {

    val (universe, args, parts) =
      c.macroApplication match {
        case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), args) =>
          val parts = parts0.map{
            case Literal(Constant(s: String)) => s
            case part => c.abort(part.pos, "Quasiquotes can only be used with constant string arguments.")
          }
          if (args.length != parts.length - 1)
            c.abort(c.enclosingPosition, "Imbalanced amount of arguments.")
          (universe, args, parts)
        case _ => c.abort(c.macroApplication.pos, "Couldn't parse call prefix tree ${c.macroApplication}.")
      }

    val (code, placeholders) = {
      val sb = new StringBuilder()
      val placeholders = mutable.Map[String, (Tree, String)]()

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

      (sb.toString, placeholders.toMap)
    }

    if (settings.Yquasiquotedebug.value) println(s"\ncode to parse=\n$code\n")

    val tree = parser.parse(code, placeholders.keys.toSet)

    if (settings.Yquasiquotedebug.value) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

    val reified = new ApplyReifier(universe, placeholders).reify(tree)

    if (settings.Yquasiquotedebug.value) println(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")

    val result =
      Block(
        List(ValDef(Modifiers(),
          nme.UNIVERSE_SHORT,
          SingletonTypeTree(universe),
          universe)),
        reified)

    if (settings.Yquasiquotedebug.value) println(s"result tree\n=${result}\n=${showRaw(result)}\n")

    result
  }
}