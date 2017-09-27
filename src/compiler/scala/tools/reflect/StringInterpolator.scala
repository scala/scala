package scala.tools.reflect

import scala.annotation.tailrec
import scala.reflect.macros.runtime.Context

abstract class StringInterpolator {
  val c: Context
  val global: c.universe.type = c.universe

  import c.universe.{Match => _, _}
  import definitions._
  import treeInfo.Applied

  @inline private def truly(body: => Unit): Boolean = {
    body
    true
  }

  @inline private def falsely(body: => Unit): Boolean = {
    body
    false
  }

  private def bail(msg: String) = global.abort(msg)

  def interpolateString: Tree = interpolateImpl(false)

  def interpolateRaw: Tree = interpolateImpl(true)

  def interpolateImpl(raw: Boolean): Tree = c.macroApplication match {
    case Applied(Select(Apply(_, parts), _), _, argss) =>
      val args = argss.flatten

      def badlyInvoked = (parts.length != args.length + 1) && truly {
        //probably cant use s"..." here as this is the macro for s
        def because(s: String) = "too " + s + " arguments for interpolated string"

        val (p, msg) =
          if (args.length + 1 < parts.length)
            (if (args.isEmpty) c.enclosingPosition else args.last.pos, because("few"))
          else (args(parts.length - 1).pos, because("many"))
        c.abort(p, msg)
      }

      if (badlyInvoked) c.macroApplication else {
        val res = interpolated(raw, parts, args)
        /*if (settings.debug) */reporter.echo(c.enclosingPosition, "rewrite expression to "+res)
        res
      }
    case other =>
      bail("Unexpected application " + showRaw(other))
      other
  }

  def interpolated(raw: Boolean, parts: List[Tree], args: List[Tree]): Tree = {
    val literals: List[List[Literal]] = parts map {
      case lit@Literal(Constant(x: String)) =>
        if (x.isEmpty) Nil
        else {
          if (raw) lit :: Nil
          else (Literal(Constant(/*StringContext.*/ treatEscapes(x))) setPos (lit.pos)) :: Nil
        }
      case _ => throw new IllegalArgumentException("internal error: argument parts must be a list of string literals")
    }
    val argsIt = args.iterator
    val literalsIt = literals.iterator
    val plus = TermName("$plus")

    // Note s"$foo" will produce
    // "" + foo
    // but the back end code writer will eliminate "" and concatenate with StringBuilder as appropriate

    var expr = if (literals(0).isEmpty) parts(0)
    else Literal(Constant("")) setPos c.macroApplication.pos

    literalsIt.next()

    while (literalsIt.hasNext) {
      val literal = literalsIt.next()
      val arg = argsIt.next()
      expr = Apply(Select(expr, plus), arg :: Nil) setPos (arg.pos)
      if (!literal.isEmpty) expr = Apply(Select(expr, plus), literal) setPos (literal.head.pos)
    }
    expr
  }

  def treatEscapes(str: String): String = treatEscapes0(str, strict = false)

  /** Treats escapes, but disallows octal escape sequences. */
  def processEscapes(str: String): String = treatEscapes0(str, strict = true)

  private def treatEscapes0(str: String, strict: Boolean): String = {
    val len = str.length

    // replace escapes with given first escape
    def replace(first: Int): String = {
      val b = new java.lang.StringBuilder()

      // append replacement starting at index `i`, with `next` backslash
      @tailrec def loop(i: Int, next: Int): String = {
        if (next >= 0) {
          //require(str(next) == '\\')
          if (next > i) b.append(str, i, next)
          var idx = next + 1
          if (idx >= len) throw new InvalidEscapeException(str, next)
          val c = str(idx) match {
            case 'b' => '\b'
            case 't' => '\t'
            case 'n' => '\n'
            case 'f' => '\f'
            case 'r' => '\r'
            case '"' => '"'
            case '\'' => '\''
            case '\\' => '\\'
            case o if '0' <= o && o <= '7' =>
              if (strict) throw new InvalidEscapeException(str, next)
              val leadch = str(idx)
              var oct = leadch - '0'
              idx += 1
              if (idx < len && '0' <= str(idx) && str(idx) <= '7') {
                oct = oct * 8 + str(idx) - '0'
                idx += 1
                if (idx < len && leadch <= '3' && '0' <= str(idx) && str(idx) <= '7') {
                  oct = oct * 8 + str(idx) - '0'
                  idx += 1
                }
              }
              idx -= 1 // retreat
              oct.toChar
            case _ => throw new InvalidEscapeException(str, next)
          }
          idx += 1 // advance
          b append c
          loop(idx, str.indexOf('\\', idx))
        } else {
          if (i < len) b.append(str, i, len)
          b.toString
        }
      }

      loop(0, first)
    }

    str indexOf '\\' match {
      case -1 => str
      case i => replace(i)
    }
  }

  class InvalidEscapeException(str: String, val index: Int) extends IllegalArgumentException(
    "invalid escape"
  )

}
