/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import ast.{ Printers, Trees }
import java.io.{ StringWriter, PrintWriter }
import annotation.elidable
import language.postfixOps

/** Ancillary bits of ParallelMatching which are better off
 *  out of the way.
 */
trait MatchSupport extends ast.TreeDSL { self: ParallelMatching =>

  import global.{ typer => _, _ }
  import CODE._

  /** Debugging support: enable with -Ypmat-debug **/
  private final def trace = settings.Ypmatdebug.value

  def impossible:           Nothing = abort("this never happens")

  def treeCollect[T](tree: Tree, pf: PartialFunction[Tree, T]): List[T] =
    tree filter (pf isDefinedAt _) map (x => pf(x))

  object Types {
    import definitions._

    val subrangeTypes = Set[Symbol](ByteClass, ShortClass, CharClass, IntClass)

    implicit class RichType(undecodedTpe: Type) {
      def tpe = decodedEqualsType(undecodedTpe)
      def isAnyRef = tpe <:< AnyRefClass.tpe

      // These tests for final classes can inspect the typeSymbol
      private def is(s: Symbol) = tpe.typeSymbol eq s
      def      isByte = is(ByteClass)
      def     isShort = is(ShortClass)
      def       isInt = is(IntClass)
      def      isChar = is(CharClass)
      def   isBoolean = is(BooleanClass)
      def   isNothing = is(NothingClass)
      def     isArray = is(ArrayClass)
    }
  }

  object Debug {
    def typeToString(t: Type): String = t match {
      case NoType => "x"
      case x      => x.toString
    }
    def symbolToString(s: Symbol): String = s match {
      case x  => x.toString
    }
    def treeToString(t: Tree): String = treeInfo.unbind(t) match {
      case EmptyTree            => "?"
      case WILD()               => "_"
      case Literal(Constant(x)) => "LIT(%s)".format(x)
      case Apply(fn, args)      => "%s(%s)".format(treeToString(fn), args map treeToString mkString ",")
      case Typed(expr, tpt)     => "%s: %s".format(treeToString(expr), treeToString(tpt))
      case x                    =>  x.toString + " (" + x.getClass + ")"
    }

    // Formatting for some error messages
    private val NPAD = 15
    def pad(s: String): String = "%%%ds" format (NPAD-1) format s
    def pad(s: Any): String = pad(s match {
      case x: Tree    => treeToString(x)
      case x          => x.toString
    })

    // pretty print for debugging
    def pp(x: Any): String = pp(x, false)
    def pp(x: Any, newlines: Boolean): String = {
      val stripStrings = List("""java\.lang\.""", """\$iw\.""")

      def clean(s: String): String =
        stripStrings.foldLeft(s)((s, x) => s.replaceAll(x, ""))

      def pplist(xs: List[Any]): String =
        if (newlines) (xs map ("    " + _ + "\n")).mkString("\n", "", "")
        else xs.mkString("(", ", ", ")")

      pp(x match {
        case s: String      => return clean(s)
        case x: Tree        => asCompactString(x)
        case xs: List[_]    => pplist(xs map pp)
        case x: Tuple2[_,_] => "%s -> %s".format(pp(x._1), pp(x._2))
        case x              => x.toString
      })
    }

    @elidable(elidable.FINE) def TRACE(f: String, xs: Any*): Unit = {
      if (trace) {
        val msg = if (xs.isEmpty) f else f.format(xs map pp: _*)
        println(msg)
      }
    }
    @elidable(elidable.FINE) def traceCategory(cat: String, f: String, xs: Any*) = {
      if (trace)
        TRACE("[" + """%10s""".format(cat) + "]  " + f, xs: _*)
    }
    def tracing[T](s: String)(x: T): T = {
      if (trace)
        println(("[" + """%10s""".format(s) + "]  %s") format pp(x))

      x
    }
    private[nsc] def printing[T](fmt: String, xs: Any*)(x: T): T = {
      println(fmt.format(xs: _*) + " == " + x)
      x
    }
    private[nsc] def debugging[T](fmt: String, xs: Any*)(x: T): T = {
      if (settings.debug.value) printing(fmt, xs: _*)(x)
      else x
    }

    def indent(s: Any) = s.toString() split "\n" map ("  " + _) mkString "\n"
    def indentAll(s: Seq[Any]) = s map ("  " + _.toString() + "\n") mkString
  }

  /** Drops the 'i'th element of a list.
   */
  def dropIndex[T](xs: List[T], n: Int) = {
    val (l1, l2) = xs splitAt n
    l1 ::: (l2 drop 1)
  }

  /** Extract the nth element of a list and return it and the remainder.
   */
  def extractIndex[T](xs: List[T], n: Int): (T, List[T]) =
    (xs(n), dropIndex(xs, n))
}
