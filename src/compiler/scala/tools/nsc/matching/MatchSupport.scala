/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import ast.{ TreePrinters, Trees }
import java.io.{ StringWriter, PrintWriter }

/** Ancillary bits of ParallelMatching which are better off
 *  out of the way.
 */
trait MatchSupport extends ast.TreeDSL
{
  self: ParallelMatching =>

  import global.{ typer => _, _ }
  import CODE._

  /** Debugging support: enable with -Ypmat-debug **/
  private final def trace = settings.Ypmatdebug.value

  def impossible:           Nothing = abort("this never happens")
  def abort(msg: String):   Nothing = Predef.error(msg)

  object Types {
    import definitions._
    implicit def enrichType(x: Type): RichType = new RichType(x)

    // see bug1434.scala for an illustration of why "x <:< y" is insufficient.
    // this code is definitely inadequate at best.  Inherited comment:
    //
    //   an approximation of _tp1 <:< tp2 that ignores _ types. this code is wrong,
    //   ideally there is a better way to do it, and ideally defined in Types.scala
    private[matching] def matches(arg1: Type, arg2: Type) = {
      val List(t1, t2) = List(arg1, arg2) map decodedEqualsType
      def eqSymbols = t1.typeSymbol eq t2.typeSymbol
      //  note: writing this as "t1.baseTypeSeq exists (_ =:= t2)" does not lead to 1434 passing.
      def isSubtype = t1.baseTypeSeq exists (_.typeSymbol eq t2.typeSymbol)

      (t1 <:< t2) || ((t1, t2) match {
        case (_: TypeRef, _: TypeRef) => !t1.isArray && (t1.prefix =:= t2.prefix) && (eqSymbols || isSubtype)
        case _ => false
      })
    }

    class RichType(undecodedTpe: Type) {
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
    def treeToString(t: Tree): String = unbind(t) match {
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
        case x: Tree        => treeToCompactString(x)
        case xs: List[_]    => pplist(xs map pp)
        case x: Tuple2[_,_] => "%s -> %s".format(pp(x._1), pp(x._2))
        case x              => x.toString
      })
    }

    object compactTreePrinter extends CompactTreePrinter

    // def treeChildrenString(t: Tree): String =
    //   nodeToString(t)

    def treeToCompactString(t: Tree): String = {
      val buffer = new StringWriter()
      val printer = compactTreePrinter.create(new PrintWriter(buffer))
      printer.print(t)
      printer.flush()
      buffer.toString
    }

    def ifDebug(body: => Unit): Unit          = { if (settings.debug.value) body }
    def DBG(msg: => String): Unit             = { ifDebug(println(msg)) }

    // @elidable(elidable.FINE)
    def TRACE(f: String, xs: Any*): Unit      = {
      if (trace) {
        val msg = if (xs.isEmpty) f else f.format(xs map pp: _*)
        println(msg)
      }
    }

    def tracing2[T](x: T)(category: String, xs: String*) = {
      val preamble = "[" + """%10s""".format(category) + "]  "
      if (xs.isEmpty) TRACE(preamble, x)
      else TRACE(preamble + xs.head, xs.tail: _*)

      x
    }

    def tracing[T](s: String, x: T): T = {
      val format = "[" + """%10s""".format(s) + "]  %s"
      TRACE(format, x)
      x
    }
    def traceCategory(cat: String, f: String, xs: Any*) = {
      val format = "[" + """%10s""".format(cat) + "]  " + f
      TRACE(format, xs: _*)
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

  /** A tree printer which is stingier about vertical whitespace and unnecessary
   *  punctuation than the standard one.
   */
  class CompactTreePrinter extends {
    val trees: global.type = global
  } with TreePrinters {
      import trees._

      override def create(writer: PrintWriter): TreePrinter = new TreePrinter(writer) {
        // drill down through Blocks and pull out the real statements.
        def allStatements(t: Tree): List[Tree] = t match {
          case Block(stmts, expr) => (stmts flatMap allStatements) ::: List(expr)
          case _                  => List(t)
        }

        def printLogicalOr(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
          printLogicalOp(t1, t2, "||")

        def printLogicalAnd(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
          printLogicalOp(t1, t2, "&&")

        def printLogicalOp(t1: (Tree, Boolean), t2: (Tree, Boolean), op: String) = {
          def maybenot(tvalue: Boolean) = if (tvalue) "" else "!"

          printRow(List(t1._1, t2._1),
            " %s(" format maybenot(t1._2),
            ") %s %s(".format(op, maybenot(t2._2)),
            ")"
          )
        }

        override def printRaw(tree: Tree): Unit = {
          // routing supercalls through this for debugging ease
          def s() = super.printRaw(tree)

          tree match {
            // labels used for jumps - does not map to valid scala code
            case LabelDef(name, params, rhs) =>
              print("labeldef %s(%s) = ".format(name, params mkString ","))
              printRaw(rhs)

            // target.method(arg) ==> target method arg
            case Apply(Select(target, method), List(arg)) =>
              (target, arg) match {
                case (_: Ident, _: Literal | _: Ident)  =>
                  printRaw(target)
                  print(" %s " format symName(tree, method))
                  printRaw(arg)
                case _                        => s()
              }

            // target.unary_! ==> !target
            case Select(qualifier, name) =>
              val n = symName(tree, name)
              if (n startsWith "unary_") {
                print(n drop 6)
                print(qualifier)
              }
              else s()

            // target.toString() ==> target.toString
            case Apply(fn, Nil)   => printRaw(fn)

            // if a Block only continues one actual statement, just print it.
            case Block(stats, expr) =>
              allStatements(tree) match {
                case List(x)            => printRow(List(x), "", ";", "")
                case _                  => s()
              }

            // We get a lot of this stuff
            case If( IsTrue(), x, _)        => printRaw(x)
            case If(IsFalse(), _, x)        => printRaw(x)

            case If(cond,  IsTrue(), elsep) =>
              printLogicalOr(cond -> true, elsep -> true)

            case If(cond, IsFalse(), elsep) =>
              printLogicalAnd(cond -> false, elsep -> true)

            case If(cond,  thenp, IsTrue()) =>
              printLogicalOr(cond -> false, thenp -> true)

            case If(cond,  thenp, IsFalse()) =>
              printLogicalAnd(cond -> true, thenp -> true)

            // If thenp or elsep has only one statement, it doesn't need more than one line.
            case If(cond, thenp, elsep) =>
              printRow(List(cond), "if (", "", ") ")

              def ifIndented(x: Tree) = {
                indent ; println ; printRaw(x) ; undent
              }

              indent ; println ;
              allStatements(thenp) match {
                case List(x: If)  => ifIndented(x)
                case List(x)      => printRaw(x)
                case _            => printRaw(thenp)
              }
              undent ; println ;
              val elseStmts = allStatements(elsep)
              if (!elseStmts.isEmpty) {
                print("else")
                indent ; println
                elseStmts match {
                  case List(x)      => printRaw(x)
                  case xs           => printRaw(elsep)
                }
                undent ; println
              }
            case _        => s()
          }
        }
      }
  }
}
