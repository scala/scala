/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter

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
      case x: TypeTree          => "TT(%s)".format(symbolToString(x.symbol))
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
      def clean(s: String): String = s.replaceAll("""java\.lang\.""", "")

      val elems: List[Any] = x match {
        case x: String      => return clean(x)
        case xs: List[_]    => xs
        case x: Tuple2[_,_] => return pp(x._1) + " -> " + pp(x._2)
        case x              => return pp(x.toString)
      }
      def pplist(xs: List[Any]): String = {
        val xs2 = xs map pp

        if (newlines) (xs2 map ("    " + _ + "\n")).mkString("\n", "", "")
        else xs2.mkString("(", ", ", ")")
      }

      pplist(elems)
    }

    def ifDebug(body: => Unit): Unit          = { if (settings.debug.value) body }
    def DBG(msg: => String): Unit             = { ifDebug(println(msg)) }

    // @elidable(elidable.FINE)
    def TRACE(f: String, xs: Any*): Unit      = { if (trace) println(pp(if (xs.isEmpty) f else f.format(xs : _*))) }

    def tracing2[T](x: T)(category: String, xs: String*) = {
      val preamble = "[" + """%10s""".format(category) + "]  "
      if (xs.isEmpty) TRACE(preamble, x)
      else TRACE(preamble + xs.head, xs.tail: _*)

      x
    }

    def tracing[T](s: String, x: T): T = {
      TRACE("[" + """%10s""".format(s) + "]  " + x.toString)
      x
    }
    def traceCategory(cat: String, f: String, xs: Any*) =
      TRACE("[" + """%10s""".format(cat) + "]  " + f, xs: _*)

    def indent(s: Any) = s.toString() split "\n" map ("  " + _) mkString "\n"
    def indentAll(s: Seq[Any]) = s map ("  " + _.toString() + "\n") mkString
  }

  /** Transforms a list of triples into a triple of lists.
   *
   *  @param xs the list of triples to unzip
   *  @return a triple of lists.
   */
  def unzip3[A,B,C](xs: List[(A,B,C)]): (List[A], List[B], List[C]) = {
    import collection.mutable.ListBuffer

    val b1 = new ListBuffer[A]
    val b2 = new ListBuffer[B]
    val b3 = new ListBuffer[C]
    var xc = xs
    while (!xc.isEmpty) {
      b1 += xc.head._1
      b2 += xc.head._2
      b3 += xc.head._3
      xc = xc.tail
    }
    (b1.toList, b2.toList, b3.toList)
  }

  /** Drops the 'i'th element of a list.
   */
  def dropIndex[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop (n + 1))
}
