/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.reflect

trait FastStringInterpolator extends FormatInterpolator {
  import c.universe._

  def interpolateS: Tree = interpolated(c.macroApplication, false)
  def interpolateRaw: Tree = interpolated(c.macroApplication, true)

  // rewrite a tree like `scala.StringContext.apply("hello \\n ", " ", "").s("world", Test.this.foo)`
  // to `"hello \n world ".+(Test.this.foo)`
  private def interpolated(macroApp: Tree, isRaw: Boolean): Tree = macroApp match {
    case Apply(Select(Apply(stringCtx@Select(qualSC, _), parts), _interpol), args) if
      stringCtx.symbol == currentRun.runDefinitions.StringContext_apply &&
      treeInfo.isQualifierSafeToElide(qualSC) &&
      parts.forall(treeInfo.isLiteralString) &&
      parts.length == (args.length + 1) &&
      args.length <= 64 => // TODO make more robust to large input so that we can drop this condition, chunk the concatenations in manageable batches

      val treated =
        if (isRaw) parts
        else
          try
            parts.mapConserve { case lit@Literal(Constant(stringVal: String)) =>
              val k = Constant(StringContext.processEscapes(stringVal))
              // To avoid the backlash of backslash, taken literally by Literal, escapes are processed strictly (scala/bug#11196)
              treeCopy.Literal(lit, k).setType(ConstantType(k))
            }
          catch {
            case e: StringContext.InvalidEscapeException => c.abort(parts.head.pos.withShift(e.index), e.getMessage)
          }

      var result: Tree = treated.head

      def concat(t: Tree): Unit =
        result =
          atPos(t.pos)((result, t) match {
            case (_, Literal(Constant("")))                                   => result
            case (Literal(Constant(a: String)), Literal(Constant(b: String))) => Literal(Constant(a + b))
            case _                                                            =>
              gen.mkMethodCall(gen.mkAttributedSelect(result, definitions.String_+), t :: Nil)
          }).setType(definitions.StringTpe)

      val numLits = treated.length - 1
      foreachWithIndex(treated.tail) { (lit, i) =>
        if (i < numLits) {
          concat(args(i))
          concat(lit)
        } else concat(lit)
      }

      result

    // We know we're selecting either the s or raw method on our own StringContext (as far as we know, statically).
    // Once we turn them into fast tracked macros (and make the standardInterpolator public), we'll inline their
    // old implementation. Until we do, we can just expand to the original tree, but suppress macro expansion.
    // After the bootstrap is complete, the first branch of the if can be removed.
    case t@Apply(Select(someStringContext, _interpol), args) =>
      if (!t.symbol.isTermMacro) global.analyzer.suppressMacroExpansion(t)
      else q"""{
        val sc = $someStringContext
        _root_.scala.StringContext.standardInterpolator(
          ${if(isRaw) q"_root_.scala.Predef.identity" else q"_root_.scala.StringContext.processEscapes"},
          $args,
          sc.parts)
      }"""
  }
}
