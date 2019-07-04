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

  // fast track entry for StringContext.s
  def interpolateS: Tree = interpolated(c.macroApplication, false)
  // fast track entry for StringContext.raw
  def interpolateRaw: Tree = interpolated(c.macroApplication, true)

  // rewrite a tree like `scala.StringContext.apply("hello \\n ", " ", "").s("world", Test.this.foo)`
  // to `"hello \n world ".+(Test.this.foo)`
  private def interpolated(macroApp: Tree, isRaw: Boolean): Tree = macroApp match {
    case Apply(Select(Apply(stringCtx@Select(qualSC, _), parts), _interpol), args) if
      stringCtx.symbol == currentRun.runDefinitions.StringContext_apply &&
      treeInfo.isQualifierSafeToElide(qualSC) &&
      parts.forall(treeInfo.isLiteralString) &&
      parts.length == (args.length + 1) =>

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

      val argsIndexed = args.toVector
      var concatArgs = collection.mutable.ListBuffer[Tree]()
      val numLits = parts.length
      foreachWithIndex(treated.tail) { (lit, i) =>
        val treatedContents = lit.asInstanceOf[Literal].value.stringValue
        val emptyLit = treatedContents.isEmpty
        if (i < numLits - 1) {
          concatArgs += argsIndexed(i)
          if (!emptyLit) concatArgs += lit
        } else if (!emptyLit) {
          concatArgs += lit
        }
      }
      def mkConcat(pos: Position, lhs: Tree, rhs: Tree): Tree =
        atPos(pos)(gen.mkMethodCall(gen.mkAttributedSelect(lhs, definitions.String_+), rhs :: Nil)).setType(definitions.StringTpe)

      var result: Tree = treated.head
      val chunkSize = 32
      if (concatArgs.lengthCompare(chunkSize) <= 0) {
        concatArgs.foreach { t =>
          result = mkConcat(t.pos, result, t)
        }
      } else {
        concatArgs.toList.grouped(chunkSize).foreach {
          case group =>
            var chunkResult: Tree = group.head
            group.tail.foreach { t =>
              chunkResult = mkConcat(t.pos, chunkResult, t)
            }
            result = mkConcat(chunkResult.pos, result, chunkResult)
        }
      }

      result

    // Fallback -- inline the original implementation of the `s` or `raw` interpolator.
    case t@Apply(Select(someStringContext, _interpol), args) =>
      q"""{
        val sc = $someStringContext
        _root_.scala.StringContext.standardInterpolator(
          ${if(isRaw) q"_root_.scala.Predef.identity" else q"_root_.scala.StringContext.processEscapes"},
          $args,
          sc.parts)
      }"""
  }
}
