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
  import definitions._

  // fast track entry for StringContext.s
  def interpolateS: Tree = interpolated(c.macroApplication, false)
  // fast track entry for StringContext.raw
  def interpolateRaw: Tree = interpolated(c.macroApplication, true)

  // rewrite a tree like `scala.StringContext.apply("hello \\n ", " ", "").s("world", Test.this.foo)`
  // to `"hello \n world ".+(Test.this.foo)`
  private def interpolated(macroApp: Tree, isRaw: Boolean): Tree =
    macroApp match {
      case Apply(Select(stringCtx@Apply(stringCtxApply@Select(qualSC, _), parts), interpol@_), args0) if
        stringCtxApply.symbol == currentRun.runDefinitions.StringContext_apply &&
        treeInfo.isQualifierSafeToElide(qualSC) &&
        parts.forall(treeInfo.isLiteralString) &&
        parts.length == (args0.length + 1) =>
        args0 match {
          case treeInfo.WildcardStarArg(expr) :: Nil =>
            if (expr.tpe <:< seqType(AnyTpe))
              expr match {
                case Apply(_, args1) => assembled(parts, args1, isRaw)
                case _ => fallback(stringCtx, Select(expr, "mkString") :: Nil, isRaw)
              }
            else {
              c.abort(expr.pos, "sequence arg is not a Seq")
              fallback(stringCtx, expr :: Nil, isRaw)
            }
          case _ => assembled(parts, args0, isRaw)
        }
      // Fallback -- inline the original implementation of the `s` or `raw` interpolator.
      case t@Apply(Select(someStringContext, interpol@_), args) => fallback(someStringContext, args, isRaw)
      case x => throw new MatchError(x)
    }
  private def fallback(stringContext: Tree, args: List[Tree], isRaw: Boolean): Tree =
    q"""{
      val sc = $stringContext
      _root_.scala.StringContext.standardInterpolator(
        ${if (isRaw) q"_root_.scala.Predef.identity" else q"_root_.scala.StringContext.processEscapes"},
        $args,
        sc.parts)
    }"""
  private def assembled(parts: List[Tree], args: List[Tree], isRaw: Boolean): Tree = {
    val treated =
      try
        parts.mapConserve {
          case lit @ Literal(Constant(stringVal: String)) =>
            val value =
              if (isRaw && currentRun.isScala3) stringVal
              else if (isRaw) {
                val processed = StringContext.processUnicode(stringVal)
                if (processed != stringVal) {
                  val diffindex = processed.zip(stringVal).zipWithIndex.collectFirst {
                    case ((p, o), i) if p != o => i
                  }.getOrElse(processed.length - 1)

                  runReporting.deprecationWarning(lit.pos.withShift(diffindex), "Unicode escapes in raw interpolations are deprecated. Use literal characters instead.", "2.13.2", "", "")
                }
                processed
              }
              else StringContext.processEscapes(stringVal)
            val k = Constant(value)
            // To avoid the backlash of backslash, taken literally by Literal, escapes are processed strictly (scala/bug#11196)
            treeCopy.Literal(lit, k).setType(ConstantType(k))
          case x => throw new MatchError(x)
        }
      catch {
        case ie: StringContext.InvalidEscapeException => c.abort(parts.head.pos.withShift(ie.index), ie.getMessage)
        case iue: StringContext.InvalidUnicodeEscapeException => c.abort(parts.head.pos.withShift(iue.index), iue.getMessage)
      }

    val argsIndexed = args.toVector
    val concatArgs = collection.mutable.ListBuffer.empty[Tree]
    val numLits = parts.length
    foreachWithIndex(treated.tail) { (lit, i) =>
      val treatedContents = lit.asInstanceOf[Literal].value.stringValue
      val emptyLit = treatedContents.isEmpty
      if (i < numLits - 2) {
        concatArgs += argsIndexed(i)
        if (!emptyLit) concatArgs += lit
      } else {
        if (i <= argsIndexed.length)
          argsIndexed.drop(i).foreach(concatArgs.+=)
        if (!emptyLit) concatArgs += lit
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
          var chunkResult: Tree = Literal(Constant("")).setType(definitions.StringTpe)
          group.foreach { t =>
            chunkResult = mkConcat(t.pos, chunkResult, t)
          }
          result = mkConcat(chunkResult.pos, result, chunkResult)
      }
    }

    result
  }
}
