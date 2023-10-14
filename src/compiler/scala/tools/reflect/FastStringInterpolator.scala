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

package scala.tools
package reflect

import nsc.Reporting.WarningCategory

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
        try
          parts.mapConserve {
            case lit @ Literal(Constant(stringVal: String)) =>
              def asRaw = {
                val processed = StringContext.processUnicode(stringVal)
                if (processed != stringVal) {
                  val pos = {
                    val diffindex = processed.zip(stringVal).zipWithIndex.collectFirst {
                      case ((p, o), i) if p != o => i
                    }.getOrElse(processed.length - 1)
                    lit.pos.withShift(diffindex)
                  }
                  def msg(fate: String) = s"Unicode escapes in raw interpolations are $fate; use literal characters instead"
                  if (currentRun.isScala3) {
                    runReporting.warning(pos, msg("ignored under -Xsource:3"), WarningCategory.Scala3Migration, c.internal.enclosingOwner)
                    stringVal
                  }
                  else {
                    runReporting.deprecationWarning(pos, msg("deprecated"), "2.13.2", "", "")
                    processed
                  }
                }
                else stringVal
              }
              val value =
                if (isRaw) asRaw
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

      if (args.forall(treeInfo.isLiteralString)) {
        val it1 = treated.iterator
        val it2 = args.iterator
        val res = new StringBuilder
        def add(t: Tree): Unit = res.append(t.asInstanceOf[Literal].value.value)
        add(it1.next())
        while (it2.hasNext) {
          add(it2.next())
          add(it1.next())
        }
        val k = Constant(res.toString)
        Literal(k).setType(ConstantType(k))
      }
      else concatenate(treated, args)

    // Fallback -- inline the original implementation of the `s` or `raw` interpolator.
    case t@Apply(Select(someStringContext, _interpol), args) =>
      q"""{
        val sc = $someStringContext
        _root_.scala.StringContext.standardInterpolator(
          ${if(isRaw) q"_root_.scala.Predef.identity" else q"_root_.scala.StringContext.processEscapes"},
          $args,
          sc.parts)
      }"""
    case x => throw new MatchError(x)
  }

  def concatenate(parts: List[Tree], args: List[Tree]): Tree = {
    val argsIndexed = args.toVector
    val concatArgs = collection.mutable.ListBuffer[Tree]()
    val numLits = parts.length
    foreachWithIndex(parts.tail) { (lit, i) =>
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

    var result: Tree = parts.head
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
