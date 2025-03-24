/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package ast.parser

import javac._

/** The compiler sub-component that defines the parser phase, which converts source code into Trees.
 */
abstract class SyntaxAnalyzer extends SubComponent with Parsers with MarkupParsers with Scanners with JavaParsers with JavaScanners {
  import global._

  val phaseName = "parser"
  def newPhase(prev: Phase): StdPhase = new ParserPhase(prev)

  abstract class MemberDefTraverser extends Traverser {
    def onMember(defn: MemberDef): Unit

    private var depth: Int = 0
    private def lower[T](body: => T): T = {
      depth += 1
      try body finally depth -= 1
    }
    def currentDepth = depth

    /** Prune this tree and all trees beneath it. Can be overridden. */
    def prune(md: MemberDef): Boolean = (
         md.mods.isSynthetic
      || md.mods.isParamAccessor
      || nme.isConstructorName(md.name)
      || md.name.containsName(nme.ANON_CLASS_NAME)
    )

    override def traverse(t: Tree): Unit = t match {
      case md: MemberDef if prune(md) =>
      case PackageDef(_, stats)       => traverseTrees(stats)
      case md: ImplDef                => onMember(md) ; lower(traverseTrees(md.impl.body))
      case md: ValOrDefDef            => onMember(md) ; lower(traverse(md.rhs))
      case _                          => super.traverse(t)
    }
  }

  class MemberPosReporter(unit: CompilationUnit) extends MemberDefTraverser {
    private var outputFn: MemberDef => String = outputForScreen
    val path = unit.source.file.path

    // If a single line, outputs the line; if it spans multiple lines
    // outputs NN,NN with start and end lines, e.g. 15,25.
    def outputPos(md: MemberDef): String = {
      val pos   = md.pos
      val start = pos.focusStart.line
      val end   = pos.focusEnd.line

      if (start == end) "" + start else s"$start,$end"
    }
    def outputForSed(md: MemberDef): String = {
      val pos_s = "%-12s" format outputPos(md) + "p"
      s"$pos_s $path    # ${md.keyword} ${md.name}"
    }
    def outputForScreen(md: MemberDef): String = {
      val pos_s = "%-20s" format " " * currentDepth + outputPos(md)
      s"$pos_s ${md.keyword} ${md.name}"
    }

    def onMember(md: MemberDef) = println(outputFn(md))
    // It recognizes "sed" and "anything else".
    def show(style: String): Unit = {
      if (style == "sed") {
        outputFn = outputForSed
        traverse(unit.body)
      }
      else {
        outputFn = outputForScreen
        println(path)
        traverse(unit.body)
      }
      println("")
    }
  }

  private def initialUnitBody(unit: CompilationUnit): Tree = {
    if (unit.isJava) newJavaUnitParser(unit).parse()
    else if (currentRun.parsing.incompleteHandled) newUnitParser(unit).parse()
    else newUnitParser(unit).smartParse()
  }

  class ParserPhase(prev: Phase) extends StdPhase(prev) {
    override val checkable = false
    override val keepsTypeParams = false

    def apply(unit: CompilationUnit): Unit = {
      informProgress("parsing " + unit)
      // if the body is already filled in, don't overwrite it
      // otherwise compileLate is going to overwrite bodies of synthetic source files
      if (unit.body == EmptyTree)
        unit.body = initialUnitBody(unit)

      if (settings.Ymemberpos.isSetByUser)
        new MemberPosReporter(unit).show(style = settings.Ymemberpos.value)
    }
  }
}
