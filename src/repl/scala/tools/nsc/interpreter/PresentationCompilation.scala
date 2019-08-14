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

package scala.tools.nsc.interpreter

import scala.reflect.internal.util.{Position, RangePosition}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.{Settings, interactive}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.classpath._

trait PresentationCompilation {
  self: IMain =>

  /** Typecheck a line of REPL input, suitably wrapped with "interpreter wrapper" objects/classes, with the
    * presentation compiler. The result of this method gives access to the typechecked tree and to autocompletion
    * suggestions.
    *
    * The caller is responsible for calling [[PresentationCompileResult#cleanup]] to dispose of the compiler instance.
    */
  private[scala] def presentationCompile(line: String): Either[IR.Result, PresentationCompileResult] = {
    if (global == null) Left(IR.Error)
    else {
      // special case for:
      //
      // scala> 1
      // scala> .toInt
      //
      // and for multi-line input.
      val line1 = partialInput + (if (Completion.looksLikeInvocation(line)) { self.mostRecentVar + line } else line)
      val compiler = newPresentationCompiler()
      val trees = compiler.newUnitParser(line1).parseStats()
      val importer = global.mkImporter(compiler)
      val request = new Request(line1, trees map (t => importer.importTree(t)), generousImports = true)
      val wrappedCode: String = request.ObjectSourceCode(request.handlers)
      val unit = compiler.newCompilationUnit(wrappedCode)
      import compiler._
      val richUnit = new RichCompilationUnit(unit.source)
      // disable brace patching in the parser, the snippet template isn't well-indented and the results can be surprising
      currentRun.parsing.withIncompleteHandler((pos, msg) => ()) {
        unitOfFile(richUnit.source.file) = richUnit
        enteringTyper(typeCheck(richUnit))
      }

      val result = PresentationCompileResult(compiler)(richUnit, request.ObjectSourceCode.preambleLength + line1.length - line.length)
      Right(result)
    }
  }

  /** Create an instance of the presentation compiler with a classpath comprising the REPL's configured classpath
    * and the classes output by previously compiled REPL lines.
    *
    * You may directly interact with this compiler from any thread, although you must not access it concurrently
    * from multiple threads.
    *
    * You may downcast the `reporter` to `StoreReporter` to access type errors.
    */
  def newPresentationCompiler(): interactive.Global = {
    def copySettings: Settings = {
      val s = new Settings(_ => () /* ignores "bad option -nc" errors, etc */)
      s.processArguments(global.settings.recreateArgs, processAll = false)
      s.YpresentationAnyThread.value = true
      s
    }
    val storeReporter: StoreReporter = new StoreReporter(copySettings)
    val interactiveGlobal = new interactive.Global(copySettings, storeReporter) { self =>
      def mergedFlatClasspath = {
        val replOutClasspath = ClassPathFactory.newClassPath(replOutput.dir, settings, closeableRegistry)
        AggregateClassPath(replOutClasspath :: global.platform.classPath :: Nil)
      }

      override lazy val platform: ThisPlatform = {
        new JavaPlatform {
          lazy val global: self.type = self
          override lazy val classPath: ClassPath = mergedFlatClasspath
        }
      }
    }
    new interactiveGlobal.TyperRun()
    interactiveGlobal
  }

  abstract class PresentationCompileResult {
    val compiler: scala.tools.nsc.interactive.Global
    def unit: compiler.RichCompilationUnit
    /** The length of synthetic code the precedes the user written code */
    def preambleLength: Int
    def cleanup(): Unit = {
      compiler.askShutdown()
    }
    import compiler.CompletionResult

    def completionsAt(cursor: Int): CompletionResult = {
      compiler.completionsAt(positionOf(cursor))
    }

    def positionOf(cursor: Int): Position =
      unit.source.position(preambleLength + cursor)

    def typedTreeAt(selectionStart: Int, selectionEnd: Int): compiler.Tree = {
      val start = selectionStart + preambleLength
      val end   = selectionEnd + preambleLength
      val pos   = new RangePosition(unit.source, start, start, end)
      compiler.typedTreeAt(pos)
    }
  }

  object PresentationCompileResult {
    def apply(compiler0: interactive.Global)(unit0: compiler0.RichCompilationUnit, preambleLength0: Int) = new PresentationCompileResult {

      override val compiler = compiler0

      override def unit = unit0.asInstanceOf[compiler.RichCompilationUnit]

      override def preambleLength = preambleLength0
    }
  }
}
