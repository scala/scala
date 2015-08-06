/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.{interactive, Settings}
import scala.tools.nsc.io._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.util.ClassPath.DefaultJavaContext
import scala.tools.nsc.util.{DirectoryClassPath, MergedClassPath}

trait PresentationCompilation {
  self: IMain =>

  private[scala] def presentationCompile(line: String): Either[IR.Result, PresentationCompileResult] = {
    if (global == null) Left(IR.Error)
    else {
      val compiler = newPresentationCompiler()
      val trees = compiler.newUnitParser(line).parseStats()
      val importer = global.mkImporter(compiler)
      val request = new Request(line, trees map (t => importer.importTree(t)), generousImports = true)
      val wrappedCode: String = request.ObjectSourceCode(request.handlers)
      import compiler._
      val unit = new RichCompilationUnit(newCompilationUnit(wrappedCode).source)
      unitOfFile(unit.source.file) = unit
      typeCheck(unit)
      val result = PresentationCompileResult(request, compiler)(unit, request.ObjectSourceCode.preambleLength)
      Right(result)
    }
  }

  private def newPresentationCompiler(): interactive.Global = {
    val storeReporter: StoreReporter = new StoreReporter
    val replOutClasspath: DirectoryClassPath = new DirectoryClassPath(replOutput.dir, DefaultJavaContext)
    val claspath = new MergedClassPath[AbstractFile](replOutClasspath :: global.platform.classPath :: Nil, DefaultJavaContext)
    def copySettings: Settings = {
      val s = new Settings(_ => () /* ignores "bad option -nc" errors, etc */)
      s.processArguments(global.settings.recreateArgs, processAll = false)
      s
    }
    val interactiveGlobal = new interactive.Global(copySettings, storeReporter) { self =>
      override def assertCorrectThread(): Unit = ()
      override lazy val platform: ThisPlatform = new JavaPlatform {
        val global: self.type = self

        override def classPath: PlatformClassPath = claspath
      }
    }
    import interactiveGlobal._
    val run = new TyperRun()
    interactiveGlobal
  }

  abstract class PresentationCompileResult {
    def request: Request
    val compiler: scala.tools.nsc.interactive.Global
    def unit: compiler.RichCompilationUnit
    def preambleLength: Int
    def cleanup(): Unit = {
      compiler.askShutdown()
    }
    sealed abstract class CompletionResult {
      def results: List[compiler.Member]
    }
    object CompletionResult {
      final case class ScopeMembers(newCursor: Int, results: List[compiler.ScopeMember]) extends CompletionResult
      final case class TypeMembers(newCursor: Int, tree: compiler.Select, results: List[compiler.TypeMember]) extends CompletionResult
      case object NoResults extends CompletionResult {
        override def results: List[compiler.Member] = Nil
      }
    }
    def completionsOf(buf: String, cursor: Int): CompletionResult = {
      val offset = preambleLength
      val pos1 = unit.source.position(offset + cursor)
      import compiler._
      val focus1: Tree = typedTreeAt(pos1)
      focus1 match {
        case sel @ Select(qual, name) =>
          val prefix = if (name == nme.ERROR) "" else name.encoded
          val allTypeMembers = typeMembers(qual.pos).toList.flatten
          val completions = allTypeMembers.filter(_.sym.name.startsWith(prefix))
          def fallback = qual.pos.end + 2
          val nameStart: Int = (qual.pos.end + 1 until focus1.pos.end).find(p =>
            unit.source.identifier(unit.source.position(p)).exists(_.length > 0)
          ).getOrElse(fallback)
          val position: Int = cursor + nameStart - pos1.start
          CompletionResult.TypeMembers(position, sel, completions)
        case Ident(name) =>
          val allMembers = scopeMembers(pos1)
          val completions = allMembers.filter(_.sym.name.startsWith(name.encoded))
          val position: Int = cursor + focus1.pos.start - pos1.start
          CompletionResult.ScopeMembers(position, completions)
        case _ =>
          CompletionResult.NoResults
      }
    }
  }

  object PresentationCompileResult {
    def apply(request0: Request, compiler0: interactive.Global)(unit0: compiler0.RichCompilationUnit, preambleLength0: Int) = new PresentationCompileResult {

      override def request: Request = request0

      override val compiler = compiler0

      override def unit = unit0.asInstanceOf[compiler.RichCompilationUnit]

      override def preambleLength = preambleLength0
    }
  }
}
