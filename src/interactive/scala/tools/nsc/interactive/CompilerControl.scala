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
package interactive

import scala.util.control.ControlThrowable
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.FailedInterrupt
import scala.tools.nsc.util.EmptyAction
import scala.tools.nsc.util.WorkScheduler
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.util.InterruptReq

/** Interface of interactive compiler to a client such as an IDE
 *  The model the presentation compiler consists of the following parts:
 *
 *  unitOfFile: The map from sourcefiles to loaded units. A sourcefile/unit is loaded if it occurs in that map.
 *
 *  manipulated by: removeUnitOf, reloadSources.
 *
 *  A call to reloadSources will add the given sources to the loaded units, and
 *  start a new background compiler pass to compile all loaded units (with the indicated sources first).
 *  Each background compiler pass has its own typer run.
 *  The background compiler thread can be interrupted each time an AST node is
 *  completely typechecked in the following ways:

 *  1. by a new call to reloadSources. This starts a new background compiler pass with a new typer run.
 *  2. by a call to askTypeTree. This starts a new typer run if the forceReload parameter = true
 *  3. by a call to askTypeAt, askTypeCompletion, askScopeCompletion, askToDoFirst, askLinkPos, askLastType.
 *  4. by raising an exception in the scheduler.
 *  5. by passing a high-priority action wrapped in ask { ... }.
 *
 *  Actions under 1-3 can themselves be interrupted if they involve typechecking
 *  AST nodes. High-priority actions under 5 cannot; they always run to completion.
 *  So these high-priority actions should to be short.
 *
 *  Normally, an interrupted action continues after the interrupting action is finished.
 *  However, if the interrupting action created a new typer run, the interrupted
 *  action is aborted. If there's an outstanding response, it will be set to
 *  a Right value with a FreshRunReq exception.
 */
trait CompilerControl { self: Global =>

  type Response[T] = scala.tools.nsc.interactive.Response[T]

  /** The scheduler by which client and compiler communicate
   *  Must be initialized before starting compilerRunner
   */
  @volatile protected[interactive] var scheduler = new WorkScheduler

  /** Return the compilation unit attached to a source file, or None
   *  if source is not loaded.
   */
  def getUnitOf(s: SourceFile): Option[RichCompilationUnit] = getUnit(s)

  /** Run operation `op` on a compilation unit associated with given `source`.
   *  If source has a loaded compilation unit, this one is passed to `op`.
   *  Otherwise a new compilation unit is created, but not added to the set of loaded units.
   */
  def onUnitOf[T](source: SourceFile)(op: RichCompilationUnit => T): T =
    op(unitOfFile.getOrElse(source.file, new RichCompilationUnit(source)))

  /** Removes the CompilationUnit corresponding to the given SourceFile
   *  from consideration for recompilation.
   */
  def removeUnitOf(s: SourceFile): Option[RichCompilationUnit] = { toBeRemoved.synchronized { toBeRemoved += s.file }; unitOfFile get s.file }

  /** Returns the top level classes and objects that were deleted
   * in the editor since last time recentlyDeleted() was called.
   */
  def recentlyDeleted(): List[Symbol] = deletedTopLevelSyms.synchronized {
    val result = deletedTopLevelSyms
    deletedTopLevelSyms.clear()
    result.toList
  }

  /** Locate smallest tree that encloses position
   *  @note Pre-condition: Position must be loaded
   */
  def locateTree(pos: Position): Tree = onUnitOf(pos.source) { unit => new Locator(pos) locateIn unit.body }

  /** Locates smallest context that encloses position as an optional value.
   */
  def locateContext(pos: Position): Option[Context] =
    for (unit <- getUnit(pos.source); cx <- locateContext(unit.contexts, pos)) yield cx

  /** Returns the smallest context that contains given `pos`, throws FatalError if none exists.
   */
  def doLocateContext(pos: Position): Context = locateContext(pos) getOrElse {
    throw new FatalError("no context found for "+pos)
  }

  private def postWorkItem(item: WorkItem) =
    if (item.onCompilerThread) item() else scheduler.postWorkItem(item)

  /** Makes sure a set of compilation units is loaded and parsed.
   *  Returns () to syncvar `response` on completion.
   *  Afterwards a new background compiler run is started with
   *  the given sources at the head of the list of to-be-compiled sources.
   */
  def askReload(sources: List[SourceFile], response: Response[Unit]) = {
    val superseded = scheduler.dequeueAll {
      case ri: ReloadItem if ri.sources == sources => Some(ri)
      case _ => None
    }
    superseded.foreach(_.response.set(()))
    postWorkItem(new ReloadItem(sources, response))
  }

  /** Removes source files and toplevel symbols, and issues a new typer run.
   *  Returns () to syncvar `response` on completion.
   */
  def askFilesDeleted(sources: List[SourceFile], response: Response[Unit]) = {
    postWorkItem(new FilesDeletedItem(sources, response))
  }

  /** Sets sync var `response` to the smallest fully attributed tree that encloses position `pos`.
   *  Note: Unlike for most other ask... operations, the source file belonging to `pos` needs not be loaded.
   */
  def askTypeAt(pos: Position, response: Response[Tree]) =
    postWorkItem(new AskTypeAtItem(pos, response))

  /** Sets sync var `response` to the fully attributed & typechecked tree contained in `source`.
   *  @note Pre-condition: `source` needs to be loaded.
   *  @note Deprecated because of race conditions in the typechecker when the background compiler
   *        is interrupted while typing the same `source`.
   *  @see  scala/bug#6578
   */
  @deprecated("Use `askLoadedTyped` instead to avoid race conditions in the typechecker", "2.10.1")
  def askType(source: SourceFile, forceReload: Boolean, response: Response[Tree]) =
    postWorkItem(new AskTypeItem(source, forceReload, response))

  /** Sets sync var `response` to the position of the definition of the given link in
   *  the given sourcefile.
   *
   *  @param   sym      The symbol referenced by the link (might come from a classfile)
   *  @param   source   The source file that's supposed to contain the definition
   *  @param   response A response that will be set to the following:
   *                    If `source` contains a definition that is referenced by the given link
   *                    the position of that definition, otherwise NoPosition.
   *  Note: This operation does not automatically load `source`. If `source`
   *  is unloaded, it stays that way.
   */
  def askLinkPos(sym: Symbol, source: SourceFile, response: Response[Position]) =
    postWorkItem(new AskLinkPosItem(sym, source, response))

  /** Sets sync var `response` to doc comment information for a given symbol.
   *
   *  @param   sym        The symbol whose doc comment should be retrieved (might come from a classfile)
   *  @param   source     The source file that's supposed to contain the definition
   *  @param   site       The symbol where 'sym' is observed
   *  @param   fragments  All symbols that can contribute to the generated documentation
   *                      together with their source files.
   *  @param   response   A response that will be set to the following:
   *                      If `source` contains a definition of a given symbol that has a doc comment,
   *                      the (expanded, raw, position) triplet for a comment, otherwise ("", "", NoPosition).
   *  Note: This operation does not automatically load sources that are not yet loaded.
   */
  def askDocComment(sym: Symbol, source: SourceFile, site: Symbol, fragments: List[(Symbol,SourceFile)], response: Response[(String, String, Position)]): Unit =
    postWorkItem(new AskDocCommentItem(sym, source, site, fragments, response))

  @deprecated("Use method that accepts fragments", "2.10.2")
  def askDocComment(sym: Symbol, site: Symbol, source: SourceFile, response: Response[(String, String, Position)]): Unit =
    askDocComment(sym, source, site, (sym,source)::Nil, response)

  /** Sets sync var `response` to list of members that are visible
   *  as members of the tree enclosing `pos`, possibly reachable by an implicit.
   *  @note Pre-condition: source is loaded
   */
  def askTypeCompletion(pos: Position, response: Response[List[Member]]) =
    postWorkItem(new AskTypeCompletionItem(pos, response))

  /** Sets sync var `response` to list of members that are visible
   *  as members of the scope enclosing `pos`.
   *  @note  Pre-condition: source is loaded
   */
  def askScopeCompletion(pos: Position, response: Response[List[Member]]) =
    postWorkItem(new AskScopeCompletionItem(pos, response))

  /** Asks to do unit corresponding to given source file on present and subsequent type checking passes.
   *  If the file is in the 'crashedFiles' ignore list it is removed and typechecked normally.
   */
  def askToDoFirst(source: SourceFile) =
    postWorkItem(new AskToDoFirstItem(source))

  /** If source is not yet loaded, loads it, and starts a new run, otherwise
   * continues with current pass.
   * Waits until source is fully type checked and returns body in response.
   * @param source     The source file that needs to be fully typed.
   * @param keepLoaded Whether to keep that file in the PC if it was not loaded before. If
                       the file is already loaded, this flag is ignored.
   * @param response   The response, which is set to the fully attributed tree of `source`.
   *                   If the unit corresponding to `source` has been removed in the meantime
   *                   the a NoSuchUnitError is raised in the response.
   */
  def askLoadedTyped(source:SourceFile, keepLoaded: Boolean, response: Response[Tree]): Unit =
    postWorkItem(new AskLoadedTypedItem(source, keepLoaded, response))

  final def askLoadedTyped(source: SourceFile, response: Response[Tree]): Unit =
    askLoadedTyped(source, keepLoaded = false, response)

  /** If source if not yet loaded, get an outline view with askParseEntered.
   *  If source is loaded, wait for it to be typechecked.
   *  In both cases, set response to parsed (and possibly typechecked) tree.
   *  @param keepSrcLoaded If set to `true`, source file will be kept as a loaded unit afterwards.
   */
  def askStructure(keepSrcLoaded: Boolean)(source: SourceFile, response: Response[Tree]) = {
    getUnit(source) match {
      case Some(_) => askLoadedTyped(source, keepSrcLoaded, response)
      case None => askParsedEntered(source, keepSrcLoaded, response)
    }
  }

  /** Set sync var `response` to the parse tree of `source` with all top-level symbols entered.
   *  @param source       The source file to be analyzed
   *  @param keepLoaded   If set to `true`, source file will be kept as a loaded unit afterwards.
   *                      If keepLoaded is `false` the operation is run at low priority, only after
   *                      everything is brought up to date in a regular type checker run.
   *  @param response     The response.
   */
  def askParsedEntered(source: SourceFile, keepLoaded: Boolean, response: Response[Tree]) =
    postWorkItem(new AskParsedEnteredItem(source, keepLoaded, response))


  /** Cancels current compiler run and start a fresh one where everything will be re-typechecked
   *  (but not re-loaded).
   */
  def askReset() = scheduler raise (new FreshRunReq)

  /** Tells the compile server to shutdown, and not to restart again */
  def askShutdown() = scheduler raise ShutdownReq

  /** Returns parse tree for source `source`. No symbols are entered. Syntax errors are reported.
   *
   *  This method is thread-safe and as such can safely run outside of the presentation
   *  compiler thread.
   */
  def parseTree(source: SourceFile): Tree = {
    newUnitParser(new CompilationUnit(source)).parse()
  }

  /** Asks for a computation to be done quickly on the presentation compiler thread */
  def ask[A](op: () => A): A = if (self.onCompilerThread) op() else scheduler doQuickly op

  /** Asks for a computation to be done on presentation compiler thread, returning
   *  a response with the result or an exception
   */
  def askForResponse[A](op: () => A): Response[A] = {
    val r = new Response[A]
    if (self.onCompilerThread) {
      try   { r set op() }
      catch { case exc: Throwable => r raise exc }
      r
    } else {
      val ir = scheduler askDoQuickly op
      ir onComplete {
        case Left(result) => r set result
        case Right(exc)   => r raise exc
      }
      r
    }
  }

  def onCompilerThread = Thread.currentThread == compileRunner

  /** Info given for every member found by completion
   */
  abstract class Member {
    def prefix: Type
    val sym: Symbol
    val tpe: Type
    val accessible: Boolean
    val aliasInfo: Option[ScopeMember]
    def implicitlyAdded = false
    def symNameDropLocal: Name = if (sym.name.isTermName) sym.name.dropLocal else sym.name

    private def accessible_s = if (accessible) "" else "[inaccessible] "
    def forceInfoString = {
      definitions.fullyInitializeSymbol(sym)
      definitions.fullyInitializeType(tpe)
      infoString
    }
    def infoString = s"$accessible_s${sym.defStringSeenAs(tpe)}"
  }

  // Note: a `TypeMember` is a member *of* a type, not a member that *is* a type
  case class TypeMember(
    sym: Symbol,
    tpe: Type,
    accessible: Boolean,
    inherited: Boolean,
    viaView: Symbol,
    aliasInfo: Option[ScopeMember] = None) extends Member {
    // should be a case class parameter, but added as a var instead to preserve compatibility with the IDE
    var prefix: Type = NoType
    override def implicitlyAdded = viaView != NoSymbol
  }

  case class ScopeMember(
    sym: Symbol,
    tpe: Type,
    accessible: Boolean,
    viaImport: Tree,
    aliasInfo: Option[ScopeMember] = None) extends Member {
    // should be a case class parameter, but added as a var instead to preserve compatibility with the IDE
    var prefix: Type = NoType
  }

  // items that get sent to scheduler

  abstract class WorkItem extends (() => Unit) {
    val onCompilerThread = self.onCompilerThread

    /** Raise a MissingResponse, if the work item carries a response. */
    def raiseMissing(): Unit
  }

  case class ReloadItem(sources: List[SourceFile], response: Response[Unit]) extends WorkItem {
    def apply() = reload(sources, response)
    override def toString = "reload "+sources

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class FilesDeletedItem(sources: List[SourceFile], response: Response[Unit]) extends WorkItem {
    def apply() = filesDeleted(sources, response)
    override def toString = "files deleted "+sources

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class AskTypeAtItem(pos: Position, response: Response[Tree]) extends WorkItem {
    def apply() = self.getTypedTreeAt(pos, response)
    override def toString = "typeat "+pos.source+" "+pos.show

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class AskTypeItem(source: SourceFile, forceReload: Boolean, response: Response[Tree]) extends WorkItem {
    def apply() = self.getTypedTree(source, forceReload, response)
    override def toString = "typecheck"

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class AskTypeCompletionItem(pos: Position, response: Response[List[Member]]) extends WorkItem {
    def apply() = self.getTypeCompletion(pos, response)
    override def toString = "type completion "+pos.source+" "+pos.show

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class AskScopeCompletionItem(pos: Position, response: Response[List[Member]]) extends WorkItem {
    def apply() = self.getScopeCompletion(pos, response)
    override def toString = "scope completion "+pos.source+" "+pos.show

    def raiseMissing() =
      response raise new MissingResponse
  }

  class AskToDoFirstItem(val source: SourceFile) extends WorkItem {
    def apply() = {
      moveToFront(List(source))
      enableIgnoredFile(source.file)
    }
    override def toString = "dofirst "+source

    def raiseMissing() = ()
  }

  case class AskLinkPosItem(sym: Symbol, source: SourceFile, response: Response[Position]) extends WorkItem {
    def apply() = self.getLinkPos(sym, source, response)
    override def toString = "linkpos "+sym+" in "+source

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class AskDocCommentItem(sym: Symbol, source: SourceFile, site: Symbol, fragments: List[(Symbol,SourceFile)], response: Response[(String, String, Position)]) extends WorkItem {
    def apply() = self.getDocComment(sym, source, site, fragments, response)
    override def toString = "doc comment "+sym+" in "+source+" with fragments:"+fragments.mkString("(", ",", ")")

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class AskLoadedTypedItem(source: SourceFile, keepLoaded: Boolean, response: Response[Tree]) extends WorkItem {
    def apply() = self.waitLoadedTyped(source, response, keepLoaded, this.onCompilerThread)
    override def toString = "wait loaded & typed "+source

    def raiseMissing() =
      response raise new MissingResponse
  }

  case class AskParsedEnteredItem(source: SourceFile, keepLoaded: Boolean, response: Response[Tree]) extends WorkItem {
    def apply() = self.getParsedEntered(source, keepLoaded, response, this.onCompilerThread)
    override def toString = "getParsedEntered "+source+", keepLoaded = "+keepLoaded

    def raiseMissing() =
      response raise new MissingResponse
  }

  /** A do-nothing work scheduler that responds immediately with MissingResponse.
   *
   *  Used during compiler shutdown.
   */
  class NoWorkScheduler extends WorkScheduler {

    override def postWorkItem(action: Action) = synchronized {
      action match {
        case w: WorkItem => w.raiseMissing()
        case e: EmptyAction => // do nothing
        case _ => println("don't know what to do with this " + action.getClass)
      }
    }

    override def doQuickly[A](op: () => A): A = {
      throw new FailedInterrupt(new Exception("Posted a work item to a compiler that's shutting down"))
    }

    override def askDoQuickly[A](op: () => A): InterruptReq { type R = A } = {
      val ir = new InterruptReq {
        type R = A
        val todo = () => throw new MissingResponse
      }
      ir.execute()
      ir
    }

  }

}

  // ---------------- Interpreted exceptions -------------------

/** Signals a request for a fresh background compiler run.
 *  Note: The object has to stay top-level so that the PresentationCompilerThread may access it.
 */
class FreshRunReq extends ControlThrowable

/** Signals a request for a shutdown of the presentation compiler.
 *  Note: The object has to stay top-level so that the PresentationCompilerThread may access it.
 */
object ShutdownReq extends ControlThrowable

class NoSuchUnitError(file: AbstractFile) extends Exception("no unit found for file "+file)

class MissingResponse extends Exception("response missing")
