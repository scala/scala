package scala.tools.nsc
package interactive

import scala.util.control.ControlThrowable
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, WorkScheduler}
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** Interface of interactive compiler to a client such as an IDE
 */
trait CompilerControl { self: Global =>

  abstract class WorkItem extends (() => Unit)

  case class ReloadItem(sources: List[SourceFile], response: Response[Unit]) extends WorkItem {
    def apply() = reload(sources, response)
    override def toString = "reload "+sources
  }

  class AskTypeAtItem(val pos: Position, response: Response[Tree]) extends WorkItem {
    def apply() = self.getTypedTreeAt(pos, response)
    override def toString = "typeat "+pos.source+" "+pos.show
  }

  class AskTypeItem(val source: SourceFile, val forceReload: Boolean, response: Response[Tree]) extends WorkItem {
    def apply() = self.getTypedTree(source, forceReload, response)
    override def toString = "typecheck"
  }

  class AskLastTypeItem(val source: SourceFile, response: Response[Tree]) extends WorkItem {
    def apply() = self.getLastTypedTree(source, response)
    override def toString = "reconcile"
  }

  class AskTypeCompletionItem(val pos: Position, response: Response[List[Member]]) extends WorkItem {
    def apply() = self.getTypeCompletion(pos, response)
    override def toString = "type completion "+pos.source+" "+pos.show
  }

  class AskScopeCompletionItem(val pos: Position, response: Response[List[Member]]) extends WorkItem {
    def apply() = self.getScopeCompletion(pos, response)
    override def toString = "scope completion "+pos.source+" "+pos.show
  }

  class AskToDoFirstItem(val source: SourceFile) extends WorkItem {
    def apply() = moveToFront(List(source))
    override def toString = "dofirst "+source
  }

  /** Info given for every member found by completion
   */
  abstract class Member {
    val sym: Symbol
    val tpe: Type
    val accessible: Boolean
  }

  case class TypeMember(
    sym: Symbol,
    tpe: Type,
    accessible: Boolean,
    inherited: Boolean,
    viaView: Symbol) extends Member

  case class ScopeMember(
    sym: Symbol,
    tpe: Type,
    accessible: Boolean,
    viaImport: Tree) extends Member

  type Response[T] = scala.tools.nsc.interactive.Response[T]

  /** The scheduler by which client and compiler communicate
   *  Must be initialized before starting compilerRunner
   */
  protected[interactive] val scheduler = new WorkScheduler

  /** The compilation unit corresponding to a source file
   *  if it does not yet exist creat a new one atomically
   */
  def unitOf(s: SourceFile): RichCompilationUnit = unitOfFile.synchronized {
    unitOfFile get s.file match {
      case Some(unit) =>
        unit
      case None =>
        val unit = new RichCompilationUnit(s)
        unitOfFile(s.file) = unit
        unit
    }
  }

  /** The compilation unit corresponding to a position */
  def unitOf(pos: Position): RichCompilationUnit = unitOf(pos.source)

  /** Remove the CompilationUnit corresponding to the given SourceFile
   *  from consideration for recompilation.
   */
  def removeUnitOf(s: SourceFile) = unitOfFile remove s.file

  /* returns the top level classes and objects that were deleted
   * in the editor since last time recentlyDeleted() was called.
   */
  def recentlyDeleted(): List[Symbol] = deletedTopLevelSyms.synchronized {
    val result = deletedTopLevelSyms
    deletedTopLevelSyms.clear()
    result.toList
  }

  /** Locate smallest tree that encloses position
   */
  def locateTree(pos: Position): Tree =
    new Locator(pos) locateIn unitOf(pos).body

  /** Locates smallest context that encloses position as an optional value.
   */
  def locateContext(pos: Position): Option[Context] =
    locateContext(unitOf(pos).contexts, pos)

  /** Returns the smallest context that contains given `pos`, throws FatalError if none exists.
   */
  def doLocateContext(pos: Position): Context = locateContext(pos) getOrElse {
    throw new FatalError("no context found for "+pos)
  }

  /** Make sure a set of compilation units is loaded and parsed.
   *  Return () to syncvar `response` on completion.
   */
  def askReload(sources: List[SourceFile], response: Response[Unit]) = {
    val superseeded = scheduler.dequeueAll {
      case ri: ReloadItem if ri.sources == sources => Some(ri)
      case _ => None
    }
    superseeded foreach (_.response.set())
    scheduler postWorkItem new ReloadItem(sources, response)
  }

  /** Set sync var `response` to the smallest fully attributed tree that encloses position `pos`.
   */
  def askTypeAt(pos: Position, response: Response[Tree]) =
    scheduler postWorkItem new AskTypeAtItem(pos, response)

  /** Set sync var `response` to the fully attributed & typechecked tree contained in `source`.
   */
  def askType(source: SourceFile, forceReload: Boolean, response: Response[Tree]) =
    scheduler postWorkItem new AskTypeItem(source, forceReload, response)

  /** Set sync var `response` to the last fully attributed & typechecked tree produced from `source`.
   *  If no such tree exists yet, do a normal askType(source, false, response)
   */
  def askLastType(source: SourceFile, response: Response[Tree]) =
    scheduler postWorkItem new AskLastTypeItem(source, response)

  /** Set sync var `response' to list of members that are visible
   *  as members of the tree enclosing `pos`, possibly reachable by an implicit.
   */
  def askTypeCompletion(pos: Position, response: Response[List[Member]]) =
    scheduler postWorkItem new AskTypeCompletionItem(pos, response)

  /** Set sync var `response' to list of members that are visible
   *  as members of the scope enclosing `pos`.
   */
  def askScopeCompletion(pos: Position, response: Response[List[Member]]) =
    scheduler postWorkItem new AskScopeCompletionItem(pos, response)

  /** Ask to do unit first on present and subsequent type checking passes */
  def askToDoFirst(f: SourceFile) =
    scheduler postWorkItem new AskToDoFirstItem(f)

  /** Cancel current compiler run and start a fresh one where everything will be re-typechecked
   *  (but not re-loaded).
   */
  def askReset() = scheduler raise FreshRunReq

  /** Tell the compile server to shutdown, and do not restart again */
  def askShutdown() = scheduler raise ShutdownReq

  /** Ask for a computation to be done quickly on the presentation compiler thread */
  def ask[A](op: () => A): A = scheduler doQuickly op
}

  // ---------------- Interpreted exceptions -------------------

/** It has to stay top-level so that the PresentationCompilerThread may access it. */
object FreshRunReq extends ControlThrowable

/** It has to stay top-level so that the PresentationCompilerThread may access it. */
object ShutdownReq extends ControlThrowable
