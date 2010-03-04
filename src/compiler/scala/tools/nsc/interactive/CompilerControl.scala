package scala.tools.nsc
package interactive

import scala.concurrent.SyncVar
import scala.util.control.ControlThrowable
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, WorkScheduler}
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** Interface of interactive compiler to a client such as an IDE
 */
trait CompilerControl { self: Global =>

  /** Response {
    override def toString = "TypeMember("+sym+","+tpe+","+accessible+","+inherited+","+viaView+")"
  }{
    override def toString = "TypeMember("+sym+","+tpe+","+accessible+","+inherited+","+viaView+")"
  }wrapper to client
   */
  type Response[T] = SyncVar[Either[T, Throwable]]

  abstract class WorkItem extends (() => Unit)

  /** Info given for every member found by completion
   */
  abstract class Member {
    val sym: Symbol
    val tpe: Type
    val accessible: Boolean
  }

  case class TypeMember(sym: Symbol, tpe: Type, accessible: Boolean, inherited: Boolean, viaView: Symbol) extends Member
  case class ScopeMember(sym: Symbol, tpe: Type, accessible: Boolean, viaImport: Tree) extends Member

  /** The scheduler by which client and compiler communicate
   *  Must be initialized before starting compilerRunner
   */
  protected val scheduler = new WorkScheduler

  /** The compilation unit corresponding to a source file
   */
  def unitOf(s: SourceFile): RichCompilationUnit = unitOfFile get s.file match {
    case Some(unit) =>
      unit
    case None =>
      val unit = new RichCompilationUnit(s)
      unitOfFile(s.file) = unit
      unit
  }

  /** The compilation unit corresponding to a position */
  def unitOf(pos: Position): RichCompilationUnit = unitOf(pos.source)

  /** Remove the CompilationUnit corresponding to the given SourceFile
   *  from consideration for recompilation.
   */
  def removeUnitOf(s: SourceFile) = unitOfFile remove s.file

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
   *  Return () to syncvar `result` on completion.
   */
  def askReload(sources: List[SourceFile], result: Response[Unit]) =
    scheduler postWorkItem new WorkItem {
      def apply() = reload(sources, result)
      override def toString = "reload "+sources
    }

  /** Set sync var `result` to a fully attributed tree located at position `pos`
   */
  def askTypeAt(pos: Position, result: Response[Tree]) =
    scheduler postWorkItem new WorkItem {
      def apply() = self.getTypedTreeAt(pos, result)
      override def toString = "typeat "+pos.source+" "+pos.show
    }

  def askType(source: SourceFile, forceReload: Boolean, result: Response[Tree]) =
    scheduler postWorkItem new WorkItem {
      def apply() = self.getTypedTree(source, forceReload, result)
      override def toString = "typecheck"
  }

  /** Set sync var `result' to list of members that are visible
   *  as members of the tree enclosing `pos`, possibly reachable by an implicit.
   *   - if `selection` is false, as identifiers in the scope enclosing `pos`
   */
  def askTypeCompletion(pos: Position, result: Response[List[Member]]) =
    scheduler postWorkItem new WorkItem {
      def apply() = self.getTypeCompletion(pos, result)
      override def toString = "type completion "+pos.source+" "+pos.show
    }

  /** Set sync var `result' to list of members that are visible
   *  as members of the scope enclosing `pos`.
   */
  def askScopeCompletion(pos: Position, result: Response[List[Member]]) =
    scheduler postWorkItem new WorkItem {
      def apply() = self.getScopeCompletion(pos, result)
      override def toString = "scope completion "+pos.source+" "+pos.show
    }

  /** Ask to do unit first on present and subsequent type checking passes */
  def askToDoFirst(f: SourceFile) = {
    scheduler postWorkItem new WorkItem {
      def apply() = moveToFront(List(f))
      override def toString = "dofirst "+f
    }
  }

  /** Cancel currently pending high-priority jobs */
  def askCancel() = scheduler raise CancelActionReq

  /** Cancel current compiler run and start a fresh one where everything will be re-typechecked
   *  (but not re-loaded).
   */
  def askReset() = scheduler raise FreshRunReq

  /** Tell the compile server to shutdown, and do not restart again */
  def askShutdown() = scheduler raise ShutdownReq

  // ---------------- Interpreted exeptions -------------------

  object CancelActionReq extends ControlThrowable
  object FreshRunReq extends ControlThrowable
  object ShutdownReq extends ControlThrowable
}
